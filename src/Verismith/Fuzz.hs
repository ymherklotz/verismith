{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Verismith.Fuzz
-- Description : Environment to run the simulator and synthesisers in a matrix.
-- Copyright   : (c) 2019, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Environment to run the simulator and synthesisers in a matrix.
module Verismith.Fuzz
  ( Fuzz (..),
    FuzzOpts (..),
    fuzz,
    fuzzInDir,
    fuzzMultiple,
    fuzzMultipleEMI,
    runFuzz,
    sampleSeed,

    -- * Helpers
    make,
    pop,
  )
where

import Control.DeepSeq (force)
import Control.Exception.Lifted (finally)
import Control.Lens hiding ((<.>))
import Control.Monad (forM, replicateM)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ByteString (ByteString)
import Data.List (nubBy, sort)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Tuple (swap)
import Hedgehog (Gen)
import qualified Hedgehog.Internal.Gen as Hog
import Hedgehog.Internal.Seed (Seed)
import qualified Hedgehog.Internal.Seed as Hog
import qualified Hedgehog.Internal.Tree as Hog
import Shelly hiding (get, sub)
import Shelly.Lifted (MonadSh, liftSh, sub)
import System.FilePath.Posix (takeBaseName)
import Verismith.Config
import Verismith.CounterEg (CounterEg (..))
import Verismith.EMI
import Verismith.Reduce
import Verismith.Report
import Verismith.Result
import Verismith.Tool.Icarus
import Verismith.Tool.Internal
import Verismith.Tool.Yosys
import Verismith.Utils
import Verismith.Verilog.AST
import Verismith.Verilog.CodeGen
import Prelude hiding (FilePath)

data FuzzOpts = FuzzOpts
  { _fuzzOptsOutput :: !(Maybe FilePath),
    _fuzzOptsForced :: !Bool,
    _fuzzOptsKeepAll :: !Bool,
    _fuzzOptsIterations :: {-# UNPACK #-} !Int,
    _fuzzOptsNoSim :: !Bool,
    _fuzzOptsNoEquiv :: !Bool,
    _fuzzOptsNoReduction :: !Bool,
    _fuzzOptsConfig :: {-# UNPACK #-} !Config,
    _fuzzDataDir :: !FilePath,
    _fuzzOptsCrossCheck :: !Bool,
    _fuzzOptsChecker :: !(Maybe Text)
  }
  deriving (Show, Eq)

$(makeLenses ''FuzzOpts)

defaultFuzzOpts :: FuzzOpts
defaultFuzzOpts =
  FuzzOpts
    { _fuzzOptsOutput = Nothing,
      _fuzzOptsForced = False,
      _fuzzOptsKeepAll = False,
      _fuzzOptsIterations = 1,
      _fuzzOptsNoSim = False,
      _fuzzOptsNoEquiv = False,
      _fuzzOptsNoReduction = False,
      _fuzzOptsConfig = defaultConfig,
      _fuzzDataDir = fromText ".",
      _fuzzOptsCrossCheck = False,
      _fuzzOptsChecker = Nothing
    }

data FuzzEnv = FuzzEnv
  { _getSynthesisers :: ![SynthTool],
    _getSimulators :: ![SimTool],
    _yosysInstance :: {-# UNPACK #-} !Yosys,
    _fuzzEnvOpts :: {-# UNPACK #-} !FuzzOpts
  }
  deriving (Eq, Show)

$(makeLenses ''FuzzEnv)

data FuzzState = FuzzState
  { _fuzzSynthResults :: ![SynthResult],
    _fuzzSimResults :: ![SimResult],
    _fuzzSynthStatus :: ![SynthStatus]
  }
  deriving (Eq, Show)

$(makeLenses ''FuzzState)

type Frequency a = [(Seed, a)] -> [(Int, Gen (Seed, a))]

-- | The main type for the fuzzing, which contains an environment that can be
-- read from and the current state of all the results.
type Fuzz m = StateT FuzzState (ReaderT FuzzEnv m)

type MonadFuzz m = (MonadBaseControl IO m, MonadIO m, MonadSh m)

runFuzz :: (MonadIO m) => FuzzOpts -> Yosys -> Fuzz Sh a -> m a
runFuzz fo yos m = shelly $ runFuzz' fo yos m

runFuzz' :: (Monad m) => FuzzOpts -> Yosys -> Fuzz m b -> m b
runFuzz' fo yos m =
  runReaderT
    (evalStateT m (FuzzState [] [] []))
    ( FuzzEnv
        { _getSynthesisers =
            ( force $
                defaultIdentitySynth :
                (descriptionToSynth <$> conf ^. configSynthesisers)
            ),
          _getSimulators = (force $ descriptionToSim <$> conf ^. configSimulators),
          _yosysInstance = yos,
          _fuzzEnvOpts = fo
        }
    )
  where
    conf = _fuzzOptsConfig fo

askConfig :: (Monad m) => Fuzz m Config
askConfig = asks (_fuzzOptsConfig . _fuzzEnvOpts)

askOpts :: (Monad m) => Fuzz m FuzzOpts
askOpts = asks _fuzzEnvOpts

genMethod conf seed gen =
  case T.toLower $ conf ^. configProperty . propSampleMethod of
    "hat" -> do
      logT "Using the hat function"
      sv hatFreqs
    "mean" -> do
      logT "Using the mean function"
      sv meanFreqs
    "median" -> do
      logT "Using the median function"
      sv medianFreqs
    _ -> do
      logT "Using first seed"
      sampleSeed seed gen
  where
    sv a = sampleVerilog a (conf ^. configProperty . propSampleSize) seed gen

relativeFuzzReport :: (MonadSh m) => FuzzReport -> m FuzzReport
relativeFuzzReport fr@(FuzzReport dir _ _ _ _ _ _ _) = liftSh $ do
  newPath <- relPath dir
  return $ (fuzzDir .~ newPath) fr

filterSynth :: SynthResult -> Bool
filterSynth (SynthResult _ _ (Pass _) _) = True
filterSynth _ = False

filterSim :: SimResult -> Bool
filterSim (SimResult _ _ _ (Pass _) _) = True
filterSim _ = False

filterSynthStat :: SynthStatus -> Bool
filterSynthStat (SynthStatus _ (Pass _) _) = True
filterSynthStat _ = False

passedFuzz :: FuzzReport -> Bool
passedFuzz (FuzzReport _ synth sim synthstat _ _ _ _) =
  (passedSynth + passedSim + passedSynthStat) == 0
  where
    passedSynth = length $ filter (not . filterSynth) synth
    passedSim = length $ filter (not . filterSim) sim
    passedSynthStat = length $ filter (not . filterSynthStat) synthstat

synthesisers :: (Monad m) => Fuzz m [SynthTool]
synthesisers = lift $ asks _getSynthesisers

-- simulators :: (Monad m) => Fuzz () m [SimTool]
-- simulators = lift $ asks getSimulators

combinations :: [a] -> [b] -> [(a, b)]
combinations l1 l2 = [(x, y) | x <- l1, y <- l2]

logT :: (MonadSh m) => Text -> m ()
logT = liftSh . logger

timeit :: (MonadIO m, MonadSh m) => m a -> m (NominalDiffTime, a)
timeit a = do
  start <- liftIO getCurrentTime
  result <- a
  end <- liftIO getCurrentTime
  return (diffUTCTime end start, result)

synthesis :: (MonadBaseControl IO m, MonadSh m, Show ann) => (SourceInfo ann) -> Fuzz m ()
synthesis src = do
  synth <- synthesisers
  resTimes <- liftSh $ mapM exec synth
  fuzzSynthStatus
    .= applyList (uncurry . SynthStatus <$> synth) (fmap swap resTimes)
  liftSh $ inspect resTimes
  where
    exec a = toolRun ("synthesis with " <> toText a) . runResultT $ do
      liftSh . mkdir_p . fromText $ toText a
      pop (fromText $ toText a) $ runSynth a src

passedSynthesis :: (MonadSh m) => Fuzz m [SynthTool]
passedSynthesis = fmap toSynth . filter passed . _fuzzSynthStatus <$> get
  where
    passed (SynthStatus _ (Pass _) _) = True
    passed _ = False
    toSynth (SynthStatus s _ _) = s

failedSynthesis :: (MonadSh m) => Fuzz m [SynthTool]
failedSynthesis = fmap toSynth . filter failed . _fuzzSynthStatus <$> get
  where
    failed (SynthStatus _ (Fail SynthFail) _) = True
    failed _ = False
    toSynth (SynthStatus s _ _) = s

make :: (MonadSh m) => FilePath -> m ()
make f = liftSh $ mkdir_p f

pop :: (MonadBaseControl IO m, MonadSh m) => FilePath -> m a -> m a
pop f a = do
  dir <- liftSh pwd
  finally (liftSh (cd f) >> a) . liftSh $ cd dir

applyList :: [a -> b] -> [a] -> [b]
applyList a b = apply' <$> zip a b where apply' (a', b') = a' b'

applyLots :: (a -> b -> c -> d -> e) -> [(a, b)] -> [(c, d)] -> [e]
applyLots func a b = applyList (uncurry . uncurry func <$> a) b

toSynthResult ::
  [(SynthTool, SynthTool)] ->
  [(NominalDiffTime, Result Failed ())] ->
  [SynthResult]
toSynthResult a b = applyLots SynthResult a $ fmap swap b

toSimResult ::
  SimTool ->
  [ByteString] ->
  [SynthTool] ->
  [(NominalDiffTime, Result Failed ByteString)] ->
  [SimResult]
toSimResult sima bs as b =
  applyList
    ( applyList
        (repeat uncurry)
        (applyList (applyList (SimResult <$> as) (repeat sima)) (repeat bs))
    )
    $ fmap swap b

toolRun :: (MonadIO m, MonadSh m, Show a) => Text -> m a -> m (NominalDiffTime, a)
toolRun t m = do
  logT $ "Running " <> t
  s <- timeit m
  logT $ "Finished " <> t <> " " <> showT s
  return s

equivalence :: (MonadBaseControl IO m, MonadSh m, Show ann) => (SourceInfo ann) -> Fuzz m ()
equivalence src = do
  doCrossCheck <- fmap _fuzzOptsCrossCheck askOpts
  datadir <- fmap _fuzzDataDir askOpts
  checker <- fmap _fuzzOptsChecker askOpts
  synth <- passedSynthesis
  conf <- fmap _fuzzOptsConfig askOpts
  let synthComb =
        if doCrossCheck
          then nubBy tupEq . filter (uncurry (/=)) $ combinations synth synth
          else nubBy tupEq . filter (uncurry (/=)) $ (,) defaultIdentitySynth <$> synth
  resTimes <- liftSh $ mapM (uncurry (equiv (conf ^. configProperty . propDefaultYosys) checker datadir)) synthComb
  fuzzSynthResults .= toSynthResult synthComb resTimes
  liftSh $ inspect resTimes
  where
    tupEq (a, b) (a', b') = (a == a' && b == b') || (a == b' && b == a')
    equiv yosysloc checker datadir a b =
      toolRun ("equivalence check for " <> toText a <> " and " <> toText b)
        . runResultT
        $ do
          make dir
          pop dir $ do
            liftSh $ do
              cp
                ( fromText ".."
                    </> fromText (toText a)
                    </> synthOutput a
                )
                $ synthOutput a
              cp
                ( fromText ".."
                    </> fromText (toText b)
                    </> synthOutput b
                )
                $ synthOutput b
              writefile "rtl.v" $ genSource src
            sub $ do
              maybe (return ()) (liftSh . prependToPath . fromText) yosysloc
              runEquiv checker datadir a b src
      where
        dir = fromText $ "equiv_" <> toText a <> "_" <> toText b

simulation :: (MonadIO m, MonadSh m, Show ann) => (SourceInfo ann) -> Fuzz m ()
simulation src = do
  datadir <- fmap _fuzzDataDir askOpts
  synth <- passedSynthesis
  counterEgs <- failEquivWithIdentityCE
  vals <- liftIO $ generateByteString Nothing 32 20
  ident <- liftSh $ sim datadir vals Nothing defaultIdentitySynth
  resTimes <- liftSh $ mapM (sim datadir vals (justPass $ snd ident)) synth
  resTimes2 <- liftSh $ mapM (simCounterEg datadir) counterEgs
  fuzzSimResults .= toSimResult defaultIcarusSim vals synth resTimes
  liftSh
    . inspect
    $ (\(_, r) -> bimap show (T.unpack . T.take 10 . showBS) r)
      <$> (ident : resTimes)
  where
    sim datadir b i a = toolRun ("simulation for " <> toText a) . runResultT $ do
      make dir
      pop dir $ do
        liftSh $ do
          cp (fromText ".." </> fromText (toText a) </> synthOutput a) $
            synthOutput a
          writefile "rtl.v" $ genSource src
        runSimIc datadir defaultIcarus a src b i
      where
        dir = fromText $ "simulation_" <> toText a
    simCounterEg datadir (a, Nothing) = toolRun ("counter-example simulation for " <> toText a) . return $ Fail EmptyFail
    simCounterEg datadir (a, Just b) = toolRun ("counter-example simulation for " <> toText a) . runResultT $ do
      make dir
      pop dir $ do
        liftSh $ do
          cp (fromText ".." </> fromText (toText a) </> synthOutput a) $ synthOutput a
          writefile "syn_identity.v" $ genSource src
        ident <- runSimIcEC datadir defaultIcarus defaultIdentitySynth src b Nothing
        runSimIcEC datadir defaultIcarus a src b (Just ident)
      where
        dir = fromText $ "countereg_sim_" <> toText a

simulationEMI :: (MonadIO m, MonadSh m, Show ann) => (SourceInfo (EMIInputs ann)) -> Fuzz m ()
simulationEMI src = do
  datadir <- fmap _fuzzDataDir askOpts
  synth <- passedSynthesis
  counterEgs <- failEquivWithIdentityCE
  vals <- liftIO $ generateByteString Nothing 32 100
  ident <- liftSh $ sim datadir vals Nothing defaultIdentitySynth
  resTimes <- liftSh $ mapM (sim datadir vals (justPass $ snd ident)) synth
  fuzzSimResults .= toSimResult defaultIcarusSim vals synth resTimes
  liftSh
    . inspect
    $ (\(_, r) -> bimap show (T.unpack . T.take 10 . showBS) r)
      <$> (ident : resTimes)
  where
    sim datadir b i a = toolRun ("simulation for " <> toText a) . runResultT $ do
      make dir
      pop dir $ do
        liftSh $ do
          cp (fromText ".." </> fromText (toText a) </> synthOutput a) $
            synthOutput a
          writefile "rtl.v" $ genSource src
        runSimIcEMI (getTopEMIIdent src) datadir defaultIcarus a (clearAnn src) b i
      where
        dir = fromText $ "emi_sim_" <> toText a

failEquivWithIdentity :: (MonadSh m) => Fuzz m [SynthResult]
failEquivWithIdentity = filter withIdentity . _fuzzSynthResults <$> get
  where
    withIdentity (SynthResult (IdentitySynth _) _ (Fail (EquivFail _)) _) = True
    withIdentity (SynthResult _ (IdentitySynth _) (Fail (EquivFail _)) _) = True
    withIdentity _ = False

failEquivWithIdentityCE :: (MonadSh m) => Fuzz m [(SynthTool, Maybe CounterEg)]
failEquivWithIdentityCE = catMaybes . fmap withIdentity . _fuzzSynthResults <$> get
  where
    withIdentity (SynthResult (IdentitySynth _) s (Fail (EquivFail c)) _) = Just (s, c)
    withIdentity (SynthResult s (IdentitySynth _) (Fail (EquivFail c)) _) = Just (s, c)
    withIdentity _ = Nothing

failedSimulations :: (MonadSh m) => Fuzz m [SimResult]
failedSimulations = filter failedSim . _fuzzSimResults <$> get
  where
    failedSim (SimResult _ _ _ (Fail (SimFail _)) _) = True
    failedSim _ = False

passEquiv :: (MonadSh m) => Fuzz m [SynthResult]
passEquiv = filter withIdentity . _fuzzSynthResults <$> get
  where
    withIdentity (SynthResult _ _ (Pass _) _) = True
    withIdentity _ = False

-- | Always reduces with respect to 'Identity'.
reduction :: (MonadSh m) => SourceInfo ann -> Fuzz m ()
reduction rsrc = do
  datadir <- fmap _fuzzDataDir askOpts
  checker <- fmap _fuzzOptsChecker askOpts
  fails <- failEquivWithIdentity
  synthFails <- failedSynthesis
  simFails <- failedSimulations
  _ <- liftSh $ mapM (red checker datadir) fails
  _ <- liftSh $ mapM redSynth synthFails
  _ <- liftSh $ mapM (redSim datadir) simFails
  return ()
  where
    red checker datadir (SynthResult a b _ _) = do
      r <- reduceSynth checker datadir a b src
      writefile (fromText $ "reduce_" <> toText a <> "_" <> toText b <> ".v") $ genSource r
    redSynth a = do
      r <- reduceSynthesis a src
      writefile (fromText $ "reduce_" <> toText a <> ".v") $ genSource r
    redSim datadir (SimResult t _ bs _ _) = do
      r <- reduceSimIc datadir bs t src
      writefile (fromText $ "reduce_sim_" <> toText t <> ".v") $ genSource r
    src = clearAnn rsrc

titleRun ::
  (MonadIO m, MonadSh m) => Text -> Fuzz m a -> Fuzz m (NominalDiffTime, a)
titleRun t f = do
  logT $ "### Starting " <> t <> " ###"
  (diff, res) <- timeit f
  logT $ "### Finished " <> t <> " (" <> showT diff <> ") ###"
  return (diff, res)

whenMaybe :: (Applicative m) => Bool -> m a -> m (Maybe a)
whenMaybe b x = if b then Just <$> x else pure Nothing

getTime :: (Num n) => Maybe (n, a) -> n
getTime = maybe 0 fst

generateSample ::
  (MonadIO m, MonadSh m, Show ann) =>
  Fuzz m (Seed, (SourceInfo ann)) ->
  Fuzz m (Seed, (SourceInfo ann))
generateSample f = do
  logT "Sampling Verilog from generator"
  (t, v@(s, _)) <- timeit f
  logT $ "Chose " <> showT s
  logT $ "Generated Verilog (" <> showT t <> ")"
  return v

verilogSize :: (Source a) => a -> Int
verilogSize = length . lines . T.unpack . genSource

sampleVerilog ::
  (MonadSh m, MonadIO m, Source a, Ord a) =>
  Frequency a ->
  Int ->
  Maybe Seed ->
  Gen a ->
  m (Seed, a)
sampleVerilog _ _ seed@(Just _) gen = sampleSeed seed gen
sampleVerilog freq n Nothing gen = do
  res <- replicateM n $ sampleSeed Nothing gen
  let sizes = fmap getSize res
  let samples = fmap snd . sort $ zip sizes res
  liftIO $ Hog.sample . Hog.frequency $ freq samples
  where
    getSize (_, s) = verilogSize s

hatFreqs :: Frequency a
hatFreqs l = zip hat (return <$> l)
  where
    h = length l `div` 2
    hat = (+ h) . negate . abs . (h -) <$> [1 .. length l]

meanFreqs :: (Source a) => Frequency a
meanFreqs l = zip hat (return <$> l)
  where
    hat = calc <$> sizes
    calc i = if abs (mean - i) == min_ then 1 else 0
    mean = sum sizes `div` length l
    min_ = minimum $ abs . (mean -) <$> sizes
    sizes = verilogSize . snd <$> l

medianFreqs :: Frequency a
medianFreqs l = zip hat (return <$> l)
  where
    h = length l `div` 2
    hat = set_ <$> [1 .. length l]
    set_ n = if n == h then 1 else 0

fuzz :: (MonadFuzz m, Ord ann, Show ann) => Gen (SourceInfo ann) -> Fuzz m FuzzReport
fuzz gen = do
  conf <- askConfig
  opts <- askOpts
  let seed = conf ^. configProperty . propSeed
  (seed', src) <- generateSample $ genMethod conf seed gen
  let size = length . lines . T.unpack $ genSource src
  liftSh
    . writefile "config.toml"
    . encodeConfig
    $ conf
      & configProperty
        . propSeed
        ?~ seed'
  (tsynth, _) <- titleRun "Synthesis" $ synthesis src
  (tequiv, _) <-
    if (_fuzzOptsNoEquiv opts)
      then return (0, mempty)
      else titleRun "Equivalence Check" $ equivalence src
  (_, _) <-
    if (_fuzzOptsNoSim opts)
      then return (0, mempty)
      else titleRun "Simulation" $ simulation src
  fails <- failEquivWithIdentity
  failedSim <- failedSimulations
  synthFails <- failedSynthesis
  redResult <-
    whenMaybe
      ( not (null failedSim && null fails && null synthFails)
          && not (_fuzzOptsNoReduction opts)
      )
      . titleRun "Reduction"
      $ reduction src
  state_ <- get
  currdir <- liftSh pwd
  let vi = flip view state_
  let report =
        FuzzReport
          currdir
          (vi fuzzSynthResults)
          (vi fuzzSimResults)
          (vi fuzzSynthStatus)
          size
          tsynth
          tequiv
          (getTime redResult)
  return report

fuzzInDirG ::
  (MonadFuzz m, Ord ann, Show ann) =>
  (Gen (SourceInfo ann) -> Fuzz m FuzzReport) ->
  Gen (SourceInfo ann) ->
  Fuzz m FuzzReport
fuzzInDirG f src = do
  fuzzOpts <- askOpts
  let fp = fromMaybe "fuzz" $ _fuzzOptsOutput fuzzOpts
  make fp
  res <- pop fp $ f src
  liftSh $ do
    writefile (fp <.> "html") $ printResultReport (bname fp) res
    when (passedFuzz res && not (_fuzzOptsKeepAll fuzzOpts)) $ rm_rf fp
  relativeFuzzReport res
  where
    bname = T.pack . takeBaseName . T.unpack . toTextIgnore

fuzzMultipleG ::
  (MonadFuzz m, Ord ann, Show ann) =>
  (Gen (SourceInfo ann) -> Fuzz m FuzzReport) ->
  Gen (SourceInfo ann) ->
  Fuzz m [FuzzReport]
fuzzMultipleG f src = do
  fuzzOpts <- askOpts
  let seed = (_fuzzOptsConfig fuzzOpts) ^. configProperty . propSeed
  x <- case _fuzzOptsOutput fuzzOpts of
    Nothing -> do
      ct <- liftIO getZonedTime
      return
        . fromText
        . T.pack
        $ "output_"
          <> formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" ct
    Just f -> return f
  make x
  pop x $ do
    results <-
      if isNothing seed
        then forM [1 .. (_fuzzOptsIterations fuzzOpts)] fuzzDir'
        else (: []) <$> fuzzDir' (1 :: Int)
    liftSh . writefile (fromText "index" <.> "html") $
      printSummary
        "Fuzz Summary"
        results
    return results
  where
    fuzzDir' n' =
      local
        ( fuzzEnvOpts . fuzzOptsOutput
            .~ (Just . fromText $ "fuzz_" <> showT n')
        )
        $ fuzzInDirG f src

sampleSeed :: (MonadSh m) => Maybe Seed -> Gen a -> m (Seed, a)
sampleSeed s gen =
  liftSh $
    let loop n =
          if n <= 0
            then
              error
                "Hedgehog.Gen.sample: too many discards, could not generate a sample"
            else do
              seed <- maybe Hog.random return s
              case Hog.evalGen 30 seed gen of
                Nothing ->
                  loop (n - 1)
                Just x ->
                  pure (seed, Hog.treeValue x)
     in loop (100 :: Int)

fuzzEMI :: (MonadFuzz m, Ord ann, Show ann) => Gen (SourceInfo (EMIInputs ann)) -> Fuzz m FuzzReport
fuzzEMI gen = do
  conf <- askConfig
  opts <- askOpts
  let seed = conf ^. configProperty . propSeed
  (seed', src) <- generateSample $ genMethod conf seed gen
  let size = length . lines . T.unpack $ genSource src
  liftSh
    . writefile "config.toml"
    . encodeConfig
    $ conf
      & configProperty
        . propSeed
        ?~ seed'
  (tsynth, _) <- titleRun "Synthesis" $ synthesis src
  (_, _) <-
    if (_fuzzOptsNoSim opts)
      then return (0, mempty)
      else titleRun "Simulation" $ simulationEMI src
  fails <- failEquivWithIdentity
  failedSim <- failedSimulations
  synthFails <- failedSynthesis
  state_ <- get
  currdir <- liftSh pwd
  let vi = flip view state_
  let report =
        FuzzReport
          currdir
          (vi fuzzSynthResults)
          (vi fuzzSimResults)
          (vi fuzzSynthStatus)
          size
          tsynth
          0
          0
  return report

fuzzInDir :: (MonadFuzz m, Ord ann, Show ann) => Gen (SourceInfo ann) -> Fuzz m FuzzReport
fuzzInDir = fuzzInDirG fuzz

fuzzInDirEMI :: (MonadFuzz m, Ord ann, Show ann) => Gen (SourceInfo (EMIInputs ann)) -> Fuzz m FuzzReport
fuzzInDirEMI = fuzzInDirG fuzzEMI

fuzzMultiple :: (MonadFuzz m, Ord ann, Show ann) => Gen (SourceInfo ann) -> Fuzz m [FuzzReport]
fuzzMultiple = fuzzMultipleG fuzz

fuzzMultipleEMI :: (MonadFuzz m, Ord ann, Show ann) => Gen (SourceInfo (EMIInputs ann)) -> Fuzz m [FuzzReport]
fuzzMultipleEMI = fuzzMultipleG fuzzEMI
