{-|
Module      : Verismith.Fuzz
Description : Environment to run the simulator and synthesisers in a matrix.
Copyright   : (c) 2019, Yann Herklotz
License     : GPL-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

Environment to run the simulator and synthesisers in a matrix.
-}

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Verismith.Fuzz
    ( Fuzz (..)
    , FuzzOpts (..)
    , fuzz
    , fuzzInDir
    , fuzzMultiple
    , runFuzz
    , sampleSeed
    -- * Helpers
    , make
    , pop
    )
where

import           Control.DeepSeq             (force)
import           Control.Exception.Lifted    (finally)
import           Control.Lens                hiding ((<.>))
import           Control.Monad               (forM, replicateM)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.ByteString             (ByteString)
import           Data.List                   (nubBy, sort)
import           Data.Maybe                  (fromMaybe, isNothing)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time
import           Data.Tuple                  (swap)
import           Hedgehog                    (Gen)
import qualified Hedgehog.Internal.Gen       as Hog
import           Hedgehog.Internal.Seed      (Seed)
import qualified Hedgehog.Internal.Seed      as Hog
import qualified Hedgehog.Internal.Tree      as Hog
import           Prelude                     hiding (FilePath)
import           Shelly                      hiding (get)
import           Shelly.Lifted               (MonadSh, liftSh)
import           System.FilePath.Posix       (takeBaseName)
import           Verismith.Config
import           Verismith.Internal
import           Verismith.Reduce
import           Verismith.Report
import           Verismith.Result
import           Verismith.Tool.Icarus
import           Verismith.Tool.Internal
import           Verismith.Tool.Yosys
import           Verismith.Verilog.AST
import           Verismith.Verilog.CodeGen

data FuzzOpts = FuzzOpts { _fuzzOptsOutput      :: !(Maybe FilePath)
                         , _fuzzOptsForced      :: !Bool
                         , _fuzzOptsKeepAll     :: !Bool
                         , _fuzzOptsIterations  :: {-# UNPACK #-} !Int
                         , _fuzzOptsNoSim       :: !Bool
                         , _fuzzOptsNoEquiv     :: !Bool
                         , _fuzzOptsNoReduction :: !Bool
                         , _fuzzOptsConfig      :: {-# UNPACK #-} !Config
                         , _fuzzDataDir         :: {-# UNPACK #-} !FilePath
                         }
              deriving (Show, Eq)

$(makeLenses ''FuzzOpts)

defaultFuzzOpts :: FuzzOpts
defaultFuzzOpts = FuzzOpts { _fuzzOptsOutput      = Nothing
                           , _fuzzOptsForced      = False
                           , _fuzzOptsKeepAll     = False
                           , _fuzzOptsIterations  = 1
                           , _fuzzOptsNoSim       = False
                           , _fuzzOptsNoEquiv     = False
                           , _fuzzOptsNoReduction = False
                           , _fuzzOptsConfig      = defaultConfig
                           , _fuzzDataDir = fromText "."
                           }

data FuzzEnv = FuzzEnv { _getSynthesisers :: ![SynthTool]
                       , _getSimulators   :: ![SimTool]
                       , _yosysInstance   :: {-# UNPACK #-} !Yosys
                       , _fuzzEnvOpts     :: {-# UNPACK #-} !FuzzOpts
                       }
               deriving (Eq, Show)

$(makeLenses ''FuzzEnv)

data FuzzState = FuzzState { _fuzzSynthResults :: ![SynthResult]
                           , _fuzzSimResults   :: ![SimResult]
                           , _fuzzSynthStatus  :: ![SynthStatus]
                           }
                 deriving (Eq, Show)

$(makeLenses ''FuzzState)

type Frequency a = [(Seed, a)] -> [(Int, Gen (Seed, a))]

-- | The main type for the fuzzing, which contains an environment that can be
-- read from and the current state of all the results.
type Fuzz m = StateT FuzzState (ReaderT FuzzEnv m)

type MonadFuzz m = (MonadBaseControl IO m, MonadIO m, MonadSh m)

runFuzz :: MonadIO m => FuzzOpts -> Yosys -> Fuzz Sh a -> m a
runFuzz fo yos m = shelly $ runFuzz' fo yos m

runFuzz' :: Monad m => FuzzOpts -> Yosys -> Fuzz m b -> m b
runFuzz' fo yos m = runReaderT
    (evalStateT m (FuzzState [] [] []))
    (FuzzEnv { _getSynthesisers = ( force
                                   $ defaultIdentitySynth
                                   : (descriptionToSynth <$> conf ^. configSynthesisers)
                                 )
             , _getSimulators = (force $ descriptionToSim <$> conf ^. configSimulators)
             , _yosysInstance = yos
             , _fuzzEnvOpts   = fo
             }
    )
  where
    conf = _fuzzOptsConfig fo

askConfig :: Monad m => Fuzz m Config
askConfig = asks (_fuzzOptsConfig . _fuzzEnvOpts)

askOpts :: Monad m => Fuzz m FuzzOpts
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
filterSynth _                            = False

filterSim :: SimResult -> Bool
filterSim (SimResult _ _ (Pass _) _) = True
filterSim _                          = False

filterSynthStat :: SynthStatus -> Bool
filterSynthStat (SynthStatus _ (Pass _) _) = True
filterSynthStat _                          = False

passedFuzz :: FuzzReport -> Bool
passedFuzz (FuzzReport _ synth sim synthstat _ _ _ _) =
    (passedSynth + passedSim + passedSynthStat) == 0
  where
    passedSynth = length $ filter (not . filterSynth) synth
    passedSim = length $ filter (not . filterSim) sim
    passedSynthStat = length $ filter (not . filterSynthStat) synthstat

synthesisers :: Monad m => Fuzz m [SynthTool]
synthesisers = lift $ asks _getSynthesisers

--simulators :: (Monad m) => Fuzz () m [SimTool]
--simulators = lift $ asks getSimulators

--combinations :: [a] -> [b] -> [(a, b)]
--combinations l1 l2 = [ (x, y) | x <- l1, y <- l2 ]

logT :: MonadSh m => Text -> m ()
logT = liftSh . logger

timeit :: (MonadIO m, MonadSh m) => m a -> m (NominalDiffTime, a)
timeit a = do
    start  <- liftIO getCurrentTime
    result <- a
    end    <- liftIO getCurrentTime
    return (diffUTCTime end start, result)

synthesis :: (MonadBaseControl IO m, MonadSh m) => SourceInfo -> Fuzz m ()
synthesis src = do
    synth    <- synthesisers
    resTimes <- liftSh $ mapM exec synth
    fuzzSynthStatus
        .= applyList (uncurry . SynthStatus <$> synth) (fmap swap resTimes)
    liftSh $ inspect resTimes
  where
    exec a = toolRun ("synthesis with " <> toText a) . runResultT $ do
        liftSh . mkdir_p . fromText $ toText a
        pop (fromText $ toText a) $ runSynth a src

passedSynthesis :: MonadSh m => Fuzz m [SynthTool]
passedSynthesis = fmap toSynth . filter passed . _fuzzSynthStatus <$> get
  where
    passed (SynthStatus _ (Pass _) _) = True
    passed _                          = False
    toSynth (SynthStatus s _ _) = s

failedSynthesis :: MonadSh m => Fuzz m [SynthTool]
failedSynthesis = fmap toSynth . filter failed . _fuzzSynthStatus <$> get
  where
    failed (SynthStatus _ (Fail SynthFail) _) = True
    failed _                                  = False
    toSynth (SynthStatus s _ _) = s

make :: MonadSh m => FilePath -> m ()
make f = liftSh $ mkdir_p f

pop :: (MonadBaseControl IO m, MonadSh m) => FilePath -> m a -> m a
pop f a = do
    dir <- liftSh pwd
    finally (liftSh (cd f) >> a) . liftSh $ cd dir

applyList :: [a -> b] -> [a] -> [b]
applyList a b = apply' <$> zip a b where apply' (a', b') = a' b'

applyLots :: (a -> b -> c -> d -> e) -> [(a, b)] -> [(c, d)] -> [e]
applyLots func a b = applyList (uncurry . uncurry func <$> a) b

toSynthResult
    :: [(SynthTool, SynthTool)]
    -> [(NominalDiffTime, Result Failed ())]
    -> [SynthResult]
toSynthResult a b = applyLots SynthResult a $ fmap swap b

toolRun :: (MonadIO m, MonadSh m) => Text -> m a -> m (NominalDiffTime, a)
toolRun t m = do
    logT $ "Running " <> t
    (diff, res) <- timeit m
    logT $ "Finished " <> t <> " (" <> showT diff <> ")"
    return (diff, res)

equivalence :: (MonadBaseControl IO m, MonadSh m) => SourceInfo -> Fuzz m ()
equivalence src = do
    datadir <- fmap _fuzzDataDir askOpts
    synth <- passedSynthesis
--    let synthComb =
--            nubBy tupEq . filter (uncurry (/=)) $ combinations synth synth
    let synthComb =
            nubBy tupEq
                .   filter (uncurry (/=))
                $   (,) defaultIdentitySynth
                <$> synth
    resTimes <- liftSh $ mapM (uncurry (equiv datadir)) synthComb
    fuzzSynthResults .= toSynthResult synthComb resTimes
    liftSh $ inspect resTimes
  where
    tupEq (a, b) (a', b') = (a == a' && b == b') || (a == b' && b == a')
    equiv datadir a b =
        toolRun ("equivalence check for " <> toText a <> " and " <> toText b)
            . runResultT
            $ do make dir
                 pop dir $ do
                     liftSh $ do
                         cp ( fromText ".."
                              </> fromText (toText a)
                              </> synthOutput a
                            ) $ synthOutput a
                         cp ( fromText ".."
                              </> fromText (toText b)
                              </> synthOutput b
                            ) $ synthOutput b
                         writefile "rtl.v" $ genSource src
                     runEquiv datadir a b src
        where dir = fromText $ "equiv_" <> toText a <> "_" <> toText b

simulation :: (MonadIO m, MonadSh m) => SourceInfo -> Fuzz m ()
simulation src = do
    datadir <- fmap _fuzzDataDir askOpts
    synth    <- passedSynthesis
    vals     <- liftIO $ generateByteString Nothing 32 20
    ident    <- liftSh $ equiv datadir vals defaultIdentitySynth
    resTimes <- liftSh $ mapM (equiv datadir vals) synth
    liftSh
        .   inspect
        $   (\(_, r) -> bimap show (T.unpack . T.take 10 . showBS) r)
        <$> (ident : resTimes)
  where
    equiv datadir b a = toolRun ("simulation for " <> toText a) . runResultT $ do
        make dir
        pop dir $ do
            liftSh $ do
                cp (fromText ".." </> fromText (toText a) </> synthOutput a)
                    $ synthOutput a
                writefile "rtl.v" $ genSource src
            runSimIc datadir defaultIcarus a src b
        where dir = fromText $ "simulation_" <> toText a

-- | generates the specific number of bytestring with a random seed.
generateByteString' :: Int -> [Word8] -> (ByteString, [Word8])
generateByteString' size words = (B.pack $ take size words, drop size words)

generateByteString :: (Maybe Int) -> Int -> Int -> IO [ByteString]
generateByteString mseed size n = do
    gen <- case mseed of
        Some seed' -> return $ mkStdGen seed'
        Nothing    -> newStdGen
    randlist <- take (size * n) <$> randoms gen
    return . fmap B.pack $ chunksOf size randlist
  where
    chunksOf i xs | i <= 0 = error $ "chunksOf, number must be positive, got " ++ show i
    chunksOf i xs = repeatedly (splitAt i) xs
    repeatedly f [] = []
    repeatedly f as = b : repeatedly f as'
      where (b, as') = f as

failEquivWithIdentity :: (MonadSh m) => Fuzz m [SynthResult]
failEquivWithIdentity = filter withIdentity . _fuzzSynthResults <$> get
  where
    withIdentity (SynthResult (IdentitySynth _) _ (Fail EquivFail) _) = True
    withIdentity (SynthResult _ (IdentitySynth _) (Fail EquivFail) _) = True
    withIdentity _                                                    = False

passEquiv :: (MonadSh m) => Fuzz m [SynthResult]
passEquiv = filter withIdentity . _fuzzSynthResults <$> get
  where
    withIdentity (SynthResult _ _ (Pass _) _) = True
    withIdentity _                            = False

-- | Always reduces with respect to 'Identity'.
reduction :: (MonadSh m) => SourceInfo -> Fuzz m ()
reduction src = do
    datadir <- fmap _fuzzDataDir askOpts
    fails      <- failEquivWithIdentity
    synthFails <- failedSynthesis
    _          <- liftSh $ mapM (red datadir) fails
    _          <- liftSh $ mapM redSynth synthFails
    return ()
  where
    red datadir (SynthResult a b _ _) = do
        make dir
        pop dir $ do
            s <- reduceSynth datadir a b src
            writefile (fromText ".." </> dir <.> "v") $ genSource s
            return s
        where dir = fromText $ "reduce_" <> toText a <> "_" <> toText b
    redSynth a = do
        make dir
        pop dir $ do
            s <- reduceSynthesis a src
            writefile (fromText ".." </> dir <.> "v") $ genSource s
            return s
        where dir = fromText $ "reduce_" <> toText a

titleRun
    :: (MonadIO m, MonadSh m) => Text -> Fuzz m a -> Fuzz m (NominalDiffTime, a)
titleRun t f = do
    logT $ "### Starting " <> t <> " ###"
    (diff, res) <- timeit f
    logT $ "### Finished " <> t <> " (" <> showT diff <> ") ###"
    return (diff, res)

whenMaybe :: Applicative m => Bool -> m a -> m (Maybe a)
whenMaybe b x = if b then Just <$> x else pure Nothing

getTime :: (Num n) => Maybe (n, a) -> n
getTime = maybe 0 fst

generateSample
    :: (MonadIO m, MonadSh m)
    => Fuzz m (Seed, SourceInfo)
    -> Fuzz m (Seed, SourceInfo)
generateSample f = do
    logT "Sampling Verilog from generator"
    (t, v@(s, _)) <- timeit f
    logT $ "Chose " <> showT s
    logT $ "Generated Verilog (" <> showT t <> ")"
    return v

verilogSize :: (Source a) => a -> Int
verilogSize = length . lines . T.unpack . genSource

sampleVerilog
    :: (MonadSh m, MonadIO m, Source a, Ord a)
    => Frequency a
    -> Int
    -> Maybe Seed
    -> Gen a
    -> m (Seed, a)
sampleVerilog _    _ seed@(Just _) gen = sampleSeed seed gen
sampleVerilog freq n Nothing       gen = do
    res <- replicateM n $ sampleSeed Nothing gen
    let sizes   = fmap getSize res
    let samples = fmap snd . sort $ zip sizes res
    liftIO $ Hog.sample . Hog.frequency $ freq samples
    where getSize (_, s) = verilogSize s

hatFreqs :: Frequency a
hatFreqs l = zip hat (return <$> l)
  where
    h   = length l `div` 2
    hat = (+ h) . negate . abs . (h -) <$> [1 .. length l]

meanFreqs :: Source a => Frequency a
meanFreqs l = zip hat (return <$> l)
  where
    hat = calc <$> sizes
    calc i = if abs (mean - i) == min_ then 1 else 0
    mean  = sum sizes `div` length l
    min_  = minimum $ abs . (mean -) <$> sizes
    sizes = verilogSize . snd <$> l

medianFreqs :: Frequency a
medianFreqs l = zip hat (return <$> l)
  where
    h   = length l `div` 2
    hat = set_ <$> [1 .. length l]
    set_ n = if n == h then 1 else 0

fuzz :: MonadFuzz m => Gen SourceInfo -> Fuzz m FuzzReport
fuzz gen = do
    conf <- askConfig
    opts <- askOpts
    let seed = conf ^. configProperty . propSeed
    (seed', src) <- generateSample $ genMethod conf seed gen
    let size = length . lines . T.unpack $ genSource src
    liftSh
        .  writefile "config.toml"
        .  encodeConfig
        $  conf
        &  configProperty
        .  propSeed
        ?~ seed'
    (tsynth, _) <- titleRun "Synthesis" $ synthesis src
    (tequiv, _) <- if (_fuzzOptsNoEquiv opts)
        then return (0, mempty)
        else titleRun "Equivalence Check" $ equivalence src
    (_     , _) <- if (_fuzzOptsNoSim opts)
        then return (0, mempty)
        else titleRun "Simulation" $ simulation src
    fails       <- failEquivWithIdentity
    synthFails  <- failedSynthesis
    redResult   <-
        whenMaybe (not (null fails && null synthFails)
                   && not (_fuzzOptsNoReduction opts))
        . titleRun "Reduction"
        $ reduction src
    state_  <- get
    currdir <- liftSh pwd
    let vi = flip view state_
    let report = FuzzReport currdir
                            (vi fuzzSynthResults)
                            (vi fuzzSimResults)
                            (vi fuzzSynthStatus)
                            size
                            tsynth
                            tequiv
                            (getTime redResult)
    return report

fuzzInDir :: MonadFuzz m => Gen SourceInfo -> Fuzz m FuzzReport
fuzzInDir src = do
    fuzzOpts <- askOpts
    let fp = fromMaybe "fuzz" $ _fuzzOptsOutput fuzzOpts
    make fp
    res <- pop fp $ fuzz src
    liftSh $ do
        writefile (fp <.> "html") $ printResultReport (bname fp) res
        when (passedFuzz res && not (_fuzzOptsKeepAll fuzzOpts)) $ rm_rf fp
    relativeFuzzReport res
  where
    bname = T.pack . takeBaseName . T.unpack . toTextIgnore

fuzzMultiple
    :: MonadFuzz m
    => Gen SourceInfo
    -> Fuzz m [FuzzReport]
fuzzMultiple src = do
    fuzzOpts <- askOpts
    let seed = (_fuzzOptsConfig fuzzOpts) ^. configProperty . propSeed
    x <- case _fuzzOptsOutput fuzzOpts of
        Nothing -> do
            ct <- liftIO getZonedTime
            return
                .  fromText
                .  T.pack
                $  "output_"
                <> formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" ct
        Just f -> return f
    make x
    pop x $ do
        results <- if isNothing seed
            then forM [1 .. (_fuzzOptsIterations fuzzOpts)] fuzzDir'
            else (: []) <$> fuzzDir' (1 :: Int)
        liftSh . writefile (fromText "index" <.> "html") $ printSummary
            "Fuzz Summary"
            results
        return results
  where
    fuzzDir' :: (Show a, MonadFuzz m) => a -> Fuzz m FuzzReport
    fuzzDir' n' = local (fuzzEnvOpts . fuzzOptsOutput .~
                         (Just . fromText $ "fuzz_" <> showT n'))
                  $ fuzzInDir src

sampleSeed :: MonadSh m => Maybe Seed -> Gen a -> m (Seed, a)
sampleSeed s gen =
    liftSh
        $ let loop n = if n <= 0
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
          in  loop (100 :: Int)
