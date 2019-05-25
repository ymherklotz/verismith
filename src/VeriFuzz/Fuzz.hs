{-|
Module      : VeriFuzz.Fuzz
Description : Environment to run the simulator and synthesisers in a matrix.
Copyright   : (c) 2019, Yann Herklotz
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Environment to run the simulator and synthesisers in a matrix.
-}

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module VeriFuzz.Fuzz
    ( Fuzz
    , fuzz
    , fuzzInDir
    , fuzzMultiple
    , runFuzz
    , sampleSeed
    )
where

import           Control.DeepSeq                  (force)
import           Control.Exception.Lifted         (finally)
import           Control.Lens                     hiding ((<.>))
import           Control.Monad                    (forM, replicateM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Control      (MonadBaseControl)
import           Control.Monad.Trans.Maybe        (runMaybeT)
import           Control.Monad.Trans.Reader       hiding (local)
import           Control.Monad.Trans.State.Strict
import           Data.List                        (nubBy, sort)
import           Data.Maybe                       (isNothing)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Time
import           Data.Tuple                       (swap)
import           Hedgehog                         (Gen)
import qualified Hedgehog.Internal.Gen            as Hog
import           Hedgehog.Internal.Seed           (Seed)
import qualified Hedgehog.Internal.Seed           as Hog
import qualified Hedgehog.Internal.Tree           as Hog
import           Prelude                          hiding (FilePath)
import           Shelly                           hiding (get)
import           Shelly.Lifted                    (MonadSh, liftSh)
import           System.FilePath.Posix            (takeBaseName)
import           VeriFuzz.Config
import           VeriFuzz.Internal
import           VeriFuzz.Reduce
import           VeriFuzz.Report
import           VeriFuzz.Result
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Sim.Yosys
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.CodeGen

data FuzzEnv = FuzzEnv { getSynthesisers :: ![SynthTool]
                       , getSimulators   :: ![SimTool]
                       , yosysInstance   :: {-# UNPACK #-} !Yosys
                       }
               deriving (Eq, Show)

data FuzzState = FuzzState { _fuzzSynthResults :: ![SynthResult]
                           , _fuzzSimResults   :: ![SimResult]
                           , _fuzzSynthStatus  :: ![SynthStatus]
                           }
                 deriving (Eq, Show)

$(makeLenses ''FuzzState)

-- | The main type for the fuzzing, which contains an environment that can be
-- read from and the current state of all the results.
type Fuzz m = StateT FuzzState (ReaderT FuzzEnv m)

type MonadFuzz m = (MonadBaseControl IO m, MonadIO m, MonadSh m)

runFuzz :: MonadIO m => Config -> Yosys -> (Config -> Fuzz Sh a) -> m a
runFuzz conf yos m = shelly $ runFuzz' conf yos m

runFuzz' :: Monad m => Config -> Yosys -> (Config -> Fuzz m b) -> m b
runFuzz' conf yos m = runReaderT
    (evalStateT (m conf) (FuzzState [] [] []))
    (FuzzEnv
        ( force
        $ defaultIdentitySynth
        : (descriptionToSynth <$> conf ^. configSynthesisers)
        )
        (force $ descriptionToSim <$> conf ^. configSimulators)
        yos
    )

synthesisers :: Monad m => Fuzz m [SynthTool]
synthesisers = lift $ asks getSynthesisers

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
make f = liftSh $ do
    mkdir_p f
    cp_r "data" $ f </> fromText "data"

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
    synth <- passedSynthesis
--    let synthComb =
--            nubBy tupEq . filter (uncurry (/=)) $ combinations synth synth
    let synthComb =
            nubBy tupEq
                .   filter (uncurry (/=))
                $   (,) defaultIdentitySynth
                <$> synth
    resTimes <- liftSh $ mapM (uncurry equiv) synthComb
    fuzzSynthResults .= toSynthResult synthComb resTimes
    liftSh $ inspect resTimes
  where
    tupEq (a, b) (a', b') = (a == a' && b == b') || (a == b' && b == a')
    equiv a b =
        toolRun ("equivalence check for " <> toText a <> " and " <> toText b)
            . runResultT
            $ do
                  make dir
                  pop dir $ do
                      liftSh $ do
                          cp
                                  (   fromText ".."
                                  </> fromText (toText a)
                                  </> synthOutput a
                                  )
                              $ synthOutput a
                          cp
                                  (   fromText ".."
                                  </> fromText (toText b)
                                  </> synthOutput b
                                  )
                              $ synthOutput b
                          writefile "rtl.v" $ genSource src
                      runEquiv a b src
        where dir = fromText $ "equiv_" <> toText a <> "_" <> toText b

failEquivWithIdentity :: (MonadSh m) => Fuzz m [SynthResult]
failEquivWithIdentity = filter withIdentity . _fuzzSynthResults <$> get
  where
    withIdentity (SynthResult (IdentitySynth _) _ (Fail EquivFail) _) = True
    withIdentity (SynthResult _ (IdentitySynth _) (Fail EquivFail) _) = True
    withIdentity _                                                    = False

-- | Always reduces with respect to 'Identity'.
reduction :: (MonadSh m) => SourceInfo -> Fuzz m ()
reduction src = do
    fails <- failEquivWithIdentity
    synthFails <- failedSynthesis
    _     <- liftSh $ mapM red fails
    _     <- liftSh $ mapM redSynth synthFails
    return ()
  where
    red (SynthResult a b _ _) = do
        make dir
        pop dir $ do
            s <- reduceSynth a b src
            writefile (fromText ".." </> dir <.> "v") $ genSource s
            return s
        where dir = fromText $ "reduce_" <> toText a <> "_" <> toText b
    redSynth (SynthStatus a _ _) = do
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

sampleVerilogHat :: (MonadIO m, MonadSh m) => Int -> Maybe Seed -> Gen SourceInfo -> Fuzz m (Seed, SourceInfo)
sampleVerilogHat _ seed@(Just _) gen = sampleSeed seed gen
sampleVerilogHat n Nothing gen = do
    res <- replicateM n $ sampleSeed Nothing gen
    let sizes = fmap getSize res
    let samples = fmap snd . sort $ zip sizes res
    liftIO $ Hog.sample . Hog.frequency $ freqs samples
    where
        getSize (_, s) = verilogSize s
        freqs l = zip hat (return <$> l)
            where
                h = length l `div` 2
                hat = (+ h) . negate . abs .  (h - ) <$> [1..length l]

fuzz :: MonadFuzz m => Gen SourceInfo -> Config -> Fuzz m FuzzReport
fuzz gen conf = do
    (seed', src) <- generateSample genMethod
    let size = length . lines . T.unpack $ genSource src
    liftSh
        .  writefile "config.toml"
        .  encodeConfig
        $  conf
        &  configProperty
        .  propSeed
        ?~ seed'
    (tsynth, _) <- titleRun "Synthesis" $ synthesis src
    (tequiv, _) <- titleRun "Equivalence Check" $ equivalence src
    fails       <- failEquivWithIdentity
    redResult <- whenMaybe (not $ null fails) . titleRun "Reduction" $ reduction
        src
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
    liftSh . writefile "index.html" $ printResultReport (bname currdir) report
    return report
  where
    seed  = conf ^. configProperty . propSeed
    bname = T.pack . takeBaseName . T.unpack . toTextIgnore
    genMethod = case conf ^. configProperty . propSampleMethod of
            "hat" -> do
                logT "Using the hat function"
                sampleVerilogHat (conf ^. configProperty . propSampleSize) seed gen
            _ -> do
                logT "Using first seed"
                sampleSeed seed gen

relativeFuzzReport :: (MonadSh m) => FuzzReport -> m FuzzReport
relativeFuzzReport fr@(FuzzReport dir _ _ _ _ _ _ _) = liftSh $ do
    newPath <- relPath dir
    return $ (fuzzDir .~ newPath) fr

fuzzInDir
    :: MonadFuzz m => FilePath -> Gen SourceInfo -> Config -> Fuzz m FuzzReport
fuzzInDir fp src conf = do
    make fp
    res <- pop fp $ fuzz src conf
    relativeFuzzReport res

fuzzMultiple
    :: MonadFuzz m
    => Int
    -> Maybe FilePath
    -> Gen SourceInfo
    -> Config
    -> Fuzz m [FuzzReport]
fuzzMultiple n fp src conf = do
    x <- case fp of
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
            then forM [1 .. n] fuzzDir'
            else (: []) <$> fuzzDir' (1 :: Int)
        liftSh . writefile (fromText "index" <.> "html") $ printSummary
            "Fuzz Summary"
            results
        return results
  where
    fuzzDir' n' = fuzzInDir (fromText $ "fuzz_" <> showT n') src conf
    seed = conf ^. configProperty . propSeed

sampleSeed :: MonadSh m => Maybe Seed -> Gen a -> m (Seed, a)
sampleSeed s gen =
    liftSh
        $ let
              loop n = if n <= 0
                  then
                      error
                          "Hedgehog.Gen.sample: too many discards, could not generate a sample"
                  else do
                      seed <- maybe Hog.random return s
                      case
                              runIdentity
                              . runMaybeT
                              . Hog.runTree
                              $ Hog.runGenT 30 seed gen
                          of
                              Nothing -> loop (n - 1)
                              Just x  -> return (seed, Hog.nodeValue x)
          in  loop (100 :: Int)
