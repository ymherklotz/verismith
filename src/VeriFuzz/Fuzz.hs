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
    )
where

import           Control.Exception.Lifted         (finally)
import           Control.Lens
import           Control.Monad                    (forM, void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Control      (MonadBaseControl)
import           Control.Monad.Trans.Reader       hiding (local)
import           Control.Monad.Trans.State.Strict
import           Data.ByteString                  (ByteString)
import           Data.List                        (nubBy)
import           Data.Text                        (Text)
import           Data.Time
import           Hedgehog                         (Gen)
import qualified Hedgehog.Gen                     as Hog
import           Prelude                          hiding (FilePath)
import           Shelly                           hiding (get)
import           Shelly.Lifted                    (MonadSh, liftSh)
import           VeriFuzz.Config
import           VeriFuzz.Internal
import           VeriFuzz.Report
import           VeriFuzz.Result
import           VeriFuzz.Sim.Icarus
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Sim.Quartus
import           VeriFuzz.Sim.Vivado
import           VeriFuzz.Sim.XST
import           VeriFuzz.Sim.Yosys
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.CodeGen

data FuzzEnv = FuzzEnv { getSynthesisers :: ![SynthTool]
                       , getSimulators   :: ![SimTool]
                       , yosysInstance   :: {-# UNPACK #-} !Yosys
                       }
               deriving (Eq, Show)

-- | The main type for the fuzzing, which contains an environment that can be
-- read from and the current state of all the results.
type Fuzz m = StateT FuzzReport (ReaderT FuzzEnv m)

type MonadFuzz m = (MonadBaseControl IO m, MonadIO m, MonadSh m)

runFuzz :: MonadIO m => Config -> Yosys -> Fuzz Sh a -> m a
runFuzz conf yos m = shelly $ runFuzz' conf yos m

runFuzz' :: Monad m => Config -> Yosys -> Fuzz m b -> m b
runFuzz' conf yos m = runReaderT
    (evalStateT m (FuzzReport [] [] []))
    (FuzzEnv (descriptionToSynth <$> conf ^. configSynthesisers)
             (descriptionToSim <$> conf ^. configSimulators)
             yos
    )

synthesisers :: Monad m => Fuzz m [SynthTool]
synthesisers = lift $ asks getSynthesisers

--simulators :: (Monad m) => Fuzz () m [SimTool]
--simulators = lift $ asks getSimulators

combinations :: [a] -> [b] -> [(a, b)]
combinations l1 l2 = [ (x, y) | x <- l1, y <- l2 ]

logT :: MonadSh m => Text -> m ()
logT = liftSh . echoP

timeit :: (MonadIO m, MonadSh m) => m a -> m (NominalDiffTime, a)
timeit a = do
    start  <- liftIO getCurrentTime
    result <- a
    end    <- liftIO getCurrentTime
    return (diffUTCTime end start, result)

synthesis :: (MonadBaseControl IO m, MonadSh m) => SourceInfo -> Fuzz m ()
synthesis src = do
    synth   <- synthesisers
    results <- liftSh $ mapM exec synth
    synthStatus .= zipWith SynthStatus synth results
    liftSh $ inspect results
  where
    exec a = runResultT $ do
        liftSh . mkdir_p . fromText $ toText a
        pop (fromText $ toText a) $ runSynth a src

generateSample :: (MonadIO m, MonadSh m) => Gen SourceInfo -> Fuzz m SourceInfo
generateSample gen = do
    logT "Sampling Verilog from generator"
    (t, src) <- timeit $ Hog.sample gen
    logT $ "Generated Verilog (" <> showT t <> ")"
    return src

passedSynthesis :: MonadSh m => Fuzz m [SynthTool]
passedSynthesis = fmap toSynth . filter passed . _synthStatus <$> get
  where
    passed (SynthStatus _ (Pass _)) = True
    passed _                        = False
    toSynth (SynthStatus s _) = s

make :: MonadSh m => FilePath -> m ()
make f = liftSh $ do
    mkdir_p f
    cp_r "data" $ f </> fromText "data"

pop :: (MonadBaseControl IO m, MonadSh m) => FilePath -> m a -> m a
pop f a = do
    dir <- liftSh pwd
    finally (liftSh (cd f) >> a) . liftSh $ cd dir

equivalence :: (MonadBaseControl IO m, MonadSh m) => SourceInfo -> Fuzz m ()
equivalence src = do
    yos   <- lift $ asks yosysInstance
    synth <- passedSynthesis
    let synthComb =
            nubBy tupEq . filter (uncurry (/=)) $ combinations synth synth
    results <- liftSh $ mapM (uncurry $ equiv yos) synthComb
    liftSh $ inspect results
  where
    tupEq (a, b) (a', b') = (a == a' && b == b') || (a == b' && b == a')
    equiv yos a b = runResultT $ do
        make dir
        pop dir $ do
            liftSh $ do
                cp (fromText ".." </> fromText (toText a) </> synthOutput a)
                    $ synthOutput a
                cp (fromText ".." </> fromText (toText b) </> synthOutput b)
                    $ synthOutput b
                writefile "rtl.v" $ genSource src
            runEquiv yos a (Just b) src
        where dir = fromText $ "equiv_" <> toText a <> "_" <> toText b

fuzz :: MonadFuzz m => Gen SourceInfo -> Fuzz m FuzzReport
fuzz gen = do
    src <- generateSample gen
    synthesis src
    equivalence src
    return mempty

fuzzInDir :: MonadFuzz m => FilePath -> Gen SourceInfo -> Fuzz m FuzzReport
fuzzInDir fp src = do
    make fp
    pop fp $ fuzz src

fuzzMultiple
    :: MonadFuzz m => Int -> FilePath -> Gen SourceInfo -> Fuzz m FuzzReport
fuzzMultiple n fp src = do
    make fp
    void . pop fp $ forM [1 .. n] fuzzDir
    return mempty
    where fuzzDir n' = fuzzInDir (fromText $ "fuzz_" <> showT n') src
