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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module VeriFuzz.Fuzz
    ( SynthTool(..)
    , SimTool(..)
    , FuzzResult(..)
    , Fuzz
    , fuzz
    , runFuzz
    , defaultIcarusSim
    , defaultVivadoSynth
    , defaultYosysSynth
    , defaultXSTSynth
    , defaultQuartusSynth
    )
where

import           Control.Exception.Lifted         (finally)
import           Control.Lens
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
import           VeriFuzz.Internal
import           VeriFuzz.Result
import           VeriFuzz.Sim.Icarus
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Sim.Quartus
import           VeriFuzz.Sim.Vivado
import           VeriFuzz.Sim.XST
import           VeriFuzz.Sim.Yosys
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.CodeGen

-- | Common type alias for synthesis results
type UResult = Result Failed ()

-- | Commont type alias for simulation results
type BResult = Result Failed ByteString

data SynthTool = XSTSynth {-# UNPACK #-} !XST
               | VivadoSynth {-# UNPACK #-} !Vivado
               | YosysSynth {-# UNPACK #-} !Yosys
               | QuartusSynth !Quartus
               deriving (Eq)

instance Show SynthTool where
    show (XSTSynth xst)         = show xst
    show (VivadoSynth vivado)   = show vivado
    show (YosysSynth yosys)     = show yosys
    show (QuartusSynth quartus) = show quartus

instance Tool SynthTool where
    toText (XSTSynth xst)         = toText xst
    toText (VivadoSynth vivado)   = toText vivado
    toText (YosysSynth yosys)     = toText yosys
    toText (QuartusSynth quartus) = toText quartus

instance Synthesiser SynthTool where
    runSynth (XSTSynth xst)         = runSynth xst
    runSynth (VivadoSynth vivado)   = runSynth vivado
    runSynth (YosysSynth yosys)     = runSynth yosys
    runSynth (QuartusSynth quartus) = runSynth quartus

    synthOutput (XSTSynth xst)         = synthOutput xst
    synthOutput (VivadoSynth vivado)   = synthOutput vivado
    synthOutput (YosysSynth yosys)     = synthOutput yosys
    synthOutput (QuartusSynth quartus) = synthOutput quartus

    setSynthOutput (YosysSynth yosys)     = YosysSynth . setSynthOutput yosys
    setSynthOutput (XSTSynth xst)         = XSTSynth . setSynthOutput xst
    setSynthOutput (VivadoSynth vivado)   = VivadoSynth . setSynthOutput vivado
    setSynthOutput (QuartusSynth quartus) = QuartusSynth . setSynthOutput quartus

defaultYosysSynth :: SynthTool
defaultYosysSynth = YosysSynth defaultYosys

defaultQuartusSynth :: SynthTool
defaultQuartusSynth = QuartusSynth defaultQuartus

defaultVivadoSynth :: SynthTool
defaultVivadoSynth = VivadoSynth defaultVivado

defaultXSTSynth :: SynthTool
defaultXSTSynth = XSTSynth defaultXST

newtype SimTool = IcarusSim Icarus
                deriving (Eq)

instance Tool SimTool where
    toText (IcarusSim icarus) = toText icarus

instance Simulator SimTool where
    runSim (IcarusSim icarus) = runSim icarus
    runSimWithFile (IcarusSim icarus) = runSimWithFile icarus

instance Show SimTool where
    show (IcarusSim icarus) = show icarus

defaultIcarusSim :: SimTool
defaultIcarusSim = IcarusSim defaultIcarus

data FuzzEnv = FuzzEnv { getSynthesisers :: ![SynthTool]
                       , getSimulators   :: ![SimTool]
                       , yosysInstance   :: {-# UNPACK #-} !Yosys
                       }
               deriving (Eq, Show)

-- | The results from running a tool through a simulator. It can either fail or
-- return a result, which is most likely a 'ByteString'.
data SimResult = SimResult !SynthTool !SimTool !(BResult)
                 deriving (Eq)

instance Show SimResult where
    show (SimResult synth sim r) = show synth <> ", " <> show sim <> ": " <> show r

-- | The results of comparing the synthesised outputs of two files using a
-- formal equivalence checker. This will either return a failure or an output
-- which is most likely '()'.
data SynthResult = SynthResult !SynthTool !SynthTool !(UResult)
                   deriving (Eq)

instance Show SynthResult where
    show (SynthResult synth synth2 r) = show synth <> ", " <> show synth2 <> ": " <> show r

-- | The status of the synthesis using a simulator. This will be checked before
-- attempting to run the equivalence checks on the simulator, as that would be
-- unnecessary otherwise.
data SynthStatus = SynthStatus !SynthTool !(UResult)
                 deriving (Eq)

instance Show SynthStatus where
    show (SynthStatus synth r) = "synthesis " <> show synth <> ": " <> show r

-- | The complete state that will be used during fuzzing, which contains the
-- results from all the operations.
data FuzzResult = FuzzResult { _synthResults :: ![SynthResult]
                             , _simResults   :: ![SimResult]
                             , _synthStatus  :: ![SynthStatus]
                             }
                  deriving (Eq, Show)

$(makeLenses ''FuzzResult)

instance Semigroup FuzzResult where
    FuzzResult a1 b1 c1 <> FuzzResult a2 b2 c2 = FuzzResult (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance Monoid FuzzResult where
    mempty = FuzzResult [] [] []

-- | The main type for the fuzzing, which contains an environment that can be
-- read from and the current state of all the results.
type Fuzz m = StateT FuzzResult (ReaderT FuzzEnv m)

runFuzz
  :: MonadIO m =>
     [SynthTool] -> [SimTool] -> Yosys -> Fuzz Sh a -> m a
runFuzz synth sim yos m = shelly $ runFuzz' synth sim yos m

runFuzz' :: Monad m => [SynthTool] -> [SimTool] -> Yosys -> Fuzz m b -> m b
runFuzz' synth sim yos m =
    runReaderT (evalStateT m (FuzzResult [] [] [])) (FuzzEnv synth sim yos)

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
    start <- liftIO getCurrentTime
    result <- a
    end <- liftIO getCurrentTime
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
    (t, src)   <- timeit $ Hog.sample gen
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
    liftSh $ cd f
    ret <- a
    liftSh $ cd dir
    return ret

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
                    cp (fromText ".." </> fromText (toText a) </> synthOutput a) $ synthOutput a
                    cp (fromText ".." </> fromText (toText b) </> synthOutput b) $ synthOutput b
                    writefile "rtl.v" $ genSource src
                runEquiv yos a (Just b) src
            where
                dir = fromText $ "equiv_" <> toText a <> "_" <> toText b

fuzz :: (MonadBaseControl IO m, MonadIO m, MonadSh m) => Gen SourceInfo -> Fuzz m FuzzResult
fuzz gen = do
    make "output"
    pop "output" $ do
        src <- generateSample gen
        synthesis src
        equivalence src
    return mempty
