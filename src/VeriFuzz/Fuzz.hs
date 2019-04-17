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

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       hiding (local)
import           Control.Monad.Trans.State.Strict
import           Data.List                        (nubBy)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Time
import           Hedgehog                         (Gen)
import qualified Hedgehog.Gen                     as Hog
import           Prelude                          hiding (FilePath)
import           Shelly
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

data SimResult a = SimResult !SynthTool !SimTool !(Result Failed a)
                 deriving (Eq, Show)

data SynthResult a = SynthResult !SynthTool !SynthTool !(Result Failed a)
                   deriving (Eq, Show)

data FuzzResult a = FuzzResult { getSynthResults :: ![SynthResult a]
                               , getSimResults   :: ![SimResult a]
                               }
                  deriving (Eq, Show)

instance Semigroup (FuzzResult a) where
    FuzzResult a1 b1 <> FuzzResult a2 b2 = FuzzResult (a1 <> a2) (b1 <> b2)

instance Monoid (FuzzResult a) where
    mempty = FuzzResult [] []

type Fuzz a m = StateT (FuzzResult a) (ReaderT FuzzEnv m)

runFuzz :: (Monad m) => [SynthTool] -> [SimTool] -> Yosys -> Fuzz a m b -> m b
runFuzz synth sim yos m =
    runReaderT (evalStateT m (FuzzResult [] [])) (FuzzEnv synth sim yos)

synthesisers :: (Monad m) => Fuzz () m [SynthTool]
synthesisers = lift $ asks getSynthesisers

--simulators :: (Monad m) => Fuzz () m [SimTool]
--simulators = lift $ asks getSimulators

combinations :: [a] -> [b] -> [(a, b)]
combinations l1 l2 = [ (x, y) | x <- l1, y <- l2 ]

runInTmp :: (MonadSh m) => m a -> m a
runInTmp a = liftSh (withTmpDir $ (\dir -> cd dir)) >> a

mkAndRun :: (MonadSh m) => FilePath -> Sh a -> m a
mkAndRun a = liftSh . chdir_p a

liftWith'
    :: (MonadIO m)
    => (Sh (Result a b) -> Sh (Result a b))
    -> ResultT a Sh b
    -> m (Result a b)
liftWith' a = liftIO . shellyFailDir . a . runResultT

lift' :: (MonadIO m) => ResultT a Sh b -> m (Result a b)
lift' = liftWith' id

logT :: (MonadIO m) => Text -> m ()
logT = liftIO . shelly . echoP

synthesis :: (MonadIO m) => SourceInfo -> Fuzz () m (FuzzResult ())
synthesis src = do
    synth   <- synthesisers
    results <- mapM
        (\a -> liftWith' (mkAndRun . fromText $ showT a)
            $ runSynth a src (fromText $ "syn_" <> showT a <> ".v")
        )
        synth
    liftIO $ print results
    return mempty

timeit :: (MonadIO m) => m a -> m (NominalDiffTime, a)
timeit a = do
    start <- liftIO getCurrentTime
    result <- a
    end <- liftIO getCurrentTime
    return (diffUTCTime end start, result)

fuzz :: (MonadIO m) => Gen SourceInfo -> Fuzz () m (FuzzResult ())
fuzz gen = do
    synth <- synthesisers
    logT "Sampling Verilog from generator"
    (t, src)   <- timeit $ Hog.sample gen
    logT $ "Generated Verilog (" <> showT t <> ")"
    yos   <- lift $ asks yosysInstance
    _     <- synthesis src
    let synthComb =
            nubBy tupEq . filter (uncurry (/=)) $ combinations synth synth
    --let simComb = combinations synth sim
    --results <- mapM (uncurry $ equivalence yos src) synthComb
    --liftIO $ print results
    return mempty
  where
    tupEq (a, b) (a', b') = (a == a' && b == b') || (a == b' && b == a')
    equivalence yos src a b =
        liftIO . shellyFailDir . runInTmp . runResultT $ runEquiv yos
                                                                  a
                                                                  (Just b)
                                                                  src
