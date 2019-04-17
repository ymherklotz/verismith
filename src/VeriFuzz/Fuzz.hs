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
    )
where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       hiding (local)
import           Control.Monad.Trans.State.Strict
import           Data.List                        (nubBy)
import           Hedgehog                         (Gen)
import           Prelude                          hiding (FilePath)
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
               deriving (Eq, Show)

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

newtype SimTool = IcarusSim Icarus
                deriving (Eq, Show)

instance Tool SimTool where
    toText (IcarusSim icarus) = toText icarus

instance Simulator SimTool where
    runSim (IcarusSim icarus) = runSim icarus
    runSimWithFile (IcarusSim icarus) = runSimWithFile icarus

data FuzzEnv = FuzzEnv { getSynthesisers :: ![SynthTool]
                       , getSimulators   :: ![SimTool]
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

runFuzz :: (Monad m) => [SynthTool] -> [SimTool] -> Fuzz a m b -> m b
runFuzz synth sim m =
    runReaderT (evalStateT m (FuzzResult [] [])) (FuzzEnv synth sim)

synthesisers :: (Monad m) => Fuzz () m [SynthTool]
synthesisers = lift $ asks getSynthesisers

simulators :: (Monad m) => Fuzz () m [SimTool]
simulators = lift $ asks getSimulators

combinations :: [a] -> [b] -> [(a, b)]
combinations l1 l2 = [ (x, y) | x <- l1, y <- l2 ]

fuzz :: (MonadIO m) => Gen SourceInfo -> Fuzz () m (FuzzResult ())
fuzz _ = do
    synth <- synthesisers
    sim   <- simulators
    let synthComb =
            nubBy tupEq . filter (uncurry (/=)) $ combinations synth synth
    let simComb = combinations synth sim
    return mempty
    where tupEq (a, b) (a', b') = (a == a' && b == b') || (a == b' && b == a')
