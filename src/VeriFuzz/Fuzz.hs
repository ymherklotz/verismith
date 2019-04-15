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

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeSynonymInstances   #-}

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
import           Prelude                          hiding (FilePath)
import           VeriFuzz.Sim.Icarus
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Sim.Quartus
import           VeriFuzz.Sim.Vivado
import           VeriFuzz.Sim.XST
import           VeriFuzz.Sim.Yosys

data Result = Pass
            | EquivFail
            | SimFail
            | TimeoutFail
            deriving (Eq, Show)

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

data SimResult = SimResult !SynthTool !SimTool !Result
               deriving (Eq, Show)

data SynthResult = SynthResult !SynthTool !SynthTool !Result
                 deriving (Eq, Show)

data FuzzResult = FuzzResult { getSynthResults :: ![SynthResult]
                             , getSimResults   :: ![SimResult]
                             }
                deriving (Eq, Show)

instance Semigroup FuzzResult where
    FuzzResult a1 b1 <> FuzzResult a2 b2 = FuzzResult (a1 <> a2) (b1 <> b2)

instance Monoid FuzzResult where
    mempty = FuzzResult [] []

type Fuzz m = StateT FuzzResult (ReaderT FuzzEnv m)

runFuzz :: (Monad m) => [SynthTool] -> [SimTool] -> Fuzz m a -> m a
runFuzz synth sim m =
    runReaderT (evalStateT m (FuzzResult [] [])) (FuzzEnv synth sim)

synthesisers :: (Monad m) => Fuzz m [SynthTool]
synthesisers = lift $ asks getSynthesisers

simulators :: (Monad m) => Fuzz m [SimTool]
simulators = lift $ asks getSimulators

fuzz :: (MonadIO m) => Fuzz m FuzzResult
fuzz = do
    _ <- synthesisers
    _ <- simulators
    return mempty
