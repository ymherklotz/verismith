{-|
Module      : VeriFuzz.Report
Description : Generate a report from a fuzz run.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Generate a report from a fuzz run.
-}

{-# LANGUAGE TemplateHaskell #-}

module VeriFuzz.Report
    ( SynthTool(..)
    , SynthStatus(..)
    , SimTool(..)
    , FuzzReport(..)
    , synthResults
    , simResults
    , synthStatus
    , defaultIcarusSim
    , defaultVivadoSynth
    , defaultYosysSynth
    , defaultXSTSynth
    , defaultQuartusSynth
    , descriptionToSim
    , descriptionToSynth
    )
where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.ByteString             (ByteString)
import           Prelude                     hiding (FilePath)
import           Shelly.Lifted               (MonadSh)
import           VeriFuzz.Config
import           VeriFuzz.Result
import           VeriFuzz.Sim.Icarus
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Sim.Quartus
import           VeriFuzz.Sim.Vivado
import           VeriFuzz.Sim.XST
import           VeriFuzz.Sim.Yosys

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
data FuzzReport = FuzzReport { _synthResults :: ![SynthResult]
                             , _simResults   :: ![SimResult]
                             , _synthStatus  :: ![SynthStatus]
                             }
                  deriving (Eq, Show)

$(makeLenses ''FuzzReport)

instance Semigroup FuzzReport where
    FuzzReport a1 b1 c1 <> FuzzReport a2 b2 c2 = FuzzReport (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance Monoid FuzzReport where
    mempty = FuzzReport [] [] []

descriptionToSim :: SimDescription -> SimTool
descriptionToSim (SimDescription "icarus") = defaultIcarusSim
descriptionToSim s =
    error $ "Could not find implementation for simulator '" <> show s <> "'"

descriptionToSynth :: SynthDescription -> SynthTool
descriptionToSynth (SynthDescription "yosys"  ) = defaultYosysSynth
descriptionToSynth (SynthDescription "vivado" ) = defaultVivadoSynth
descriptionToSynth (SynthDescription "xst"    ) = defaultXSTSynth
descriptionToSynth (SynthDescription "quartus") = defaultQuartusSynth
descriptionToSynth s =
    error $ "Could not find implementation for synthesiser '" <> show s <> "'"
