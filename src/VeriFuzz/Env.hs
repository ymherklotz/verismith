{-|
Module      : VeriFuzz.Env
Description : Environment to run the simulator and synthesisers in a matrix.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Environment to run the simulator and synthesisers in a matrix.
-}

module VeriFuzz.Env where

import           Prelude           hiding (FilePath)
import           Shelly
import           VeriFuzz.Icarus
import           VeriFuzz.Internal
import           VeriFuzz.XST
import           VeriFuzz.Yosys

data SynthTool = XstSynth {-# UNPACK #-} !Xst
               | YosysSynth {-# UNPACK #-} !Yosys
               deriving (Eq, Show)

instance Tool SynthTool where
    toText (XstSynth xst)     = toText xst
    toText (YosysSynth yosys) = toText yosys

instance Synthesisor SynthTool where
    runSynth (XstSynth xst)     = runSynth xst
    runSynth (YosysSynth yosys) = runSynth yosys

newtype SimTool = IcarusSim Icarus
                deriving (Eq, Show)

instance Tool SimTool where
    toText (IcarusSim icarus) = toText icarus

instance Simulator SimTool where
    runSim (IcarusSim icarus) = runSim icarus
    runSimWithFile (IcarusSim icarus) = runSimWithFile icarus

data SimEnv = SimEnv { simTools :: [SimTool]
                     , simDir   :: FilePath
                     }

data SynthEnv = SynthEnv { synthTools :: [SynthTool]
                         , synthDir   :: FilePath
                         }


