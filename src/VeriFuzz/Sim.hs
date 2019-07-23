{-|
Module      : VeriFuzz.Sim
Description : Simulator implementations.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

Simulator implementations.
-}

module VeriFuzz.Sim
    (
    -- * Simulators
    -- ** Icarus
      Icarus(..)
    , defaultIcarus
    -- * Synthesisers
    -- ** Yosys
    , Yosys(..)
    , defaultYosys
    -- ** Vivado
    , Vivado(..)
    , defaultVivado
    -- ** XST
    , XST(..)
    , defaultXST
    -- ** Quartus
    , Quartus(..)
    , defaultQuartus
    -- ** Identity
    , Identity(..)
    , defaultIdentity
    -- * Equivalence
    , runEquiv
    -- * Simulation
    , runSim
    -- * Synthesis
    , runSynth
    , logger
    )
where

import           VeriFuzz.Sim.Icarus
import           VeriFuzz.Sim.Identity
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Sim.Quartus
import           VeriFuzz.Sim.Vivado
import           VeriFuzz.Sim.XST
import           VeriFuzz.Sim.Yosys
