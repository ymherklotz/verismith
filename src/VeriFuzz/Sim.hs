{-|
Module      : VeriSmith.Sim
Description : Simulator implementations.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

Simulator implementations.
-}

module VeriSmith.Sim
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

import           VeriSmith.Sim.Icarus
import           VeriSmith.Sim.Identity
import           VeriSmith.Sim.Internal
import           VeriSmith.Sim.Quartus
import           VeriSmith.Sim.Vivado
import           VeriSmith.Sim.XST
import           VeriSmith.Sim.Yosys
