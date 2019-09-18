{-|
Module      : Verismith.Sim
Description : Simulator implementations.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

Simulator implementations.
-}

module Verismith.Sim
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

import           Verismith.Sim.Icarus
import           Verismith.Sim.Identity
import           Verismith.Sim.Internal
import           Verismith.Sim.Quartus
import           Verismith.Sim.Vivado
import           Verismith.Sim.XST
import           Verismith.Sim.Yosys
