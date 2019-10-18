{-|
Module      : Verismith.Tool
Description : Simulator implementations.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

Simulator implementations.
-}

module Verismith.Tool
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

import           Verismith.Tool.Icarus
import           Verismith.Tool.Identity
import           Verismith.Tool.Internal
import           Verismith.Tool.Quartus
import           Verismith.Tool.Vivado
import           Verismith.Tool.XST
import           Verismith.Tool.Yosys
