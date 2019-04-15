{-|
Module      : VeriFuzz.Sim
Description : Simulator implementations.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
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
    -- ** Yosys
    , Yosys(..)
    , defaultYosys
    -- ** Vivado
    , Vivado(..)
    , defaultVivado
    -- ** XST
    , XST(..)
    , defaultXST
    -- * Reducer
    , reduce
    -- * Equivalence
    , runEquiv
    -- * Simulation
    , runSim
    -- * Synthesis
    , runSynth
    , echoP
    )
where

import           VeriFuzz.Sim.Icarus
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Sim.Reduce
import           VeriFuzz.Sim.Vivado
import           VeriFuzz.Sim.XST
import           VeriFuzz.Sim.Yosys
