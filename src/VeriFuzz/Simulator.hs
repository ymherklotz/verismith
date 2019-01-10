{-|
Module      : Test.VeriFuzz.Simulator
Description : Simulator module.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Simulator module.
-}

module Test.VeriFuzz.Simulator
  ( module Test.VeriFuzz.Simulator.General
  , module Test.VeriFuzz.Simulator.Yosys
  , module Test.VeriFuzz.Simulator.Xst
  , module Test.VeriFuzz.Simulator.Icarus
  ) where

import           Test.VeriFuzz.Simulator.General
import           Test.VeriFuzz.Simulator.Icarus
import           Test.VeriFuzz.Simulator.Xst
import           Test.VeriFuzz.Simulator.Yosys
