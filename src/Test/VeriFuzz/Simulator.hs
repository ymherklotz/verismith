{-|
Module      : Test.VeriFuzz.Simulator
Description : Simulator module.
Copyright   : (c) Yann Herklotz Grave 2018
License     : GPL-3
Maintainer  : ymherklotz@gmail.com
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
