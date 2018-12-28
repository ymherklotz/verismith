{-|
Module      : Test.VeriFuzz.Simulator.Icarus
Description : Icarus verilog module.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Icarus verilog module.
-}

{-# LANGUAGE QuasiQuotes #-}

module Test.VeriFuzz.Simulator.Icarus where

import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Prelude                         hiding (FilePath)
import           Shelly
import           Test.VeriFuzz.Simulator.General
import           Test.VeriFuzz.Verilog.AST
import           Test.VeriFuzz.Verilog.CodeGen
import           Text.Shakespeare.Text           (st)

data Icarus = Icarus { icarusPath :: FilePath }

instance Simulator Icarus where
  toText _ = "iverilog"

instance Simulate Icarus where
  runSim = runSimIcarus

runSimIcarus :: Icarus -> ModDecl -> [Int] -> Sh Int
runSimIcarus sim mod inp = return 0
