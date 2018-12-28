{-|
Module      : Test.VeriFuzz.Simulator.Icarus
Description : Icarus verilog module.
Copyright   : (c) Yann Herklotz Grave 2018
License     : GPL-3
Maintainer  : ymherklotz@gmail.com
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
