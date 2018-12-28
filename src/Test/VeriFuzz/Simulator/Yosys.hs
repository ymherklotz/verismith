{-|
Module      : Test.VeriFuzz.Simulator.Yosys
Description : Yosys simulator implementation.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Yosys simulator implementation.
-}

{-# LANGUAGE QuasiQuotes #-}

module Test.VeriFuzz.Simulator.Yosys where

import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Prelude                         hiding (FilePath)
import           Shelly
import           Test.VeriFuzz.Simulator.General
import           Test.VeriFuzz.Verilog.AST
import           Test.VeriFuzz.Verilog.CodeGen
import           Text.Shakespeare.Text           (st)

data Yosys = Yosys { yosysPath :: FilePath }

instance Simulator Yosys where
  toText _ = "yosys"

instance Simulate Yosys where
  runSim = runSimYosys

instance Synthesize Yosys where
  runSynth = runSynthYosys

writeSimFile :: Yosys    -- ^ Simulator instance
             -> ModDecl  -- ^ Current module
             -> FilePath -- ^ Output sim file
             -> Sh ()
writeSimFile sim mod file = do
  writefile "rtl.v" $ genSource mod
  writefile file [st|read_verilog rtl.v; proc;;
rename mod mod_rtl
|]

runSimYosys :: Yosys -> ModDecl -> [Int] -> Sh Int
runSimYosys sim ver tb = return 0

runSynthYosys :: Yosys -> ModDecl -> FilePath -> Sh ()
runSynthYosys sim mod outf = do
  writefile inpf $ genSource mod
  run_ (yosysPath sim) ["-q", "-l", "synth.log", "-b", "verilog -noattr", "-o", out, "-S", inp]
  where
    inpf = "rtl.v"
    inp = toTextIgnore inpf
    out = toTextIgnore outf
