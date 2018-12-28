{-|
Module      : Test.VeriFuzz.Simulator.Xst
Description : Xst (ise) simulator implementation.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Xst (ise) simulator implementation.
-}

{-# LANGUAGE QuasiQuotes #-}

module Test.VeriFuzz.Simulator.Xst where

import           Control.Lens                    hiding ((<.>))
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Prelude                         hiding (FilePath)
import           Shelly
import           Test.VeriFuzz.Simulator.General
import           Test.VeriFuzz.Verilog.AST
import           Test.VeriFuzz.Verilog.CodeGen
import           Text.Shakespeare.Text           (st)

data Xst = Xst { xstPath :: FilePath }

instance Simulator Xst where
  toText _ = "xst"

instance Synthesize Xst where
  runSynth = runSynthXst

runSynthXst :: Xst -> ModDecl -> FilePath -> Sh ()
runSynthXst sim mod outf = do
  writefile xstFile [st|run
-ifn #{modName}.prj -ofn #{modName} -p artix7 -top #{modName}
-iobuf NO -ram_extract NO -rom_extract NO -use_dsp48 NO
-fsm_extract YES -fsm_encoding Auto
-change_error_to_warning "HDLCompiler:226 HDLCompiler:1832"
|]
  writefile prjFile [st|verilog work "#{modName}.v"|]
  writefile vFile $ genSource mod
  timeout_ "/opt/Xilinx/14.7/ISE_DS/ISE/bin/lin64/xst" ["-ifn", toTextIgnore xstFile]
  run_ "netgen" ["-w", "-ofmt", "verilog", toTextIgnore $ modFile <.> "ngc", "output.v"]
  where
    modName = mod ^. moduleId . getIdentifier
    modFile = fromText modName
    xstFile = modFile <.> "xst"
    prjFile = modFile <.> "prj"
    vFile = modFile <.> "v"
