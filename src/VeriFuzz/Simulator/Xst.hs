{-|
Module      : VeriFuzz.Simulator.Xst
Description : Xst (ise) simulator implementation.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Xst (ise) simulator implementation.
-}

{-# LANGUAGE QuasiQuotes #-}

module VeriFuzz.Simulator.Xst where

import           Control.Lens                         hiding ((<.>))
import           Prelude                              hiding (FilePath)
import           Shelly
import           Text.Shakespeare.Text                (st)
import           VeriFuzz.Simulator.General
import           VeriFuzz.Simulator.Internal.Template
import           VeriFuzz.Verilog
import           VeriFuzz.Verilog

data Xst = Xst { xstPath    :: FilePath
               , netgenPath :: FilePath
               }

instance Simulator Xst where
  toText _ = "xst"

instance Synthesize Xst where
  runSynth = runSynthXst

defaultXst :: Xst
defaultXst =
  Xst "/opt/Xilinx/14.7/ISE_DS/ISE/bin/lin64/xst" "/opt/Xilinx/14.7/ISE_DS/ISE/bin/lin64/netgen"

runSynthXst :: Xst -> ModDecl -> FilePath -> Sh ()
runSynthXst sim m outf = do
  writefile xstFile $ xstSynthConfig m
  writefile prjFile [st|verilog work "rtl.v"|]
  writefile "rtl.v" $ genSource m
  timeout_ (xstPath sim) ["-ifn", toTextIgnore xstFile]
  run_ (netgenPath sim)
       ["-w", "-ofmt", "verilog", toTextIgnore $ modFile <.> "ngc", toTextIgnore outf]
  run_ "sed" ["-i", "/^`ifndef/,/^`endif/ d; s/ *Timestamp: .*//;", toTextIgnore outf]
 where
  modFile = fromText $ modName m
  xstFile = modFile <.> "xst"
  prjFile = modFile <.> "prj"
