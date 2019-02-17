{-|
Module      : VeriFuzz.XST
Description : Xst (ise) simulator implementation.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Xst (ise) simulator implementation.
-}

{-# LANGUAGE QuasiQuotes #-}

module VeriFuzz.XST where

import           Prelude                     hiding (FilePath)
import           Shelly
import           Text.Shakespeare.Text       (st)
import           VeriFuzz.AST
import           VeriFuzz.CodeGen
import           VeriFuzz.General
import           VeriFuzz.Internal.AST
import           VeriFuzz.Internal.Simulator

data Xst = Xst { xstPath    :: FilePath
               , netgenPath :: FilePath
               }

instance Simulator Xst where
  toText _ = "xst"

instance Synthesize Xst where
  runSynth = runSynthXst

defaultXst :: Xst
defaultXst = Xst "xst" "netgen"

runSynthXst :: Xst -> ModDecl -> FilePath -> Sh ()
runSynthXst sim m outf = do
    dir <- pwd
    writefile xstFile $ xstSynthConfig m
    writefile prjFile [st|verilog work "rtl.v"|]
    writefile "rtl.v" $ genSource m
    echoP "XST: run"
    _ <- logger dir "xst" $ timeout (xstPath sim) ["-ifn", toTextIgnore xstFile]
    echoP "XST: netgen"
    _ <- logger dir "netgen" $ run
        (netgenPath sim)
        ["-w", "-ofmt", "verilog", toTextIgnore $ modFile <.> "ngc", toTextIgnore outf]
    echoP "XST: clean"
    noPrint $ run_ "sed" ["-i", "/^`ifndef/,/^`endif/ d; s/ *Timestamp: .*//;", toTextIgnore outf]
    echoP "XST: done"
  where
    modFile = fromText $ modName m
    xstFile = modFile <.> "xst"
    prjFile = modFile <.> "prj"
