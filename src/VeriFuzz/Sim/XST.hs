{-|
Module      : VeriFuzz.Sim.XST
Description : XST (ise) simulator implementation.
Copyright   : (c) 2018-2019, Yann Herklotz
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

XST (ise) simulator implementation.
-}

{-# LANGUAGE QuasiQuotes #-}

module VeriFuzz.Sim.XST
    ( XST(..)
    , defaultXST
    )
where

import           Prelude                  hiding (FilePath)
import           Shelly
import           Text.Shakespeare.Text    (st)
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Sim.Template
import           VeriFuzz.Verilog.CodeGen

data XST = XST { xstPath    :: {-# UNPACK #-} !FilePath
               , netgenPath :: {-# UNPACK #-} !FilePath
               }
         deriving (Eq, Show)

instance Tool XST where
    toText _ = "xst"

instance Synthesisor XST where
    runSynth = runSynthXST

defaultXST :: XST
defaultXST = XST "xst" "netgen"

runSynthXST :: XST -> SourceInfo -> FilePath -> Sh ()
runSynthXST sim (SourceInfo top src) outf = do
    dir <- pwd
    writefile xstFile $ xstSynthConfig top
    writefile prjFile [st|verilog work "rtl.v"|]
    writefile "rtl.v" $ genSource src
    echoP "XST: run"
    logger_ dir "xst" $ timeout (xstPath sim) ["-ifn", toTextIgnore xstFile]
    echoP "XST: netgen"
    logger_ dir "netgen" $ run
        (netgenPath sim)
        [ "-w"
        , "-ofmt"
        , "verilog"
        , toTextIgnore $ modFile <.> "ngc"
        , toTextIgnore outf
        ]
    echoP "XST: clean"
    noPrint $ run_
        "sed"
        [ "-i"
        , "/^`ifndef/,/^`endif/ d; s/ *Timestamp: .*//;"
        , toTextIgnore outf
        ]
    echoP "XST: done"
  where
    modFile = "xst_" <> fromText top
    xstFile = modFile <.> "xst"
    prjFile = modFile <.> "prj"
