{-|
Module      : VeriFuzz.Yosys
Description : Yosys simulator implementation.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Yosys simulator implementation.
-}

{-# LANGUAGE QuasiQuotes #-}

module VeriFuzz.Yosys where

import           Prelude               hiding (FilePath)
import           Shelly
import           Text.Shakespeare.Text (st)
import           VeriFuzz.AST
import           VeriFuzz.CodeGen
import           VeriFuzz.Internal
import           VeriFuzz.Mutate

newtype Yosys = Yosys { yosysPath :: FilePath }
              deriving (Eq, Show)

instance Tool Yosys where
  toText _ = "yosys"

instance Synthesisor Yosys where
  runSynth = runSynthYosys

defaultYosys :: Yosys
defaultYosys = Yosys "yosys"

writeSimFile
    :: Yosys      -- ^ Simulator instance
    -> VerilogSrc -- ^ Current Verilog source
    -> FilePath   -- ^ Output sim file
    -> Sh ()
writeSimFile _ src file = do
    writefile "rtl.v" $ genSource src
    writefile file yosysSimConfig

runSynthYosys :: Yosys -> SourceInfo -> FilePath -> Sh ()
runSynthYosys sim (SourceInfo _ src) outf = do
    dir <- pwd
    writefile inpf $ genSource src
    echoP "Yosys: synthesis"
    _ <- logger dir "yosys"
        $ timeout
              (yosysPath sim)
              ["-b", "verilog -noattr", "-o", out, "-S", inp]
    echoP "Yosys: synthesis done"
  where
    inpf = "rtl.v"
    inp  = toTextIgnore inpf
    out  = toTextIgnore outf

runMaybeSynth :: (Synthesisor a) => Maybe a -> SourceInfo -> Sh ()
runMaybeSynth (Just sim) srcInfo =
    runSynth sim srcInfo $ fromText [st|syn_#{toText sim}.v|]
runMaybeSynth Nothing (SourceInfo _ src) =
    writefile "syn_rtl.v" $ genSource src

runEquivYosys
    :: (Synthesisor a, Synthesisor b)
    => Yosys
    -> a
    -> Maybe b
    -> SourceInfo
    -> Sh ()
runEquivYosys yosys sim1 sim2 srcInfo = do
    writefile "top.v" . genSource . initMod . makeTop 2 $ mainModule srcInfo
    writefile checkFile $ yosysSatConfig sim1 sim2 srcInfo
    runSynth sim1 srcInfo $ fromText [st|syn_#{toText sim1}.v|]
    runMaybeSynth sim2 srcInfo
    echoP "Yosys: equivalence check"
    run_ (yosysPath yosys) [toTextIgnore checkFile]
    echoP "Yosys: equivalence done"
  where
    checkFile =
        fromText [st|test.#{toText sim1}.#{maybe "rtl" toText sim2}.ys|]

runEquiv
    :: (Synthesisor a, Synthesisor b)
    => Yosys
    -> a
    -> Maybe b
    -> SourceInfo
    -> Sh ()
runEquiv _ sim1 sim2 srcInfo = do
    root <- rootPath
    dir  <- pwd
    echoP "SymbiYosys: setup"
    writefile "top.v" . genSource . initMod . makeTopAssert $ mainModule srcInfo
    writefile "test.sby" $ sbyConfig root sim1 sim2 srcInfo
    runSynth sim1 srcInfo $ fromText [st|syn_#{toText sim1}.v|]
    runMaybeSynth sim2 srcInfo
    echoP "SymbiYosys: run"
    _ <- logger dir "symbiyosys" $ run "sby" ["-f", "test.sby"]
    echoP "SymbiYosys: done"
