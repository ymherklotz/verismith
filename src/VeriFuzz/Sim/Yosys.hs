{-|
Module      : VeriFuzz.Sim.Yosys
Description : Yosys simulator implementation.
Copyright   : (c) 2018-2019, Yann Herklotz
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Yosys simulator implementation.
-}

{-# LANGUAGE QuasiQuotes #-}

module VeriFuzz.Sim.Yosys
    ( Yosys(..)
    , defaultYosys
    , runEquiv
    , runEquivYosys
    )
where

import           Control.Lens
import           Prelude                  hiding (FilePath)
import           Shelly
import           Shelly.Lifted            (liftSh)
import           Text.Shakespeare.Text    (st)
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Sim.Template
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.CodeGen
import           VeriFuzz.Verilog.Mutate

newtype Yosys = Yosys { yosysPath :: FilePath }
              deriving (Eq)

instance Tool Yosys where
  toText _ = "yosys"

instance Synthesiser Yosys where
  runSynth = runSynthYosys

instance Show Yosys where
    show _ = "yosys"

defaultYosys :: Yosys
defaultYosys = Yosys "yosys"

runSynthYosys :: Yosys -> SourceInfo -> FilePath -> ResultSh ()
runSynthYosys sim (SourceInfo _ src) outf = (<?> SynthFail) . liftSh $ do
    dir <- pwd
    writefile inpf $ genSource src
    echoP "Yosys: synthesis"
    logger_ dir "yosys"
        $ timeout
              (yosysPath sim)
              ["-b", "verilog -noattr", "-o", out, "-S", inp]
    echoP "Yosys: synthesis done"
  where
    inpf = "rtl.v"
    inp  = toTextIgnore inpf
    out  = toTextIgnore outf

runMaybeSynth :: (Synthesiser a) => Maybe a -> SourceInfo -> ResultSh ()
runMaybeSynth (Just sim) srcInfo =
    runSynth sim srcInfo $ fromText [st|syn_#{toText sim}.v|]
runMaybeSynth Nothing (SourceInfo _ src) =
    liftSh . writefile "syn_rtl.v" $ genSource src

runEquivYosys
    :: (Synthesiser a, Synthesiser b)
    => Yosys
    -> a
    -> Maybe b
    -> SourceInfo
    -> ResultSh ()
runEquivYosys yosys sim1 sim2 srcInfo = do
    liftSh $ do
        writefile "top.v"
            .  genSource
            .  initMod
            .  makeTop 2
            $  srcInfo
            ^. mainModule
        writefile checkFile $ yosysSatConfig sim1 sim2 srcInfo
    runSynth sim1 srcInfo $ fromText [st|syn_#{toText sim1}.v|]
    runMaybeSynth sim2 srcInfo
    liftSh $ do
        echoP "Yosys: equivalence check"
        run_ (yosysPath yosys) [toTextIgnore checkFile]
        echoP "Yosys: equivalence done"
  where
    checkFile =
        fromText [st|test.#{toText sim1}.#{maybe "rtl" toText sim2}.ys|]

runEquiv
    :: (Synthesiser a, Synthesiser b)
    => Yosys
    -> a
    -> Maybe b
    -> SourceInfo
    -> ResultSh ()
runEquiv _ sim1 sim2 srcInfo = do
    root <- liftSh rootPath
    dir  <- liftSh pwd
    liftSh $ do
        echoP "SymbiYosys: setup"
        writefile "top.v"
            .  genSource
            .  initMod
            .  makeTopAssert
            $  srcInfo
            ^. mainModule
        writefile "test.sby" $ sbyConfig root sim1 sim2 srcInfo
    runSynth sim1 srcInfo $ fromText [st|syn_#{toText sim1}.v|]
    runMaybeSynth sim2 srcInfo
    liftSh $ echoP "SymbiYosys: run"
    execute_ EquivFail dir "symbiyosys" "sby" ["-f", "test.sby"]
    liftSh $ echoP "SymbiYosys: done"
