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

import           Prelude                     hiding (FilePath)
import           Shelly
import           Text.Shakespeare.Text       (st)
import           VeriFuzz.AST
import           VeriFuzz.CodeGen
import           VeriFuzz.General
import           VeriFuzz.Internal.Simulator
import           VeriFuzz.Mutate

newtype Yosys = Yosys { yosysPath :: FilePath }

instance Simulator Yosys where
  toText _ = "yosys"

instance Synthesize Yosys where
  runSynth = runSynthYosys

defaultYosys :: Yosys
defaultYosys = Yosys "yosys"

writeSimFile
  :: Yosys    -- ^ Simulator instance
  -> ModDecl  -- ^ Current module
  -> FilePath -- ^ Output sim file
  -> Sh ()
writeSimFile _ m file = do
  writefile "rtl.v" $ genSource m
  writefile file yosysSimConfig

runSynthYosys :: Yosys -> ModDecl -> FilePath -> Sh ()
runSynthYosys sim m outf = do
  dir <- pwd
  writefile inpf $ genSource m
  echoP "Yosys: synthesis"
  _ <- logger dir "yosys" $ timeout (yosysPath sim) ["-q", "-b", "verilog -noattr", "-o", out, "-S", inp]
  echoP "Yosys: synthesis done"
 where
  inpf = "rtl.v"
  inp  = toTextIgnore inpf
  out  = toTextIgnore outf
  -- ids = T.intercalate "," $ allVars m ^.. traverse . getIdentifier

runMaybeSynth :: (Synthesize a) => Maybe a -> ModDecl -> Sh ()
runMaybeSynth (Just sim) m = runSynth sim m $ fromText [st|syn_#{toText sim}.v|]
runMaybeSynth Nothing    m = writefile "syn_rtl.v" $ genSource m

runEquivYosys :: (Synthesize a, Synthesize b) => Yosys -> a -> Maybe b -> ModDecl -> Sh ()
runEquivYosys yosys sim1 sim2 m = do
  writefile "top.v" . genSource . initMod $ makeTop 2 m
  writefile checkFile $ yosysSatConfig sim1 sim2 m
  runSynth sim1 m $ fromText [st|syn_#{toText sim1}.v|]
  runMaybeSynth sim2 m
  echoP "Yosys: equivalence check"
  run_ (yosysPath yosys) [toTextIgnore checkFile]
  echoP "Yosys: equivalence done"
  where
    checkFile = fromText [st|test.#{toText sim1}.#{maybe "rtl" toText sim2}.ys|]

runEquiv :: (Synthesize a, Synthesize b) => Yosys -> a -> Maybe b -> ModDecl -> Sh ()
runEquiv _ sim1 sim2 m = do
  root <- rootPath
  dir <- pwd
  echoP "SymbiYosys: setup"
  writefile "top.v" . genSource . initMod $ makeTopAssert m
  writefile "test.sby" $ sbyConfig root sim1 sim2 m
  runSynth sim1 m $ fromText [st|syn_#{toText sim1}.v|]
  runMaybeSynth sim2 m
  echoP "SymbiYosys: run"
  _ <- logger dir "symbiyosys" $ run "sby" ["test.sby"]
  echoP "SymbiYosys: done"
