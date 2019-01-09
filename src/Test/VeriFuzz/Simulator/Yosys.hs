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

import           Control.Lens
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as B
import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Prelude                         hiding (FilePath)
import           Shelly
import           Test.VeriFuzz.Simulator.General
import           Test.VeriFuzz.Verilog
import           Text.Shakespeare.Text           (st)

newtype Yosys = Yosys { yosysPath :: FilePath }

instance Simulator Yosys where
  toText _ = "yosys"

instance Simulate Yosys where
  runSim = runSimYosys

instance Synthesize Yosys where
  runSynth = runSynthYosys

defaultYosys :: Yosys
defaultYosys = Yosys "/usr/bin/yosys"

writeSimFile :: Yosys    -- ^ Simulator instance
             -> ModDecl  -- ^ Current module
             -> FilePath -- ^ Output sim file
             -> Sh ()
writeSimFile sim m file = do
  writefile "rtl.v" $ genSource m
  writefile file [st|read_verilog rtl.v; proc;;
rename mod mod_rtl
|]

runSimYosys :: Yosys -> ModDecl -> [ByteString] -> Sh Int
runSimYosys sim ver tb = return 0

runSynthYosys :: Yosys -> ModDecl -> FilePath -> Sh ()
runSynthYosys sim m outf = do
  writefile inpf $ genSource m
  run_ (yosysPath sim) ["-q", "-b", "verilog -noattr", "-o", out, "-S", inp]
  where
    inpf = "rtl.v"
    inp = toTextIgnore inpf
    out = toTextIgnore outf

writeSatFile :: (Synthesize a, Synthesize b) => Text -> a -> Maybe b -> ModDecl -> Sh ()
writeSatFile checkFile sim1 sim2 m =
  writefile (fromText checkFile) [st|read_verilog syn_#{toText sim1}.v
rename #{modName} #{modName}_1
read_verilog syn_#{idSim2}.v
rename #{modName} #{modName}_2
read_verilog top.v
proc; opt_clean
flatten #{modName}
! touch test.#{toText sim1}.#{idSim2}.input_ok
sat -timeout 20 -verify-no-timeout -ignore_div_by_zero -prove y_1 y_2 #{modName}
|]
  where
    idSim2 = maybe "rtl" toText sim2
    modName = m ^. moduleId . getIdentifier
    ids = T.intercalate "," $ allVars m ^.. traverse . getIdentifier

runOtherSynth :: (Synthesize a) => Maybe a -> ModDecl -> Sh ()
runOtherSynth (Just sim) m = runSynth sim m $ fromText [st|syn_#{toText sim}.v|]
runOtherSynth Nothing m = writefile "syn_rtl.v" $ genSource m

runEquiv :: (Synthesize a, Synthesize b) => Yosys -> a -> Maybe b -> ModDecl -> Sh ()
runEquiv yosys sim1 sim2 m = do
  writefile "top.v" . genSource . initMod $ makeTop 2 m
  writeSatFile checkFile sim1 sim2 m
  runSynth sim1 m $ fromText [st|syn_#{toText sim1}.v|]
  runOtherSynth sim2 m
  run_ (yosysPath yosys) [checkFile]
  where
    checkFile = [st|test.#{toText sim1}.#{maybe "rtl" toText sim2}.ys|]
