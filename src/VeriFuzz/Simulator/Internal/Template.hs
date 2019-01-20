{-|
Module      : VeriFuzz.Simulator.Internal.Template
Description : Template file for different configuration files
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Template file for different configuration files.
-}

{-# LANGUAGE QuasiQuotes #-}

module VeriFuzz.Simulator.Internal.Template where

import           Data.Text                  (Text)
import           Text.Shakespeare.Text      (st)
import           VeriFuzz.Simulator.General
import           VeriFuzz.Verilog

yosysSatConfig :: (Simulator a, Simulator b) => a -> Maybe b -> ModDecl -> Text
yosysSatConfig sim1 sim2 m = [st|read_verilog syn_#{toText sim1}.v
rename #{modName m} #{modName m}_1
read_verilog syn_#{idSim2}.v
rename #{modName m} #{modName m}_2
read_verilog top.v
proc; opt_clean
flatten #{modName m}
! touch test.#{toText sim1}.#{idSim2}.input_ok
sat -timeout 20 -show-all -verify-no-timeout -ignore_div_by_zero -prove y_1 y_2 #{modName m}
|]
  where
    idSim2 = maybe "rtl" toText sim2

yosysSimConfig :: Text
yosysSimConfig = [st|read_verilog rtl.v; proc;;
rename mod mod_rtl
|]

xstSynthConfig :: ModDecl -> Text
xstSynthConfig m = [st|run
-ifn #{modName m}.prj -ofn #{modName m} -p artix7 -top #{modName m}
-iobuf NO -ram_extract NO -rom_extract NO -use_dsp48 NO
-fsm_extract YES -fsm_encoding Auto
-change_error_to_warning "HDLCompiler:226 HDLCompiler:1832"
|]
