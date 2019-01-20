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
import qualified Data.Text                  as T
import           Prelude                    hiding (FilePath)
import           Shelly
import           Text.Shakespeare.Text      (st)
import           VeriFuzz.Simulator.General
import           VeriFuzz.Verilog

-- brittany-disable-next-binding
yosysSatConfig :: (Simulator a, Simulator b) => a -> Maybe b -> ModDecl -> Text
yosysSatConfig sim1 sim2 m = [st|read_verilog syn_#{toText sim1}.v
rename #{mi} #{mi}_1
read_verilog syn_#{idSim2}.v
rename #{mi} #{mi}_2
read_verilog top.v
proc; opt_clean
flatten #{mi}
! touch test.#{toText sim1}.#{idSim2}.input_ok
sat -timeout 20 -show-all -verify-no-timeout -ignore_div_by_zero -prove y_1 y_2 #{mi}
|]
  where
    idSim2 = maybe "rtl" toText sim2
    mi = modName m

-- brittany-disable-next-binding
yosysSimConfig :: Text
yosysSimConfig = [st|read_verilog rtl.v; proc;;
rename mod mod_rtl
|]

-- brittany-disable-next-binding
xstSynthConfig :: ModDecl -> Text
xstSynthConfig m = [st|run
-ifn #{mi}.prj -ofn #{mi} -p artix7 -top #{mi}
-iobuf NO -ram_extract NO -rom_extract NO -use_dsp48 NO
-fsm_extract YES -fsm_encoding Auto
-change_error_to_warning "HDLCompiler:226 HDLCompiler:1832"
|]
  where
    mi = modName m

-- brittany-disable-next-binding
sbyConfig :: (Simulator a, Simulator b) => FilePath -> a -> Maybe b -> ModDecl -> Text
sbyConfig bd sim1 sim2 m = [st|[options]
mode prove

[engines]
smtbmc

[script]
#{readL}
read -formal syn_#{toText sim1}.v
rename #{mi} #{mi}_1
read -formal syn_#{maybe "rtl" toText sim2}.v
rename #{mi} #{mi}_2
read -formal top.v
prep -top #{mi}

[files]
#{depList}
syn_#{maybe "rtl" toText sim2}.v
syn_#{toText sim1}.v
top.v
|]
  where
    mi = modName m
    deps = ["cells_cmos.v", "cells_cyclone_v.v", "cells_verific.v", "cells_xilinx_7.v"]
    depList =
      T.intercalate "\n"
        $   toTextIgnore
        .   ((bd </> fromText "data") </>)
        .   fromText
        <$> deps
    readL = T.intercalate "\n" $ mappend "read -formal " <$> deps
