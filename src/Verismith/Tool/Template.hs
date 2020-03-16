{-|
Module      : Verismith.Tool.Template
Description : Template file for different configuration files
Copyright   : (c) 2019, Yann Herklotz
License     : GPL-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

Template file for different configuration files.
-}

{-# LANGUAGE QuasiQuotes #-}

module Verismith.Tool.Template
    ( yosysSynthConfigStd
    , yosysSatConfig
    , yosysSimConfig
    , quartusLightSynthConfig
    , quartusSynthConfig
    , xstSynthConfig
    , vivadoSynthConfig
    , sbyConfig
    , icarusTestbench
    )
where

import           Control.Lens              ((^..))
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Prelude                   hiding (FilePath)
import           Shelly
import           Text.Shakespeare.Text     (st)
import           Verismith.Tool.Internal
import           Verismith.Verilog.AST
import           Verismith.Verilog.CodeGen

rename :: Text -> [Text] -> Text
rename end entries =
    T.intercalate "\n"
        $   flip mappend end
        .   mappend "rename "
        .   doubleName
        <$> entries
{-# INLINE rename #-}

doubleName :: Text -> Text
doubleName n = n <> " " <> n
{-# INLINE doubleName #-}

outputText :: Synthesiser a => a -> Text
outputText = toTextIgnore . synthOutput

yosysSynthConfig :: Synthesiser a => Text -> a -> FilePath -> Text
yosysSynthConfig t a fp = [st|read_verilog #{toTextIgnore fp}
#{t}
write_verilog #{outputText a}
|]

yosysSynthConfigStd :: Synthesiser a => a -> FilePath -> Text
yosysSynthConfigStd = yosysSynthConfig "synth"

yosysSatConfig :: (Synthesiser a, Synthesiser b) => a -> b -> (SourceInfo ann) -> Text
yosysSatConfig sim1 sim2 (SourceInfo top src) = [st|read_verilog #{outputText sim1}
#{rename "_1" mis}
read_verilog syn_#{outputText sim2}.v
#{rename "_2" mis}
read_verilog #{top}.v
proc; opt_clean
flatten #{top}
sat -timeout 20 -show-all -verify-no-timeout -ignore_div_by_zero -prove y_1 y_2 #{top}
|]
  where
    mis = src ^.. getSourceId

yosysSimConfig :: Text
yosysSimConfig = [st|read_verilog rtl.v; proc;;
rename mod mod_rtl
|]

quartusLightSynthConfig :: Synthesiser a => a -> FilePath -> Text -> FilePath -> Text
quartusLightSynthConfig q sdc top fp = [st|load_package flow

project_new -overwrite #{top}

set_global_assignment -name FAMILY "Cyclone V"
set_global_assignment -name SYSTEMVERILOG_FILE #{toTextIgnore fp}
set_global_assignment -name TOP_LEVEL_ENTITY #{top}
set_global_assignment -name SDC_FILE #{toTextIgnore sdc}
set_global_assignment -name INI_VARS "qatm_force_vqm=on;"
set_global_assignment -name NUM_PARALLEL_PROCESSORS 2
set_instance_assignment -name VIRTUAL_PIN ON -to *

execute_module -tool map
execute_module -tool fit
execute_module -tool sta -args "--mode=implement"
execute_module -tool eda -args "--simulation --tool=vcs"

project_close
|]

quartusSynthConfig :: Synthesiser a => a -> FilePath -> Text -> FilePath -> Text
quartusSynthConfig q sdc top fp = [st|load_package flow

project_new -overwrite #{top}

set_global_assignment -name FAMILY "Cyclone 10 GX"
set_global_assignment -name SYSTEMVERILOG_FILE #{toTextIgnore fp}
set_global_assignment -name TOP_LEVEL_ENTITY #{top}
set_global_assignment -name SDC_FILE #{toTextIgnore sdc}
set_global_assignment -name INI_VARS "qatm_force_vqm=on;"
set_global_assignment -name NUM_PARALLEL_PROCESSORS 2
set_instance_assignment -name VIRTUAL_PIN ON -to *

execute_module -tool syn
execute_module -tool eda -args "--simulation --tool=vcs"

project_close
|]

xstSynthConfig :: Text -> Text
xstSynthConfig top = [st|run
-ifn #{top}.prj -ofn #{top} -p artix7 -top #{top}
-iobuf NO -ram_extract NO -rom_extract NO -use_dsp48 NO
-fsm_extract YES -fsm_encoding Auto
-change_error_to_warning "HDLCompiler:226 HDLCompiler:1832"
|]

vivadoSynthConfig :: Text -> Text -> Text
vivadoSynthConfig top outf = [st|
# CRITICAL WARNING: [Synth 8-5821] Potential divide by zero
set_msg_config -id {Synth 8-5821} -new_severity {WARNING}

read_verilog rtl.v
synth_design -part xc7k70t -top #{top}
write_verilog -force #{outf}
|]

sbyConfig :: (Synthesiser a, Synthesiser b) => Maybe Text -> FilePath -> a -> b -> (SourceInfo ann) -> Text
sbyConfig mt datadir sim1 sim2 (SourceInfo top _) = [st|[options]
multiclock on
mode prove
aigsmt #{fromMaybe "none" mt}

[engines]
abc pdr

[script]
#{readL}
read -formal #{outputText sim1}
read -formal #{outputText sim2}
read -formal top.v
prep -top #{top}

[files]
#{depList}
#{outputText sim2}
#{outputText sim1}
top.v
|]
  where
    deps = ["cells_cmos.v", "cells_cyclone_v.v", "cells_verific.v", "cells_xilinx_7.v", "cells_yosys.v"]
    depList =
      T.intercalate "\n"
        $   toTextIgnore
        .   (datadir </> fromText "data" </>)
        .   fromText
        <$> deps
    readL = T.intercalate "\n" $ mappend "read -formal " <$> deps

icarusTestbench :: (Synthesiser a) => FilePath -> (Verilog ann) -> a -> Text
icarusTestbench datadir t synth1 = [st|
`include "#{ddir}/data/cells_cmos.v"
`include "#{ddir}/data/cells_cyclone_v.v"
`include "#{ddir}/data/cells_verific.v"
`include "#{ddir}/data/cells_xilinx_7.v"
`include "#{ddir}/data/cells_yosys.v"
`include "#{toTextIgnore $ synthOutput synth1}"

#{genSource t}
|]
  where
    ddir = toTextIgnore datadir
