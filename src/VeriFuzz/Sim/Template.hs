{-|
Module      : VeriFuzz.Sim.Template
Description : Template file for different configuration files
Copyright   : (c) 2019, Yann Herklotz
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Template file for different configuration files.
-}

{-# LANGUAGE QuasiQuotes #-}

module VeriFuzz.Sim.Template
    ( yosysSatConfig
    , yosysSimConfig
    , xstSynthConfig
    , vivadoSynthConfig
    , sbyConfig
    )
where

import           Control.Lens          ((^..))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Prelude               hiding (FilePath)
import           Shelly
import           Text.Shakespeare.Text (st)
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Verilog.AST

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

-- brittany-disable-next-binding
yosysSatConfig :: (Synthesiser a, Synthesiser b) => a -> Maybe b -> SourceInfo -> Text
yosysSatConfig sim1 sim2 (SourceInfo top src) = [st|read_verilog #{outputText sim1}
#{rename "_1" mis}
read_verilog syn_#{idSim2}.v
#{rename "_2" mis}
read_verilog #{top}.v
proc; opt_clean
flatten #{top}
sat -timeout 20 -show-all -verify-no-timeout -ignore_div_by_zero -prove y_1 y_2 #{top}
|]
  where
    idSim2 = maybe "rtl" toText sim2
    mis = src ^.. getSourceId

-- brittany-disable-next-binding
yosysSimConfig :: Text
yosysSimConfig = [st|read_verilog rtl.v; proc;;
rename mod mod_rtl
|]

-- brittany-disable-next-binding
xstSynthConfig :: Text -> Text
xstSynthConfig top = [st|run
-ifn #{top}.prj -ofn #{top} -p artix7 -top #{top}
-iobuf NO -ram_extract NO -rom_extract NO -use_dsp48 NO
-fsm_extract YES -fsm_encoding Auto
-change_error_to_warning "HDLCompiler:226 HDLCompiler:1832"
|]

-- brittany-disable-next-binding
vivadoSynthConfig :: Text -> Text -> Text
vivadoSynthConfig top outf = [st|
# CRITICAL WARNING: [Synth 8-5821] Potential divide by zero
set_msg_config -id {Synth 8-5821} -new_severity {WARNING}

read_verilog rtl.v
synth_design -part xc7k70t -top #{top}
write_verilog -force #{outf}
|]

-- brittany-disable-next-binding
sbyConfig :: (Synthesiser a, Synthesiser b) => a -> Maybe b -> SourceInfo -> Text
sbyConfig sim1 sim2 (SourceInfo top _) = [st|[options]
multiclock on
mode prove

[engines]
smtbmc z3

[script]
#{readL}
read -formal #{outputText sim1}
read -formal #{maybe "rtl.v" outputText sim2}
read -formal top.v
prep -top #{top}

[files]
#{depList}
#{maybe "rtl.v" outputText sim2}
#{outputText sim1}
top.v
|]
  where
    deps = ["cells_cmos.v", "cells_cyclone_v.v", "cells_verific.v", "cells_xilinx_7.v", "cells_yosys.v"]
    depList =
      T.intercalate "\n"
        $   toTextIgnore
        .   (fromText "data" </>)
        .   fromText
        <$> deps
    readL = T.intercalate "\n" $ mappend "read -formal " <$> deps
