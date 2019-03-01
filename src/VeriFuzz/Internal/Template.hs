{-|
Module      : VeriFuzz.Internal.Template
Description : Template file for different configuration files
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Template file for different configuration files.
-}

{-# LANGUAGE QuasiQuotes #-}

module VeriFuzz.Internal.Template where

import           Control.Lens                ((^..))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Prelude                     hiding (FilePath)
import           Shelly
import           Text.Shakespeare.Text       (st)
import           VeriFuzz.AST
import           VeriFuzz.Internal.Simulator

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

-- brittany-disable-next-binding
yosysSatConfig :: (Tool a, Tool b) => a -> Maybe b -> SourceInfo -> Text
yosysSatConfig sim1 sim2 (SourceInfo top src) = [st|read_verilog syn_#{toText sim1}.v
#{rename "_1" mis}
read_verilog syn_#{idSim2}.v
#{rename "_2" mis}
read_verilog #{top}.v
proc; opt_clean
flatten #{top}
! touch test.#{toText sim1}.#{idSim2}.input_ok
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
sbyConfig :: (Tool a, Tool b) => FilePath -> a -> Maybe b -> SourceInfo -> Text
sbyConfig bd sim1 sim2 (SourceInfo top src) = [st|[options]
mode prove

[engines]
smtbmc

[script]
#{readL}
read -formal syn_#{toText sim1}.v
#{rename "_1" mis}
read -formal syn_#{maybe "rtl" toText sim2}.v
#{rename "_2" mis}
read -formal top.v
prep -top #{top}

[files]
#{depList}
syn_#{maybe "rtl" toText sim2}.v
syn_#{toText sim1}.v
top.v
|]
  where
    mis = src ^.. getSourceId
    deps = ["cells_cmos.v", "cells_cyclone_v.v", "cells_verific.v", "cells_xilinx_7.v"]
    depList =
      T.intercalate "\n"
        $   toTextIgnore
        .   ((bd </> fromText "data") </>)
        .   fromText
        <$> deps
    readL = T.intercalate "\n" $ mappend "read -formal " <$> deps