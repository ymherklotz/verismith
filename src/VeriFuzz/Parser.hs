{-|
Module      : VeriFuzz.Parser
Description : Parser module for Verilog.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Parser module for Verilog.
-}

module VeriFuzz.Parser
    ( parseVerilog
    , uncomment
    , preprocess
    )
where

import           VeriFuzz.Parser.Parser
import           VeriFuzz.Parser.Preprocess
