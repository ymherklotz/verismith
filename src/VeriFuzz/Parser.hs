{-|
Module      : VeriFuzz.Parser
Description : Minimal Verilog parser to reconstruct the AST.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Minimal Verilog parser to reconstruct the AST. This parser does not support the
whole Verilog syntax, as the AST does not support it either.
-}

module VeriFuzz.Parser where

import           Data.Attoparsec.Text
import           Data.Text            (Text)
import qualified Data.Text            as T
import           VeriFuzz.AST

parseDescription :: Text -> Parser Description
parseDescription = undefined

parseVerilogSrc :: Text -> Parser VerilogSrc
parseVerilogSrc = VerilogSrc <$> many1 parseDescription
