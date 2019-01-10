{-|
Module      : VeriFuzz.Verilog
Description : The main verilog module with the syntax and code generation.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

The main verilog module with the syntax and code generation.
-}

module VeriFuzz.Verilog
  ( -- * AST
    module VeriFuzz.Verilog.AST
    -- * Code Generation
  , module VeriFuzz.Verilog.CodeGen
    -- * Verilog mutations
  , module VeriFuzz.Verilog.Mutate
  , module VeriFuzz.Verilog.Helpers
  ) where

import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.CodeGen
import           VeriFuzz.Verilog.Helpers
import           VeriFuzz.Verilog.Mutate
