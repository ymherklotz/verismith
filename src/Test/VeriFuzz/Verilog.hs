{-|
Module      : Test.VeriFuzz.Verilog
Description : The main verilog module with the syntax and code generation.
Copyright   : (c) Yann Herklotz Grave 2018
License     : GPL-3
Maintainer  : ymherklotz@gmail.com
Stability   : experimental
Portability : POSIX

The main verilog module with the syntax and code generation.
-}

module Test.VeriFuzz.Verilog
  ( -- * AST
    module Test.VeriFuzz.Verilog.AST
    -- * Code Generation
  , module Test.VeriFuzz.Verilog.CodeGen
    -- * Verilog mutations
  , module Test.VeriFuzz.Verilog.Mutate
  ) where

import           Test.VeriFuzz.Verilog.AST
import           Test.VeriFuzz.Verilog.CodeGen
import           Test.VeriFuzz.Verilog.Mutate
