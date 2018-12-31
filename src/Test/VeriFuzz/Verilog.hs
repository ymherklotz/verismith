{-|
Module      : Test.VeriFuzz.Verilog
Description : The main verilog module with the syntax and code generation.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
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
  , module Test.VeriFuzz.Verilog.Helpers
  , module Test.VeriFuzz.Verilog.Arbitrary
  ) where

import           Test.VeriFuzz.Verilog.Arbitrary
import           Test.VeriFuzz.Verilog.AST
import           Test.VeriFuzz.Verilog.CodeGen
import           Test.VeriFuzz.Verilog.Helpers
import           Test.VeriFuzz.Verilog.Mutate
