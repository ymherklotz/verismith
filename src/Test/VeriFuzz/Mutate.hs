{-|
Module      : Test.VeriFuzz.Mutation
Description : Functions to mutate the Verilog AST.
Copyright   : (c) Yann Herklotz Grave 2018
License     : GPL-3
Maintainer  : ymherklotz@gmail.com
Stability   : experimental
Portability : POSIX

Functions to mutate the Verilog AST from "Test.VeriFuzz.VerilogAST" to generate
more random patterns, such as nesting wires instead of creating new ones.
-}

module Test.VeriFuzz.Mutate where

