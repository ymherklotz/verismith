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

{-# LANGUAGE OverloadedStrings #-}

module Test.VeriFuzz.Mutate where

import           Control.Lens
import           Data.Maybe                    (catMaybes, fromMaybe)
import           Test.VeriFuzz.Internal.Shared
import           Test.VeriFuzz.VerilogAST

-- | Return if the 'Identifier' is in a 'ModuleDecl'.
inPort :: Identifier -> ModuleDecl -> Bool
inPort id mod = any (\a -> a ^. portName == id) $ mod ^. modPorts

-- | Find the last assignment of a specific wire/reg to an expression, and
-- returns that expression.
findAssign :: Identifier -> [ModuleItem] -> Maybe Expression
findAssign id items =
  safe last . catMaybes $ isAssign <$> items
  where
    isAssign (Assign ca)
      | ca ^. contAssignNetLVal == id = Just $ ca ^. contAssignExpr
      | otherwise = Nothing

-- | Transforms an expression by replacing an Identifier with an
-- expression. This is used inside 'transformOf' and 'traverseExpr' to replace
-- the 'Identifier' recursively.
idTrans :: Identifier -> Expression -> Expression -> Expression
idTrans i expr (PrimExpr (PrimId id))
  | id == i = expr
  | otherwise = (PrimExpr (PrimId id))
idTrans _ _ e = e

-- | Replaces the identifier recursively in an expression.
replace :: Identifier -> Expression -> Expression -> Expression
replace = (transformOf traverseExpr .) . idTrans

-- | Nest expressions for a specific 'Identifier'. If the 'Identifier' is not found,
-- the AST is not changed.
--
-- This could be improved by instead of only using the last assignment to the
-- wire that one finds, to use the assignment to the wire before the current
-- expression. This would require a different approach though.
nestId :: Identifier -> ModuleDecl -> ModuleDecl
nestId id mod
  | not $ inPort id mod =
      let expr = fromMaybe def . findAssign id $ mod ^. moduleItems
      in mod & get %~ replace id expr
  | otherwise = mod
  where
    get = moduleItems . traverse . _Assign . contAssignExpr
    def = PrimExpr $ PrimId id

-- | Replaces an identifier by a expression in all the module declaration.
nestSource :: Identifier -> SourceText -> SourceText
nestSource id src =
  src & getSourceText . traverse . getDescription %~ nestId id
