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

import           Control.Lens
import           Data.Maybe                    (catMaybes, fromMaybe)
import           Test.VeriFuzz.Internal.Gen
import           Test.VeriFuzz.Internal.Shared
import           Test.VeriFuzz.VerilogAST

-- | Return if the 'Identifier' is in a 'ModDecl'.
inPort :: Identifier -> ModDecl -> Bool
inPort id mod = any (\a -> a ^. portName == id) $ mod ^. modPorts

-- | Find the last assignment of a specific wire/reg to an expression, and
-- returns that expression.
findAssign :: Identifier -> [ModItem] -> Maybe Expression
findAssign id items =
  safe last . catMaybes $ isAssign <$> items
  where
    isAssign (ModCA (ContAssign val expr))
      | val == id = Just $ expr
      | otherwise = Nothing
    isAssign _ = Nothing

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
nestId :: Identifier -> ModDecl -> ModDecl
nestId id mod
  | not $ inPort id mod =
      let expr = fromMaybe def . findAssign id $ mod ^. moduleItems
      in mod & get %~ replace id expr
  | otherwise = mod
  where
    get = moduleItems . traverse . _ModCA . contAssignExpr
    def = PrimExpr $ PrimId id

-- | Replaces an identifier by a expression in all the module declaration.
nestSource :: Identifier -> SourceText -> SourceText
nestSource id src =
  src & getSourceText . traverse . getDescription %~ nestId id

-- | Nest variables in the format @w[0-9]*@ up to a certain number.
nestUpTo :: Int -> SourceText -> SourceText
nestUpTo i src =
  foldl (flip nestSource) src $ Identifier . fromNode <$> [1..i]

-- | Add a Module Instantiation using 'ModInst' from the first module passed to
-- it to the body of the second module.
instantiateMod :: ModDecl -> ModDecl -> ModDecl
instantiateMod mod main =
  main
