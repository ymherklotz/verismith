{-|
Module      : VeriFuzz.Verilog.Mutation
Description : Functions to mutate the Verilog AST.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Functions to mutate the Verilog AST from "VeriFuzz.Verilog.AST" to generate
more random patterns, such as nesting wires instead of creating new ones.
-}

module VeriFuzz.Verilog.Mutate where

import           Control.Lens
import           Data.Maybe               (catMaybes, fromMaybe)
import           VeriFuzz.Internal.Gen
import           VeriFuzz.Internal.Shared
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.CodeGen

-- | Return if the 'Identifier' is in a 'ModDecl'.
inPort :: Identifier -> ModDecl -> Bool
inPort i m = inInput
  where
    inInput = any (\a -> a ^. portName == i) $ m ^. modInPorts ++ m ^. modOutPorts

-- | Find the last assignment of a specific wire/reg to an expression, and
-- returns that expression.
findAssign :: Identifier -> [ModItem] -> Maybe Expr
findAssign i items =
  safe last . catMaybes $ isAssign <$> items
  where
    isAssign (ModCA (ContAssign val expr))
      | val == i = Just expr
      | otherwise = Nothing
    isAssign _ = Nothing

-- | Transforms an expression by replacing an Identifier with an
-- expression. This is used inside 'transformOf' and 'traverseExpr' to replace
-- the 'Identifier' recursively.
idTrans :: Identifier -> Expr -> Expr -> Expr
idTrans i expr (Id id')
  | id' == i = expr
  | otherwise = Id id'
idTrans _ _ e = e

-- | Replaces the identifier recursively in an expression.
replace :: Identifier -> Expr -> Expr -> Expr
replace = (transformOf traverseExpr .) . idTrans

-- | Nest expressions for a specific 'Identifier'. If the 'Identifier' is not found,
-- the AST is not changed.
--
-- This could be improved by instead of only using the last assignment to the
-- wire that one finds, to use the assignment to the wire before the current
-- expression. This would require a different approach though.
nestId :: Identifier -> ModDecl -> ModDecl
nestId i m
  | not $ inPort i m =
      let expr = fromMaybe def . findAssign i $ m ^. modItems
      in m & get %~ replace i expr
  | otherwise = m
  where
    get = modItems . traverse . _ModCA . contAssignExpr
    def = Id i

-- | Replaces an identifier by a expression in all the module declaration.
nestSource :: Identifier -> VerilogSrc -> VerilogSrc
nestSource i src =
  src & getVerilogSrc . traverse . getDescription %~ nestId i

-- | Nest variables in the format @w[0-9]*@ up to a certain number.
nestUpTo :: Int -> VerilogSrc -> VerilogSrc
nestUpTo i src =
  foldl (flip nestSource) src $ Identifier . fromNode <$> [1..i]

allVars :: ModDecl -> [Identifier]
allVars m =
  (m ^.. modOutPorts . traverse . portName) ++ (m ^.. modInPorts . traverse . portName)
-- $setup
-- >>> let m = (ModDecl (Identifier "m") [Port Wire 5 (Identifier "y")] [Port Wire 5 "x"] [])
-- >>> let main = (ModDecl "main" [] [] [])

-- | Add a Module Instantiation using 'ModInst' from the first module passed to
-- it to the body of the second module. It first has to make all the inputs into
-- @reg@.
--
-- >>> render $ instantiateMod m main
-- module main;
-- wire [4:0] y;
-- reg [4:0] x;
-- m m1(y, x);
-- endmodule
-- <BLANKLINE>
instantiateMod :: ModDecl -> ModDecl -> ModDecl
instantiateMod m main =
  main & modItems %~ ((out ++ regIn ++ [inst])++)
  where
    out = Decl Nothing <$> m ^. modOutPorts
    regIn = Decl Nothing <$> (m ^. modInPorts & traverse . portType .~ Reg False)
    inst = ModInst (m ^. moduleId) (m ^. moduleId <> (Identifier . showT $ count+1)) conns
    count = length . filter (==m ^. moduleId) $ main ^.. modItems . traverse . modInstId
    conns = ModConn . Id <$> allVars m

-- | Instantiate without adding wire declarations. It also does not count the
-- current instantiations of the same module.
--
-- >>> render $ instantiateMod_ m
-- m m(y, x);
-- <BLANKLINE>
instantiateMod_ :: ModDecl -> ModItem
instantiateMod_ m =
  ModInst (m ^. moduleId) (m ^. moduleId) conns
  where
    conns = ModConn . Id <$>
      (m ^.. modOutPorts . traverse . portName) ++ (m ^.. modInPorts . traverse . portName)

-- | Initialise all the inputs and outputs to a module.
--
-- >>> render $ initMod m
-- module m(y, x);
-- output wire [4:0] y;
-- input wire [4:0] x;
-- endmodule
-- <BLANKLINE>
initMod :: ModDecl -> ModDecl
initMod m = m & modItems %~ ((out ++ inp)++)
  where
    out = Decl (Just PortOut) <$> (m ^. modOutPorts)
    inp = Decl (Just PortIn) <$> (m ^. modInPorts)

makeIdFrom :: (Show a) => a -> Identifier -> Identifier
makeIdFrom a i =
  (i<>) . Identifier . ("_"<>) $ showT a

-- | Make top level module for equivalence verification. Also takes in how many
-- modules to instantiate.
makeTop :: Int -> ModDecl -> ModDecl
makeTop i m =
  ModDecl (m ^. moduleId) ys (m ^. modInPorts) modItems
  where
    ys = Port Wire 90 . (flip makeIdFrom) "y" <$> [1..i]
    modItems = instantiateMod_ . modN <$> [1..i]
    modN n = m
             & moduleId %~ makeIdFrom n
             & modOutPorts .~ [Port Wire 90 (makeIdFrom n "y")]
