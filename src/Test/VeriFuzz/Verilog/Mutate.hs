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
inPort id mod = inInput
  where
    inInput = any (\a -> a ^. portName == id) $ mod ^. modInPorts ++ mod ^. modOutPorts

-- | Find the last assignment of a specific wire/reg to an expression, and
-- returns that expression.
findAssign :: Identifier -> [ModItem] -> Maybe Expr
findAssign id items =
  safe last . catMaybes $ isAssign <$> items
  where
    isAssign (ModCA (ContAssign val expr))
      | val == id = Just expr
      | otherwise = Nothing
    isAssign _ = Nothing

-- | Transforms an expression by replacing an Identifier with an
-- expression. This is used inside 'transformOf' and 'traverseExpr' to replace
-- the 'Identifier' recursively.
idTrans :: Identifier -> Expr -> Expr -> Expr
idTrans i expr (Id id)
  | id == i = expr
  | otherwise = Id id
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
nestId id mod
  | not $ inPort id mod =
      let expr = fromMaybe def . findAssign id $ mod ^. moduleItems
      in mod & get %~ replace id expr
  | otherwise = mod
  where
    get = moduleItems . traverse . _ModCA . contAssignExpr
    def = Id id

-- | Replaces an identifier by a expression in all the module declaration.
nestSource :: Identifier -> VerilogSrc -> VerilogSrc
nestSource id src =
  src & getVerilogSrc . traverse . getDescription %~ nestId id

-- | Nest variables in the format @w[0-9]*@ up to a certain number.
nestUpTo :: Int -> VerilogSrc -> VerilogSrc
nestUpTo i src =
  foldl (flip nestSource) src $ Identifier . fromNode <$> [1..i]

allVars :: ModDecl -> [Identifier]
allVars mod =
  (mod ^.. modOutPorts . traverse . portName) ++ (mod ^.. modInPorts . traverse . portName)
-- $setup
-- >>> let mod = (ModDecl (Identifier "m") [Port Wire 5 (Identifier "y")] [Port Wire 5 "x"] [])
-- >>> let main = (ModDecl "main" [] [] [])

-- | Add a Module Instantiation using 'ModInst' from the first module passed to
-- it to the body of the second module. It first has to make all the inputs into
-- @reg@.
--
-- >>> instantiateMod mod main
-- module main;
-- wire [4:0] y;
-- reg [4:0] x;
-- m m1(y, x);
-- endmodule
-- <BLANKLINE>
instantiateMod :: ModDecl -> ModDecl -> ModDecl
instantiateMod mod main =
  main & moduleItems %~ ((out ++ regIn ++ [inst])++)
  where
    out = Decl Nothing <$> mod ^. modOutPorts
    regIn = Decl Nothing <$> (mod ^. modInPorts & traverse . portType .~ Reg False)
    inst = ModInst (mod ^. moduleId) (mod ^. moduleId <> (Identifier . showT $ count+1)) conns
    count = length . filter (==mod ^. moduleId) $ main ^.. moduleItems . traverse . modInstId
    conns = ModConn . Id <$> allVars mod

-- | Instantiate without adding wire declarations. It also does not count the
-- current instantiations of the same module.
--
-- >>> instantiateMod_ mod main
-- m m(y, x);
-- <BLANKLINE>
instantiateMod_ :: ModDecl -> ModItem
instantiateMod_ mod =
  ModInst (mod ^. moduleId) (mod ^. moduleId) conns
  where
    conns = ModConn . Id <$>
      (mod ^.. modOutPorts . traverse . portName) ++ (mod ^.. modInPorts . traverse . portName)

-- | Initialise all the inputs and outputs to a module.
--
-- >>> initMod mod
-- module m(y, x);
-- output wire [4:0] y;
-- input wire [4:0] x;
-- endmodule
-- <BLANKLINE>
initMod :: ModDecl -> ModDecl
initMod mod = mod & moduleItems %~ ((out ++ inp)++)
  where
    out = Decl (Just PortOut) <$> (mod ^. modOutPorts)
    inp = Decl (Just PortIn) <$> (mod ^. modInPorts)

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
