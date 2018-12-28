{-|
Module      : Test.VeriFuzz.Graph.ASTGen
Description : Generates the AST from the graph directly.
Copyright   : (c) Yann Herklotz Grave 2018
License     : GPL-3
Maintainer  : ymherklotz@gmail.com
Stability   : experimental
Portability : POSIX

Generates the AST from the graph directly.
-}

module Test.VeriFuzz.Graph.ASTGen where

import           Data.Graph.Inductive       (LNode, Node)
import qualified Data.Graph.Inductive       as G
import           Data.Maybe                 (catMaybes)
import qualified Data.Text                  as T
import           Test.VeriFuzz.Circuit
import           Test.VeriFuzz.Internal.Gen
import           Test.VeriFuzz.Verilog.AST

-- | Converts a 'Node' to an 'Identifier'.
frNode :: Node -> Identifier
frNode = Identifier . fromNode

-- | Converts a 'Gate' to a 'BinaryOperator', which should be a bijective
-- mapping.
fromGate :: Gate -> BinaryOperator
fromGate And = BinAnd
fromGate Or  = BinOr
fromGate Xor = BinXor

genPortsAST :: Circuit -> [Port]
genPortsAST c =
  (port Input . frNode <$> inp) ++ (port Output . frNode <$> out)
  where
    inp = inputs graph
    out = outputs graph
    graph = getCircuit c
    port x = Port (Just x) Nothing

-- | Generates the nested expression AST, so that it can then generate the
-- assignment expressions.
genAssignExpr :: Gate -> [Node] -> Maybe Expression
genAssignExpr g [] = Nothing
genAssignExpr g (n:[]) = Just . PrimExpr . PrimId $ frNode n
genAssignExpr g (n:ns) = OpExpr wire op <$> genAssignExpr g ns
  where
    wire = PrimExpr . PrimId $ frNode n
    op = fromGate g

-- | Generate the continuous assignment AST for a particular node. If it does
-- not have any nodes that link to it then return 'Nothing', as that means that
-- the assignment will just be empty.
genContAssignAST :: Circuit -> LNode Gate -> Maybe ModItem
genContAssignAST c (n, g) = ModCA . ContAssign name <$> genAssignExpr g nodes
  where
    gr = getCircuit c
    nodes = G.pre gr n
    name = frNode n

genAssignAST :: Circuit -> [ModItem]
genAssignAST c = catMaybes $ genContAssignAST c <$> nodes
  where
    gr = getCircuit c
    nodes = G.labNodes gr

genModuleDeclAST :: Circuit -> ModDecl
genModuleDeclAST c = ModDecl id ports items
  where
    id = Identifier "gen_module"
    ports = genPortsAST c
    items = genAssignAST c

generateAST :: Circuit -> VerilogSrc
generateAST c = VerilogSrc [Description $ genModuleDeclAST c]
