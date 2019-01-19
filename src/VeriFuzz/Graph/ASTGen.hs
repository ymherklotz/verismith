{-|
Module      : VeriFuzz.Graph.ASTGen
Description : Generates the AST from the graph directly.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Generates the AST from the graph directly.
-}

module VeriFuzz.Graph.ASTGen where

import           Data.Foldable                  ( fold )
import           Data.Graph.Inductive           ( LNode
                                                , Node
                                                )
import qualified Data.Graph.Inductive          as G
import           Data.Maybe                     ( catMaybes )
import           VeriFuzz.Circuit
import           VeriFuzz.Internal.Gen
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.Helpers

-- | Converts a 'CNode' to an 'Identifier'.
frNode :: Node -> Identifier
frNode = Identifier . fromNode

-- | Converts a 'Gate' to a 'BinaryOperator', which should be a bijective
-- mapping.
fromGate :: Gate -> BinaryOperator
fromGate And = BinAnd
fromGate Or  = BinOr
fromGate Xor = BinXor

inputsC :: Circuit -> [Node]
inputsC c = inputs (getCircuit c)

outputsC :: Circuit -> [Node]
outputsC c = outputs (getCircuit c)

genPortsAST :: (Circuit -> [Node]) -> Circuit -> [Port]
genPortsAST f c = port . frNode <$> f c where port = Port Wire 4

-- | Generates the nested expression AST, so that it can then generate the
-- assignment expressions.
genAssignExpr :: Gate -> [Node] -> Maybe Expr
genAssignExpr _ []       = Nothing
genAssignExpr _ [n     ] = Just . Id $ frNode n
genAssignExpr g (n : ns) = BinOp wire op <$> genAssignExpr g ns
 where
  wire = Id $ frNode n
  op   = fromGate g

-- | Generate the continuous assignment AST for a particular node. If it does
-- not have any nodes that link to it then return 'Nothing', as that means that
-- the assignment will just be empty.
genContAssignAST :: Circuit -> LNode Gate -> Maybe ModItem
genContAssignAST c (n, g) = ModCA . ContAssign name <$> genAssignExpr g nodes
 where
  gr    = getCircuit c
  nodes = G.pre gr n
  name  = frNode n

genAssignAST :: Circuit -> [ModItem]
genAssignAST c = catMaybes $ genContAssignAST c <$> nodes
 where
  gr    = getCircuit c
  nodes = G.labNodes gr

genModuleDeclAST :: Circuit -> ModDecl
genModuleDeclAST c = ModDecl i output ports items
 where
  i      = Identifier "gen_module"
  ports  = genPortsAST inputsC c
  output = [Port Wire 90 "y"]
  items =
    genAssignAST c ++ [ModCA . ContAssign "y" . fold $ portToExpr <$> ports]

generateAST :: Circuit -> VerilogSrc
generateAST c = VerilogSrc [Description $ genModuleDeclAST c]
