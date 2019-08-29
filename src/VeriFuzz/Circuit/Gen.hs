{-|
Module      : Verilog.Circuit.Gen
Description : Generate verilog from circuit.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

Generate verilog from circuit.
-}

module VeriSmith.Circuit.Gen
    ( generateAST
    )
where

import           Data.Graph.Inductive       (LNode, Node)
import qualified Data.Graph.Inductive       as G
import           Data.Maybe                 (catMaybes)
import           VeriSmith.Circuit.Base
import           VeriSmith.Circuit.Internal
import           VeriSmith.Verilog.AST
import           VeriSmith.Verilog.Mutate

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

genPortsAST :: (Circuit -> [Node]) -> Circuit -> [Port]
genPortsAST f c = port . frNode <$> f c where port = Port Wire False 4

-- | Generates the nested expression AST, so that it can then generate the
-- assignment expressions.
genAssignExpr :: Gate -> [Node] -> Maybe Expr
genAssignExpr _ []       = Nothing
genAssignExpr _ [n     ] = Just . Id $ frNode n
genAssignExpr g (n : ns) = BinOp wire oper <$> genAssignExpr g ns
  where
    wire = Id $ frNode n
    oper = fromGate g

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
genModuleDeclAST c = ModDecl i output ports (combineAssigns yPort a) []
  where
    i      = Identifier "gen_module"
    ports  = genPortsAST inputsC c
    output = []
    a      = genAssignAST c
    yPort  = Port Wire False 90 "y"

generateAST :: Circuit -> Verilog
generateAST c = Verilog [genModuleDeclAST c]
