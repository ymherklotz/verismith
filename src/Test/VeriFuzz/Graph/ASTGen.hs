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

{-# LANGUAGE OverloadedStrings #-}

module Test.VeriFuzz.Graph.ASTGen where

import qualified Data.Graph.Inductive       (LNode, Node)
import qualified Data.Graph.Inductive       as G
import qualified Data.Text                  as T
import           Test.VeriFuzz.Circuit
import           Test.VeriFuzz.Internal.Gen
import           Test.VeriFuzz.VerilogAST

frNode :: Node -> Identifier
frNode = Identifier . fromNode

fromGate :: Gate -> BinaryOperator
fromGate And = BinAnd
fromGate Or  = BinOr
fromGate Xor = BinXor

genPortsAST :: Circuit -> [Port]
genPortsAST c =
  ((Port Input . frNode) <$> inp) ++ ((Port Output) . frNode <$> out)
  where
    inp = inputs graph
    out = outputs graph
    graph = getCircuit c

genAssignExpr :: Gate -> [Node] -> Expression
genAssignExpr g ns = (error "FIXME: Not yet done")

genContAssignAST :: Circuit -> LNode Gate -> ContAssign
genContAssignAST c (n, g) =
  ContAssign name $ genAssignExpr g nodes
  where
    gr = getCircuit c
    nodes = G.pre gr n
    name = frNode n

genAssignAST :: Circuit -> [ContAssign]
genAssignAST c =
  nodes
  where
    gr = getCircuit c
    nodes = G.labNodes gr

genModuleDeclAST :: Circuit -> ModuleDecl
genModuleDeclAST c =
  ModuleDecl id ports items
  where
    id = Identifier "gen_module"
    ports = genPortsAST c
    items = Assign <$> genAssignAST c

generateAST :: Circuit -> SourceText
generateAST c =
  SourceText [Description $ genModuleDeclAST c]
