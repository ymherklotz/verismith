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

import qualified Data.Graph.Inductive     as G
import qualified Data.Text                as T
import           Test.VeriFuzz.Circuit
import           Test.VeriFuzz.VerilogAST

fromNode :: G.Node -> Identifier
fromNode node = Identifier . T.pack $ "w" <> show node

filterGr :: (G.Graph gr) => gr n e -> (G.Node -> Bool) -> [G.Node]
filterGr graph f =
  filter f $ G.nodes graph

genPortsAST :: Circuit -> [Port]
genPortsAST c = ((Port Input . fromNode) <$> inp) ++ ((Port Output) . fromNode <$> out)
  where
    zero fun1 fun2 n = fun1 graph n == 0 && fun2 graph n /= 0
    inp = filterGr graph $ zero G.indeg G.outdeg
    out = filterGr graph $ zero G.outdeg G.indeg
    graph = getCircuit c

genModuleDeclAST :: Circuit -> ModuleDecl
genModuleDeclAST c =
  ModuleDecl id ports items
  where
    id = Identifier "gen_module"
    ports = genPortsAST c
    items = []

generateAST :: Circuit -> SourceText
generateAST c =
  SourceText [Description $ genModuleDeclAST c]
