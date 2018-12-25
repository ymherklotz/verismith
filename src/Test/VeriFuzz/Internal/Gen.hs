{-|
Module      : Test.VeriFuzz.Internal.Gen
Description : Internal helpers for generation.
Copyright   : (c) Yann Herklotz Grave 2018
License     : GPL-3
Maintainer  : ymherklotz@gmail.com
Stability   : experimental
Portability : POSIX

Internal helpers for generation.
-}

module Test.VeriFuzz.Internal.Gen where

import           Data.Graph.Inductive (Graph, Node)
import qualified Data.Graph.Inductive as G
import qualified Data.Text            as T

fromNode :: Int -> T.Text
fromNode node = T.pack $ "w" <> show node

filterGr :: (Graph gr) => gr n e -> (Node -> Bool) -> [Node]
filterGr graph f =
  filter f $ G.nodes graph

only :: (Graph gr) => gr n e -> (gr n e -> Node -> Int) -> (gr n e -> Node -> Int) -> Node -> Bool
only graph fun1 fun2 n = fun1 graph n == 0 && fun2 graph n /= 0

inputs :: (Graph gr) => gr n e -> [Node]
inputs graph = filterGr graph $ only graph G.indeg G.outdeg

outputs :: (Graph gr) => gr n e -> [Node]
outputs graph = filterGr graph $ only graph G.outdeg G.indeg
