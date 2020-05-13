-- |
-- Module      : Verismith.Circuit.Internal
-- Description : Internal helpers for generation.
-- Copyright   : (c) 2018-2019, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Internal helpers for generation.
module Verismith.Circuit.Internal
  ( fromNode,
    filterGr,
    only,
    inputs,
    outputs,
  )
where

import Data.Graph.Inductive (Graph, Node)
import qualified Data.Graph.Inductive as G
import qualified Data.Text as T

-- | Convert an integer into a label.
--
-- >>> fromNode 5
-- "w5"
fromNode :: Int -> T.Text
fromNode node = T.pack $ "w" <> show node

-- | General function which runs 'filter' over a graph.
filterGr :: (Graph gr) => gr n e -> (Node -> Bool) -> [Node]
filterGr graph f = filter f $ G.nodes graph

-- | Takes two functions that return an 'Int', and compares there results to 0
-- and not 0 respectively. This result is returned.
only ::
  (Graph gr) =>
  gr n e ->
  (gr n e -> Node -> Int) ->
  (gr n e -> Node -> Int) ->
  Node ->
  Bool
only graph fun1 fun2 n = fun1 graph n == 0 && fun2 graph n /= 0

-- | Returns all the input nodes to a graph, which means nodes that do not have
-- an input themselves.
inputs :: (Graph gr) => gr n e -> [Node]
inputs graph = filterGr graph $ only graph G.indeg G.outdeg

-- | Returns all the output nodes to a graph, similar to the 'inputs' function.
outputs :: (Graph gr) => gr n e -> [Node]
outputs graph = filterGr graph $ only graph G.outdeg G.indeg
