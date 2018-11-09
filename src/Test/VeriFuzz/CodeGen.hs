{-# LANGUAGE OverloadedStrings #-}

module Test.VeriFuzz.CodeGen where

import Data.Text (Text, empty, pack)
import Data.Graph.Inductive (Graph, Node, indeg, outdeg, nodes)

import Test.VeriFuzz.Types

fromNode :: Node -> Text
fromNode node = pack $ "w" <> show node

filterGr :: (Graph gr) => gr n e -> (Node -> Bool) -> [Node]
filterGr graph f =
  filter f $ nodes graph

generate :: (Graph gr) => gr Gate e -> Text
generate graph =
  "module generated_module(\n"
  <> fromList (imap "  " ",\n" inp)
  <> fromList (imap "  " ",\n" out)
  <> ");\n"
  <> fromList (imap "  input wire " ";\n" inp)
  <> fromList (imap "  output wire " ";\n" out)
  <> "endmodule\n"
  where
    and a b c = a == b && a /= c
    inputs n = indeg graph n == 0 && outdeg graph n /= 0
    outputs n = indeg graph n /= 0 && outdeg graph n == 0
    fromList = foldl mappend empty
    inp = filterGr graph inputs
    out = filterGr graph outputs
    imap b e = map ((\s -> b <> s <> e) . fromNode)
