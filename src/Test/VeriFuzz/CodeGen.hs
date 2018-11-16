{-# LANGUAGE OverloadedStrings #-}

module Test.VeriFuzz.CodeGen where

import           Data.Graph.Inductive (Graph, LNode, Node, indeg, nodes, outdeg,
                                       pre)
import           Data.Text            (Text, empty, pack)
import           Test.VeriFuzz.Types

fromNode :: Node -> Text
fromNode node = pack $ "w" <> show node

filterGr :: (Graph gr) => gr n e -> (Node -> Bool) -> [Node]
filterGr graph f =
  filter f $ nodes graph

fromList :: [Text] -> Text
fromList = foldl mappend empty

toOperator :: Gate -> Text
toOperator And = " & "
toOperator Or  = " | "
toOperator Xor = " ^ "

toStatement :: (Graph gr) => gr Gate e -> LNode Gate -> Text
toStatement graph (n, g) =
  fromNode n <> " = " <> connNodes <> ";\n"
  where
    connNodes = fromList . map ((<> toOperator g) . fromNode) $ pre graph n

generate :: (Graph gr) => gr Gate e -> Text
generate graph =
  "module generated_module(\n"
  <> fromList (imap "  input wire " ",\n" inp)
  <> fromList (imap "  output wire " ",\n" out)
  <> ");\n"
  <> "endmodule\n\nmodule main;\n  initial\n    begin\n      "
  <> "$display(\"Hello, world\");\n      $finish;\n    "
  <> "end\nendmodule"
  where
    zero fun = (==0) . fun graph
    inp = filterGr graph $ zero indeg
    out = filterGr graph $ zero outdeg
    imap b e = map ((\s -> b <> s <> e) . fromNode)
