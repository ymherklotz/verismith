{-# LANGUAGE OverloadedStrings #-}

module Test.VeriFuzz.CodeGen where

import           Data.Graph.Inductive (Graph, LNode, Node, indeg, labNodes,
                                       nodes, outdeg, pre)
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text, empty, pack)
import           Test.VeriFuzz.Types

fromNode :: Node -> Text
fromNode node = pack $ "w" <> show node

filterGr :: (Graph gr) => gr n e -> (Node -> Bool) -> [Node]
filterGr graph f =
  filter f $ nodes graph

fromList :: [Text] -> Text
fromList = foldl mappend empty

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail l  = Just $ tail l

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead l  = Just $ head l

toOperator :: Gate -> Text
toOperator And = " & "
toOperator Or  = " | "
toOperator Xor = " ^ "

statList :: Gate -> [Node] -> Maybe Text
statList g n = toStr <$> safeTail n
  where
    toStr = fromList . map ((<> toOperator g) . fromNode)

lastEl :: [Node] -> Maybe Text
lastEl n = fromNode <$> safeHead n

toStatement :: (Graph gr) => gr Gate e -> LNode Gate -> Text
toStatement graph (n, g) =
  fromMaybe empty $ Just "  assign " <> Just (fromNode n) <> Just " = " <> statList g nodeL <> lastEl nodeL <> Just ";\n"
  where
    nodeL = pre graph n

generate :: (Graph gr) => gr Gate e -> Text
generate graph =
  "module generated_module(\n"
  <> fromList (imap "  input wire " ",\n" inp)
  <> fromList (imap "  output wire " ",\n" out)
  <> ");\n"
  <> fromList (map (toStatement graph) (labNodes graph))
  <> "endmodule\n\nmodule main;\n  initial\n    begin\n      "
  <> "$display(\"Hello, world\");\n      $finish;\n    "
  <> "end\nendmodule"
  where
    zero fun = (==0) . fun graph
    inp = filterGr graph $ zero indeg
    out = filterGr graph $ zero outdeg
    imap b e = map ((\s -> b <> s <> e) . fromNode)
