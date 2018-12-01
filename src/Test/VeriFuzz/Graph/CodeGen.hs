{-# LANGUAGE OverloadedStrings #-}

module Test.VeriFuzz.Graph.CodeGen where

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

fromList :: (Foldable t, Monoid a) => t a -> a
fromList = foldl mappend mempty

safe :: ([a] -> b) -> [a] -> Maybe b
safe _ [] = Nothing
safe f l  = Just $ f l

toOperator :: Gate -> Text
toOperator And = " & "
toOperator Or  = " | "
toOperator Xor = " ^ "

sep :: (Monoid a) => a -> [a] -> a
sep el l = fromMaybe mempty $
  (fromList . fmap (<>el) <$> safe init l) <> safe last l

statList :: Gate -> [Node] -> Maybe Text
statList g n = toStr <$> safe tail n
  where
    toStr = fromList . fmap ((<> toOperator g) . fromNode)

lastEl :: [Node] -> Maybe Text
lastEl n = fromNode <$> safe head n

toStatement :: (Graph gr) => gr Gate e -> LNode Gate -> Text
toStatement graph (n, g) =
  fromMaybe empty $ Just "  assign " <> Just (fromNode n)
  <> Just " = " <> statList g nodeL <> lastEl nodeL <> Just ";\n"
  where
    nodeL = pre graph n

generate :: (Graph gr) => gr Gate e -> Text
generate graph =
  "module generated_module(\n"
  <> fromList (imap "  input wire " ",\n" inp)
  <> sep ",\n" (imap "  output wire " "" out)
  <> ");\n"
  <> fromList (toStatement graph <$> labNodes graph)
  <> "endmodule\n\nmodule main;\n  initial\n    begin\n      "
  <> "$display(\"Hello, world\");\n      $finish;\n    "
  <> "end\nendmodule"
  where
    zero fun1 fun2 n = fun1 graph n == 0 && fun2 graph n /= 0
    inp = filterGr graph $ zero indeg outdeg
    out = filterGr graph $ zero outdeg indeg
    imap b e = fmap ((\s -> b <> s <> e) . fromNode)
