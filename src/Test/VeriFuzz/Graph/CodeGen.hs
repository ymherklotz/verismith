{-|
Module      : Test.VeriFuzz.Graph.Random
Description : Code generation directly from DAG.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Define the code generation directly from the random DAG.
-}

module Test.VeriFuzz.Graph.CodeGen
  ( generate
  ) where

import           Data.Graph.Inductive          (Graph, LNode, Node, indeg,
                                                labNodes, nodes, outdeg, pre)
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text, empty, pack)
import           Test.VeriFuzz.Circuit
import           Test.VeriFuzz.Internal.Gen
import           Test.VeriFuzz.Internal.Shared

toOperator :: Gate -> Text
toOperator And = " & "
toOperator Or  = " | "
toOperator Xor = " ^ "

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
    inp = inputs graph
    out = outputs graph
    imap b e = fmap ((\s -> b <> s <> e) . fromNode)
