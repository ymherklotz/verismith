{-|
Module      : VeriFuzz.Graph.Random
Description : Code generation directly from DAG.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Define the code generation directly from the random DAG.
-}

module VeriFuzz.Graph.CodeGen
  ( generate
  )
where

import           Data.Foldable                  ( fold )
import           Data.Graph.Inductive           ( Graph
                                                , LNode
                                                , Node
                                                , labNodes
                                                , pre
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           VeriFuzz.Circuit
import           VeriFuzz.Internal.Gen
import           VeriFuzz.Internal.Shared

toOperator :: Gate -> Text
toOperator And = " & "
toOperator Or  = " | "
toOperator Xor = " ^ "

statList :: Gate -> [Node] -> Maybe Text
statList g n = toStr <$> safe tail n where toStr = fold . fmap ((<> toOperator g) . fromNode)

lastEl :: [Node] -> Maybe Text
lastEl n = fromNode <$> safe head n

toStmnt :: (Graph gr) => gr Gate e -> LNode Gate -> Text
toStmnt graph (n, g) =
  fromMaybe T.empty
    $  Just "  assign "
    <> Just (fromNode n)
    <> Just " = "
    <> statList g nodeL
    <> lastEl nodeL
    <> Just ";\n"
  where nodeL = pre graph n

generate :: (Graph gr) => gr Gate e -> Text
generate graph =
  "module generated_module(\n"
    <> fold (imap "  input wire " ",\n" inp)
    <> T.intercalate ",\n" (imap "  output wire " "" out)
    <> ");\n"
    <> fold (toStmnt graph <$> labNodes graph)
    <> "endmodule\n\nmodule main;\n  initial\n    begin\n      "
    <> "$display(\"Hello, world\");\n      $finish;\n    "
    <> "end\nendmodule"
 where
  inp = inputs graph
  out = outputs graph
  imap b e = fmap ((\s -> b <> s <> e) . fromNode)
