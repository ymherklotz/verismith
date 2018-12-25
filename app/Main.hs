{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Graph.Inductive              as G
import qualified Data.GraphViz                     as Gviz
import qualified Data.GraphViz.Attributes.Complete as Gviz
import qualified Data.Text.IO                      as T
import qualified Data.Text.Lazy                    as T
import qualified Test.QuickCheck                   as QC
import           Test.VeriFuzz

instance Gviz.Labellable Gate where
  toLabelValue gate = Gviz.StrLabel . T.pack $ show gate

main :: IO ()
--main = sample (arbitrary :: Gen (Circuit Input))
main = do
  gr <- genRandomDAG 100 :: IO (G.Gr Gate ())
--  _ <- runGraphviz (graphToDot quickParams $ emap (const "") gr) Png "output.png",
--  T.putStrLn $ generate gr
  --g <- QC.generate (QC.arbitrary :: QC.Gen SourceText)
  render . genSourceText . addTestBench . nestUpTo 20 . generateAST $ Circuit gr
