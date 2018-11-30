module Main where

import           Data.Graph.Inductive
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.Text.IO                      as T
import           Data.Text.Lazy

import           Test.VeriFuzz

instance Labellable Gate where
  toLabelValue gate = StrLabel . pack $ show gate

main :: IO ()
--main = sample (arbitrary :: Gen (Circuit Input))
main = do
  gr <- genRandomDAG 100 :: IO (Gr Gate ())
--  _ <- runGraphviz (graphToDot quickParams $ emap (const "") gr) Png "output.png"
  T.putStrLn $ generate gr
