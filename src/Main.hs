module Main where

import Data.GraphViz
import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Commands
import Data.Text.Lazy
import Data.Text.IO as T

import Test.VeriFuzz

instance Labellable Gate where
  toLabelValue gate = StrLabel . pack $ show gate

main :: IO ()
--main = sample (arbitrary :: Gen (Circuit Input))
main = do
  gr <- (randomDAG 100 :: IO (Gr Gate ()))
  runGraphviz (graphToDot quickParams $ emap (\_ -> "") gr) Png "output.png"
  T.putStrLn $ generate gr
