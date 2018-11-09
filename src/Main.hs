module Main where

import Data.Bits
import Test.QuickCheck hiding ((.&.))
import Data.GraphViz
import Data.Graph.Inductive.Example (clr479, dag4)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz.Attributes.Complete
import Data.Text.Lazy
import Data.GraphViz.Commands
import System.Random.MWC

import Test.VeriFuzz.Graph.Random
import Test.VeriFuzz.Types

instance Labellable Gate where
  toLabelValue gate = StrLabel . pack $ show gate

main :: IO FilePath
--main = sample (arbitrary :: Gen (Circuit Input))
main = do
  gr <- (randomDAG 100 :: IO (Gr Gate ()))
  runGraphviz (graphToDot quickParams $ emap (\_ -> "") gr) Png "output.png"
