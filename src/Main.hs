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
import Data.Graph.Generators.Random.WattsStrogatz
import System.Random.MWC
import Data.Graph.Generators.FGL
import Data.Graph.Generators

type Input = Bool

data Gate = Nand
          | And
          | Or
          deriving (Show, Eq, Ord)

instance Labellable Gate where
  toLabelValue gate = StrLabel . pack $ show gate

instance Arbitrary Gate where
  arbitrary = elements [Nand, And, Or]

randomTree :: Gr Gate String
randomTree = mkGraph [(1, Nand), (2, Nand), (3, Or), (4, Nand), (5, Nand), (6, Nand), (7, Or)] [(3, 1, ""), (7, 1, ""), (5, 1, ""), (6, 2, ""), (7, 2, ""), (5, 2, ""), (1, 4, ""), (2, 4, ""), (3, 4, ""), (6, 4, "")]

main :: IO FilePath
--main = sample (arbitrary :: Gen (Circuit Input))
main = do
  gen <- withSystemRandom . asGenIO $ return
  gr <- wattsStrogatzGraph gen 50 3 0.6
  runGraphviz (graphToDot nonClusteredParams (graphInfoToUGr gr)) Png "output.png"
