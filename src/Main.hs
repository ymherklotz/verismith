module Main where

import Data.Bits
import Test.QuickCheck hiding ((.&.))
import Data.GraphViz
import Data.Graph.Inductive.Example (clr479, dag4)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

type Input = Bool

data Gate = Nand
          deriving (Show)

data Circuit a = In a
               | Node Gate (Circuit a) (Circuit a)
               deriving (Show)

instance Arbitrary Gate where
  arbitrary = return Nand

instance (Arbitrary a) => Arbitrary (Circuit a) where
  arbitrary = do
    x <- arbitrary
    frequency [(1, return (In x)), (1, arbNode)]
      where
        arbNode = Node <$> arbitrary <*> arbitrary <*> arbitrary

eval :: (Bits a) => Circuit a -> a
eval (In val) = val
eval (Node Nand c1 c2) = complement $ eval c1 .&. eval c2

visualize :: (Show a) => Circuit a -> Gr String String
visualize circ =
  graph Nothing 0 (empty :: Gr String String) circ
  where
    graph parent nl graph circ =
      let newNode str graph = (head $ newNodes 1 graph, str) in
      case (parent, circ) of
        (Nothing, (In val)) ->
          insNode (newNode "IN" graph) graph
        _ ->
          graph

main :: IO ()
--main = sample (arbitrary :: Gen (Circuit Input))
main = preview $ visualize (In True)
