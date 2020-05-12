{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Property
  ( propertyTests,
  )
where

import Data.Either (either, isRight)
import qualified Data.Graph.Inductive as G
import Data.Text (Text)
import Hedgehog ((===), Gen, Property)
import qualified Hedgehog as Hog
import qualified Hedgehog.Gen as Hog
import qualified Hedgehog.Range as Hog
import Parser (parserTests)
import Test.Tasty
import Test.Tasty.Hedgehog
import Text.Parsec
import Verismith
import Verismith.Result
import Verismith.Verilog.Lex
import Verismith.Verilog.Parser

randomDAG' :: Gen Circuit
randomDAG' = Hog.resize 30 randomDAG

acyclicGraph :: Property
acyclicGraph = Hog.property $ do
  xs <- Hog.forAllWith (const "") randomDAG'
  Hog.assert $ simp xs
  where
    simp g =
      (== G.noNodes (getCircuit g))
        . sum
        . fmap length
        . G.scc
        . getCircuit
        $ g

propertyTests :: TestTree
propertyTests =
  testGroup
    "Property Tests"
    [ testProperty "acyclic graph generation check" acyclicGraph,
      parserTests
    ]
