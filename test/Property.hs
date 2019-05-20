{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Property
    ( propertyTests
    )
where

import           Data.Either             (either, isRight)
import qualified Data.Graph.Inductive    as G
import           Data.Text               (Text)
import           Hedgehog                (Gen, Property, (===))
import qualified Hedgehog                as Hog
import           Hedgehog.Function       (Arg, Vary)
import qualified Hedgehog.Function       as Hog
import qualified Hedgehog.Gen            as Hog
import qualified Hedgehog.Range          as Hog
import           Parser                  (parserTests)
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Text.Parsec
import           VeriFuzz
import           VeriFuzz.Result
import           VeriFuzz.Verilog.Lex
import           VeriFuzz.Verilog.Parser

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

type GenFunctor f a b c =
    ( Functor f
    , Show (f a)
    , Show a, Arg a, Vary a
    , Show b, Arg b, Vary b
    , Show c
    , Eq (f c)
    , Show (f c)
    )

mapCompose
    :: forall f a b c
     . GenFunctor f a b c
    => (forall x . Gen x -> Gen (f x))
    -> Gen a
    -> Gen b
    -> Gen c
    -> Property
mapCompose genF genA genB genC = Hog.property $ do
    g  <- Hog.forAllFn $ Hog.fn @a genB
    f  <- Hog.forAllFn $ Hog.fn @b genC
    xs <- Hog.forAll $ genF genA
    fmap (f . g) xs === fmap f (fmap g xs)

propertyResultInterrupted :: Property
propertyResultInterrupted = do
    mapCompose genResult
               (Hog.int (Hog.linear 0 100))
               (Hog.int (Hog.linear 0 100))
               (Hog.int (Hog.linear 0 100))
  where
    genResult :: Gen a -> Gen (Result Text a)
    genResult a = Hog.choice
        [Pass <$> a, Fail <$> Hog.text (Hog.linear 1 100) Hog.unicode]

propertyTests :: TestTree
propertyTests = testGroup
    "Property Tests"
    [ testProperty "acyclic graph generation check" acyclicGraph
    , testProperty "fmap for Result"                propertyResultInterrupted
    , parserTests
    ]
