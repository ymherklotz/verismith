module Property
    ( propertyTests
    )
where

import           Data.Either          (fromRight, isRight)
import qualified Data.Graph.Inductive as G
import           Hedgehog             (Gen, (===))
import qualified Hedgehog             as Hog
import qualified Hedgehog.Gen         as Hog
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Text.Parsec
import           VeriFuzz
import           VeriFuzz.Parser.Lex

randomMod' :: Gen ModDecl
randomMod' = Hog.resize 20 (randomMod 3 10)

randomDAG' :: Gen Circuit
randomDAG' = Hog.resize 30 randomDAG

simpleGraph :: TestTree
simpleGraph = testProperty "simple graph generation check" . Hog.property $ do
    xs <- Hog.forAllWith (const "") randomDAG'
    Hog.assert $ simp xs
    where simp = G.isSimple . getCircuit

parserInput' :: Hog.Property
parserInput' = Hog.property $ do
    v <- Hog.forAll randomMod'
    Hog.assert . isRight $ parse parseModDecl
                                 "input_test.v"
                                 (alexScanTokens $ str v)
    where str = show . GenVerilog

parserIdempotent' :: Hog.Property
parserIdempotent' = Hog.property $ do
    v <- Hog.forAll randomMod'
    let sv = vshow v
    p sv === (p . p) sv
  where
    vshow = show . GenVerilog
    p =
        vshow
            . fromRight (error "Failed idempotent test")
            . parse parseModDecl "idempotent_test.v"
            . alexScanTokens

parserInput :: TestTree
parserInput = testProperty "parser input" parserInput'

parserIdempotent :: TestTree
parserIdempotent = testProperty "parser idempotence" parserIdempotent'

propertyTests :: TestTree
propertyTests =
    testGroup "Property Tests" [simpleGraph, parserInput, parserIdempotent]
