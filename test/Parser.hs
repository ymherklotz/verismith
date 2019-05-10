{-|
Module      : Parser
Description : Test the parser.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Test the parser.
-}

module Parser
    ( parserTests
    , parseUnitTests
    )
where

import           Data.Either             (either, isRight)
import           Hedgehog                (Gen, Property, (===))
import qualified Hedgehog                as Hog
import qualified Hedgehog.Gen            as Hog
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit
import           Text.Parsec
import           VeriFuzz
import           VeriFuzz.Verilog.Lex
import           VeriFuzz.Verilog.Parser

randomMod' :: Gen ModDecl
randomMod' = Hog.resize 20 (randomMod 3 10)

parserInput :: Property
parserInput = Hog.property $ do
    v <- Hog.forAll randomMod'
    Hog.assert . isRight $ parse parseModDecl
                                 "input_test.v"
                                 (alexScanTokens $ str v)
    where str = show . GenVerilog

parserIdempotent :: Property
parserIdempotent = Hog.property $ do
    v <- Hog.forAll randomMod'
    let sv = vshow v
    p sv === (p . p) sv
  where
    vshow = show . GenVerilog
    p sv =
        either (\x -> show x <> "\n" <> sv) vshow
            . parse parseModDecl "idempotent_test.v"
            $ alexScanTokens sv

parserTests :: TestTree
parserTests = testGroup "Parser properties"
    [ testProperty "Input" parserInput
    , testProperty "Idempotence" parserIdempotent
    ]

testParse :: (Eq a, Show a) => Parser a -> String -> String -> a -> TestTree
testParse p name input golden = testCase name $
    case parse p "testcase" (alexScanTokens $ input) of
        Left e       -> assertFailure $ show e
        Right result -> golden @=? result

testParseFail :: (Eq a, Show a) => Parser a -> String -> String -> TestTree
testParseFail p name input = testCase name $
    case parse p "testcase" (alexScanTokens $ input) of
        Left _  -> return ()
        Right _ -> assertFailure "Parse incorrectly succeeded"

parseEventUnit :: TestTree
parseEventUnit =
    testGroup "Event"
    [ testFailure "No empty event" "@()"
    , test "@*" EAll
    , test "@(*)" EAll
    , test "@(posedge clk)" $ EPosEdge "clk"
    , test "@(negedge clk)" $ ENegEdge "clk"
    , test "@(wire1)" $ EId "wire1"
    , test "@(a or b or c or d)" $ EOr (EId "a") (EOr (EId "b") (EOr (EId "c") (EId "d")))
    , test "@(a, b, c, d)" $ EComb (EId "a") (EComb (EId "b") (EComb (EId "c") (EId "d")))
    ]
    where
        test a = testParse parseEvent ("Test " <> a) a
        testFailure = testParseFail parseEvent

parseUnitTests :: TestTree
parseUnitTests =
    testGroup "Parser unit"
    [ parseEventUnit
    ]
