{-|
Module      : Parser
Description : Test the parser.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

Test the parser.
-}

module Parser
    ( parserTests
    , parseUnitTests
    )
where

import           Control.Lens
import           Data.Either             (either, isRight)
import           Hedgehog                (Gen, Property, (===))
import qualified Hedgehog                as Hog
import qualified Hedgehog.Gen            as Hog
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit
import           Text.Parsec
import           VeriFuzz
import           VeriFuzz.Internal
import           VeriFuzz.Verilog.Lex
import           VeriFuzz.Verilog.Parser

smallConfig :: Config
smallConfig = defaultConfig & configProperty . propSize .~ 5

randomMod' :: Gen ModDecl
randomMod' = Hog.resize 20 (randomMod 3 10)

parserInputMod :: Property
parserInputMod = Hog.property $ do
    v <- Hog.forAll randomMod'
    Hog.assert . isRight $ parse parseModDecl
                                 "input_test_mod"
                                 (alexScanTokens $ str v)
    where str = show . GenVerilog

parserIdempotentMod :: Property
parserIdempotentMod = Hog.property $ do
    v <- Hog.forAll randomMod'
    let sv = vshow v
    p sv === (p . p) sv
  where
    vshow = show . GenVerilog
    p sv =
        either (\x -> show x <> "\n" <> sv) vshow
            . parse parseModDecl "idempotent_test_mod"
            $ alexScanTokens sv

parserInput :: Property
parserInput = Hog.property $ do
    v <- Hog.forAll (procedural "top" smallConfig)
    Hog.assert . isRight $ parse parseModDecl
                                 "input_test"
                                 (alexScanTokens $ str v)
    where str = show . GenVerilog

parserIdempotent :: Property
parserIdempotent = Hog.property $ do
    v <- Hog.forAll (procedural "top" smallConfig)
    let sv = vshow v
    p sv === (p . p) sv
  where
    vshow = showT . GenVerilog
    p sv = either (\x -> showT x <> "\n" <> sv) vshow
        $ parseVerilog "idempotent_test" sv

parserTests :: TestTree
parserTests = testGroup
    "Parser properties"
    [ testProperty "Input Mod"       parserInputMod
    , testProperty "Input"           parserInput
    , testProperty "Idempotence Mod" parserIdempotentMod
    , testProperty "Idempotence"     parserIdempotent
    ]

testParse :: (Eq a, Show a) => Parser a -> String -> String -> a -> TestTree
testParse p name input golden =
    testCase name $ case parse p "testcase" (alexScanTokens input) of
        Left  e      -> assertFailure $ show e
        Right result -> golden @=? result

testParseFail :: (Eq a, Show a) => Parser a -> String -> String -> TestTree
testParseFail p name input =
    testCase name $ case parse p "testcase" (alexScanTokens input) of
        Left  _ -> return ()
        Right _ -> assertFailure "Parse incorrectly succeeded"

parseEventUnit :: TestTree
parseEventUnit = testGroup
    "Event"
    [ testFailure "No empty event" "@()"
    , test "@*"   EAll
    , test "@(*)" EAll
    , test "@(posedge clk)" $ EPosEdge "clk"
    , test "@(negedge clk)" $ ENegEdge "clk"
    , test "@(wire1)" $ EId "wire1"
    , test "@(a or b or c or d)"
        $ EOr (EId "a") (EOr (EId "b") (EOr (EId "c") (EId "d")))
    , test "@(a, b, c, d)"
        $ EComb (EId "a") (EComb (EId "b") (EComb (EId "c") (EId "d")))
    , test "@(posedge a or negedge b or c or d)"
        $ EOr (EPosEdge "a") (EOr (ENegEdge "b") (EOr (EId "c") (EId "d")))
    ]
  where
    test a = testParse parseEvent ("Test " <> a) a
    testFailure = testParseFail parseEvent

parseAlwaysUnit :: TestTree
parseAlwaysUnit = testGroup
    "Always"
    [ test "Empty" "always begin end" $ Always (SeqBlock [])
    , test "Empty with event @*"             "always @* begin end"
        $ Always (EventCtrl EAll (Just (SeqBlock [])))
    , test "Empty with event @(posedge clk)" "always @(posedge clk) begin end"
        $ Always (EventCtrl (EPosEdge "clk") (Just (SeqBlock [])))
    ]
    where test = testParse parseModItem

parseUnitTests :: TestTree
parseUnitTests = testGroup "Parser unit" [parseEventUnit, parseAlwaysUnit]
