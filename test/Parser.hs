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
    )
where

import           Data.Either             (either, isRight)
import           Hedgehog                (Gen, Property, (===))
import qualified Hedgehog                as Hog
import qualified Hedgehog.Gen            as Hog
import           Test.Tasty
import           Test.Tasty.Hedgehog
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
parserTests = testGroup "Parser tests"
    [ testProperty "Input" parserInput
    , testProperty "Idempotence" parserIdempotent
    ]
