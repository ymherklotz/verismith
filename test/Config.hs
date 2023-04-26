-- |
-- Module      : Config
-- Description : Test the configuration parsing/printing.
-- Copyright   : (c) 2019, Yann Herklotz Grave
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Test the parser.
module Config
  ( configUnitTests,
  )
where

import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import Verismith
import Verismith.Config

testParseFailure :: String -> FilePath -> TestTree
testParseFailure name fp = testCase name $ do
  conf <- parseConfigFile fp
  case conf of
    Left _ -> return ()
    Right _ -> assertFailure "Configuration was incorrectly parsed"

testParse :: String -> FilePath -> Config -> TestTree
testParse name fp conf =
  testCase name $ do
    parsedConf <- parseConfigFile fp
    case parsedConf of
      Left err -> assertFailure $ T.unpack err
      Right parsedConf' ->
        assertEqual "Configuration different to expected" parsedConf' conf

configUnitTests :: TestTree
configUnitTests =
  testGroup
    "Config unit tests"
    [ testParse "Default configuration parsed" "test/data/default_config.toml" defaultConfig,
      testParseFailure "Additional fields not parsed" "test/data/additional.toml"
    ]
