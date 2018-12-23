module Main where

import           Property
import           Test.Tasty
import           Test.VeriFuzz
import           Unit

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

main = defaultMain tests
