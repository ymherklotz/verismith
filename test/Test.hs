module Main where

import           Property
import           Test.Tasty
import           Unit
import           VeriFuzz

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

main = defaultMain tests
