module Main where

import Property
import Test.Tasty
import Unit

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

main :: IO ()
main = defaultMain tests
