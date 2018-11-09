module Test.VeriFuzz.Types where

import Test.QuickCheck
import System.Random

data Gate = And
          | Or
          | Xor
          | Nor
          | Nand
          deriving (Show, Eq, Enum, Bounded)

instance Random Gate where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')

  random g = randomR (minBound, maxBound) g

instance Arbitrary Gate where
  arbitrary = elements [And, Or, Xor, Nor, Nand]
