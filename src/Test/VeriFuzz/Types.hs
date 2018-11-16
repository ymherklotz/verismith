module Test.VeriFuzz.Types where

import           System.Random
import           Test.QuickCheck

data Gate = And
          | Or
          | Xor
          deriving (Show, Eq, Enum, Bounded, Ord)

instance Random Gate where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')

  random = randomR (minBound, maxBound)

instance Arbitrary Gate where
  arbitrary = elements [And, Or, Xor]
