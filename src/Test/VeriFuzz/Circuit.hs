{-|
Module      : Test.VeriFuzz.Circuit
Description : Definition of the circuit graph.
Copyright   : (c) Yann Herklotz Grave 2018
License     : GPL-3
Maintainer  : ymherklotz@gmail.com
Stability   : experimental
Portability : POSIX

Definition of the circuit graph.
-}

module Test.VeriFuzz.Circuit where

import           Data.Graph.Inductive
import           System.Random
import           Test.QuickCheck

-- | The types for all the gates.
data Gate = And
          | Or
          | Xor
          deriving (Show, Eq, Enum, Bounded, Ord)

-- | Newtype for the Circuit which implements a Graph from fgl.
newtype Circuit = Circuit { getCircuit :: Gr Gate () }

instance Random Gate where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')

  random = randomR (minBound, maxBound)

instance Arbitrary Gate where
  arbitrary = elements [And, Or, Xor]
