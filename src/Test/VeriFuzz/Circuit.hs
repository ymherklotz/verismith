{-|
Module      : Test.VeriFuzz.Circuit
Description : Definition of the circuit graph.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Definition of the circuit graph.
-}

module Test.VeriFuzz.Circuit where

import           Data.Graph.Inductive (Gr, LNode)
import           System.Random
import           Test.QuickCheck

-- | The types for all the gates.
data Gate = And
          | Or
          | Xor
          deriving (Show, Eq, Enum, Bounded, Ord)

-- | Newtype for the Circuit which implements a Graph from fgl.
newtype Circuit = Circuit { getCircuit :: Gr Gate () }

newtype CNode = CNode { getCNode :: LNode Gate }

instance Random Gate where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')

  random = randomR (minBound, maxBound)

instance Arbitrary Gate where
  arbitrary = elements [And, Or, Xor]
