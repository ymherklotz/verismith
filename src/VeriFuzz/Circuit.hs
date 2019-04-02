{-|
Module      : VeriFuzz.Circuit
Description : Definition of the circuit graph.
Copyright   : (c) 2018-2019, Yann Herklotz
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Definition of the circuit graph.
-}

module VeriFuzz.Circuit
    ( -- * Circuit
      Gate(..)
    , Circuit(..)
    , CNode(..)
    , CEdge(..)
    )
where

import           Data.Graph.Inductive (Gr, LEdge, LNode)
import           System.Random

-- | The types for all the gates.
data Gate = And
          | Or
          | Xor
          deriving (Show, Eq, Enum, Bounded, Ord)

-- | Newtype for the Circuit which implements a Graph from fgl.
newtype Circuit = Circuit { getCircuit :: Gr Gate () }

newtype CNode = CNode { getCNode :: LNode Gate }

newtype CEdge = CEdge { getCEdge :: LEdge () }

instance Random Gate where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')

  random = randomR (minBound, maxBound)
