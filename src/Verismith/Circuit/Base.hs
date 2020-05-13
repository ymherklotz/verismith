-- |
-- Module      : Verismith.Circuit.Base
-- Description : Base types for the circuit module.
-- Copyright   : (c) 2019, Yann Herklotz Grave
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Base types for the circuit module.
module Verismith.Circuit.Base
  ( Gate (..),
    Circuit (..),
    CNode (..),
    CEdge (..),
  )
where

import Data.Graph.Inductive (Gr, LEdge, LNode)
import System.Random

-- | The types for all the gates.
data Gate
  = And
  | Or
  | Xor
  deriving (Show, Eq, Enum, Bounded, Ord)

-- | Newtype for the Circuit which implements a Graph from fgl.
newtype Circuit = Circuit {getCircuit :: Gr Gate ()}

-- | Newtype for a node in the circuit, which is an 'LNode Gate'.
newtype CNode = CNode {getCNode :: LNode Gate}

-- | Newtype for a named edge which is empty, as it does not need a label.
newtype CEdge = CEdge {getCEdge :: LEdge ()}

instance Random Gate where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')

  random = randomR (minBound, maxBound)
