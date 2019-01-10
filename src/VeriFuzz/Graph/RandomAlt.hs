{-|p
Module      : VeriFuzz.Graph.RandomAlt
Description : RandomAlt generation for DAG
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Define the random generation for the directed acyclic graph.
-}

module VeriFuzz.Graph.RandomAlt where

import qualified Data.Graph.Inductive.Arbitrary    as G
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Test.QuickCheck                   (Arbitrary, Gen)
import qualified Test.QuickCheck                   as QC

randomDAG :: (Arbitrary l, Arbitrary e)
             => Gen (Gr l e)
randomDAG =
  G.looplessGraph <$> QC.arbitrary

-- | Generate a random acyclic DAG with an IO instance.
genRandomDAG :: (Arbitrary l, Arbitrary e)
             => IO (Gr l e)
genRandomDAG = QC.generate randomDAG
