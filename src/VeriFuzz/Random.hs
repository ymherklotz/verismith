{-|
Module      : VeriFuzz.Random
Description : Random generation for DAG
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Define the random generation for the directed acyclic graph.
-}

module VeriFuzz.Random where

import           Data.Graph.Inductive              (Context, LEdge)
import qualified Data.Graph.Inductive              as G
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.List                         (nub)
import           Test.QuickCheck                   (Arbitrary, Gen)
import qualified Test.QuickCheck                   as QC
import           VeriFuzz.Circuit

dupFolder :: (Eq a, Eq b) => Context a b -> [Context a b] -> [Context a b]
dupFolder cont ns = unique cont : ns
    where unique (a, b, c, d) = (nub a, b, c, nub d)

-- | Remove duplicates.
rDups :: (Eq a, Eq b) => Gr a b -> Gr a b
rDups g = G.buildGr $ G.ufold dupFolder [] g

-- | Remove duplicates.
rDupsCirc :: Circuit -> Circuit
rDupsCirc = Circuit . rDups . getCircuit

-- | Gen instance to create an arbitrary edge, where the edges are limited by
-- `n` that is passed to it.
arbitraryEdge :: (Arbitrary e) => Int -> Gen (LEdge e)
arbitraryEdge n = do
    x <- with $ \a -> a < n && a > 0 && a /= n - 1
    y <- with $ \a -> x < a && a < n && a > 0
    z <- QC.arbitrary
    return (x, y, z)
    where with = QC.suchThat $ QC.resize n QC.arbitrary

-- | Gen instance for a random acyclic DAG.
randomDAG :: (Arbitrary l, Arbitrary e, Eq l, Eq e) => Gen (Gr l e) -- ^ The generated graph. It uses Arbitrary to
                          -- generate random instances of each node
randomDAG = do
    list <- QC.infiniteListOf QC.arbitrary
    l    <- QC.infiniteListOf aE
    QC.sized (\n -> return . G.mkGraph (nodes list n) $ take (10 * n) l)
  where
    nodes l n = zip [0 .. n] $ take n l
    aE = QC.sized arbitraryEdge

-- | Generate a random acyclic DAG with an IO instance.
genRandomDAG :: (Arbitrary l, Arbitrary e, Eq l, Eq e) => IO (Gr l e)
genRandomDAG = QC.generate randomDAG

-- | Generate a random circuit instead of a random graph
randomCircuit :: Gen Circuit
randomCircuit = Circuit <$> randomDAG
