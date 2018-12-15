{-|
Module      : Test.VeriFuzz.Graph.Random
Description : Random generation for DAG
Copyright   : (c) Yann Herklotz Grave 2018
License     : GPL-3
Maintainer  : ymherklotz@gmail.com
Stability   : experimental
Portability : POSIX

Define the random generation for the directed acyclic graph.
-}

module Test.VeriFuzz.Graph.Random where

import           Data.Graph.Inductive (Graph, LEdge, mkGraph)
import           Test.QuickCheck      (Arbitrary, Gen, arbitrary, generate,
                                       infiniteListOf, resize, suchThat)

-- | Gen instance to create an arbitrary edge, where the edges are limited by
-- `n` that is passed to it.
arbitraryEdge :: (Arbitrary e) => Int -> Gen (LEdge e)
arbitraryEdge n = do
  x <- with $ \a -> a < n && a > 0 && a /= n-1
  y <- with $ \a -> x < a && a < n && a > 0
  z <- arbitrary
  return (x, y, z)
    where
      with = suchThat . resize n $ arbitrary

-- | Gen instance for a random acyclic DAG.
randomDAG :: (Arbitrary l, Arbitrary e, Graph gr)
          => Int          -- ^ The number of nodes
          -> Gen (gr l e) -- ^ The generated graph. It uses Arbitrary to
                          -- generate random instances of each node
randomDAG n = do
  list <- infiniteListOf arbitrary
  l <- infiniteListOf $ arbitraryEdge n
  return . mkGraph (nodes list) $ take (10*n) l
    where
      nodes l = zip [0..n] $ take n l

-- | Generate a random acyclic DAG with an IO instance.
genRandomDAG :: (Arbitrary l, Arbitrary e, Graph gr)
             => Int
             -> IO (gr l e)
genRandomDAG = generate . randomDAG
