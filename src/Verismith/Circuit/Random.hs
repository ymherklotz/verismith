-- |
-- Module      : Verismith.Circuit.Random
-- Description : Random generation for DAG
-- Copyright   : (c) 2018-2019, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Define the random generation for the directed acyclic graph.
module Verismith.Circuit.Random
  ( rDups,
    rDupsCirc,
    randomDAG,
    genRandomDAG,
  )
where

import Data.Graph.Inductive (Context)
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (nub)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Hog
import qualified Hedgehog.Range as Hog
import Verismith.Circuit.Base

dupFolder :: (Eq a, Eq b) => Context a b -> [Context a b] -> [Context a b]
dupFolder cont ns = unique cont : ns
  where
    unique (a, b, c, d) = (nub a, b, c, nub d)

-- | Remove duplicates.
rDups :: (Eq a, Eq b) => Gr a b -> Gr a b
rDups g = G.buildGr $ G.ufold dupFolder [] g

-- | Remove duplicates.
rDupsCirc :: Circuit -> Circuit
rDupsCirc = Circuit . rDups . getCircuit

-- | Gen instance to create an arbitrary edge, where the edges are limited by
-- `n` that is passed to it.
arbitraryEdge :: Hog.Size -> Gen CEdge
arbitraryEdge n = do
  x <- with $ \a -> a < n && a > 0 && a /= n - 1
  y <- with $ \a -> x < a && a < n && a > 0
  return $ CEdge (fromIntegral x, fromIntegral y, ())
  where
    with =
      flip Hog.filter $
        fromIntegral
          <$> Hog.resize
            n
            (Hog.int (Hog.linear 0 100))

-- | Gen instance for a random acyclic DAG.
randomDAG ::
  -- | The generated graph. It uses Arbitrary to generate
  -- random instances of each node
  Gen Circuit
randomDAG = do
  list <- Hog.list (Hog.linear 1 100) $ Hog.enum minBound maxBound
  l <- Hog.list (Hog.linear 10 1000) aE
  return . Circuit $ G.mkGraph (nodes list) l
  where
    nodes l = zip [0 .. length l - 1] l
    aE = getCEdge <$> Hog.sized arbitraryEdge

-- | Generate a random acyclic DAG with an IO instance.
genRandomDAG :: IO Circuit
genRandomDAG = Hog.sample randomDAG
