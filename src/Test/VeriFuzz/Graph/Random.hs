module Test.VeriFuzz.Graph.Random
  ( randomDAG
  ) where

import Data.Graph.Inductive
import Test.QuickCheck

randomDAG :: (Arbitrary a)
          => GenIO    -- ^ The random number generator to use
          -> Int      -- ^ The number of nodes
          -> IO (Gr (LNode a) e) -- ^ The generated graph. It uses Arbitrary to
                      -- generate random instances of each node
randomDAG = do
