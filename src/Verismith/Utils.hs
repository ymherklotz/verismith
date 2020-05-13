-- |
-- Module      : Verismith
-- Description : Verismith
-- Copyright   : (c) 2020, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
module Verismith.Utils
  ( generateByteString,
  )
where

import Data.ByteString (ByteString, pack)
import System.Random (mkStdGen, newStdGen, randoms)

generateByteString :: (Maybe Int) -> Int -> Int -> IO [ByteString]
generateByteString mseed size n = do
  fmap pack . chunksOf size . take (size * n) . randoms
    <$> case mseed of
      Just seed' -> return $ mkStdGen seed'
      Nothing -> newStdGen
  where
    chunksOf i _ | i <= 0 = error $ "chunksOf, number must be positive, got " ++ show i
    chunksOf i xs = repeatedly (splitAt i) xs
    repeatedly _ [] = []
    repeatedly f as = b : repeatedly f as'
      where
        (b, as') = f as
