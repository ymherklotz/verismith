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
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T
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

showBS :: ByteString -> Text
showBS = decodeUtf8 . L.toStrict . toLazyByteString . byteStringHex
