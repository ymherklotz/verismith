-- |
-- Module      : Verismith
-- Description : Verismith
-- Copyright   : (c) 2018-2023, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
module Verismith.Utils
  ( generateByteString,
    nonEmpty,
    foldrMap1,
    foldrMap1',
    foldrMapM1,
    mkpair,
    uncurry3,
    safe,
    showT,
    showBS,
    comma,
    commaNL,
  )
where

import Control.Applicative
import Data.ByteString (ByteString, pack)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Lazy as L
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import System.Random (mkStdGen, newStdGen, randoms)

-- List and nonempty list utils

nonEmpty :: b -> (NonEmpty a -> b) -> [a] -> b
nonEmpty e ne = maybe e ne . NE.nonEmpty

foldrMap1 :: (a -> b) -> (a -> b -> b) -> NonEmpty a -> b
foldrMap1 f g (h :| t) = nonEmpty (f h) (\x -> g h $ foldrMap1 f g x) t

foldrMap1' :: b -> (a -> b) -> (a -> b -> b) -> [a] -> b
foldrMap1' d f g = nonEmpty d (foldrMap1 f g)

foldrMapM1 :: (Applicative m, Monad m) => (a -> m b) -> (a -> b -> m b) -> NonEmpty a -> m b
foldrMapM1 f g (h :| t) = nonEmpty (f h) (\x -> foldrMapM1 f g x >>= g h) t

mkpair :: Applicative f => f a -> f b -> f (a, b)
mkpair = liftA2 (,)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

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

-- | Function to show a bytestring in a hex format.
showBS :: ByteString -> Text
showBS = decodeUtf8 . L.toStrict . toLazyByteString . byteStringHex

-- | Converts unsafe list functions in the Prelude to a safe version.
safe :: ([a] -> b) -> [a] -> Maybe b
safe _ [] = Nothing
safe f l = Just $ f l

-- | Show function for 'Text'
showT :: (Show a) => a -> Text
showT = T.pack . show

-- | Inserts commas between '[Text]' and except the last one.
comma :: [Text] -> Text
comma = T.intercalate ", "

-- | Inserts commas and newlines between '[Text]' and except the last one.
commaNL :: [Text] -> Text
commaNL = T.intercalate ",\n"
