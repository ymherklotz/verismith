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
    rmapM,
    nermapM,
    rmap',
    rmap,
    liftA4,
    liftA5,
    mkpair,
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
import Data.List (foldl')
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

rmapM :: Monad m => (a -> m b) -> [a] -> m [b]
rmapM f = foldl' (\acc e -> do y <- acc; x <- f e; return $ x : y) $ return []

nermapM :: Monad m => (a -> m b) -> NonEmpty a -> m (NonEmpty b)
nermapM f (x :| l) =
  foldl' (\acc e -> do y <- acc; x <- f e; return $ x <| y) ((:| []) <$> f x) l

rmap' :: (a -> b) -> [b] -> [a] -> [b]
rmap' f = foldl' (\acc e -> f e : acc)

rmap :: (a -> b) -> [a] -> [b]
rmap f = rmap' f []

liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = liftA3 f a b c <*> d

liftA5 :: Applicative f => (a -> b -> c -> d -> e -> z) -> f a -> f b -> f c -> f d -> f e -> f z
liftA5 f a b c d e = liftA4 f a b c d <*> e

mkpair :: Applicative f => f a -> f b -> f (a, b)
mkpair = liftA2 (,)

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
