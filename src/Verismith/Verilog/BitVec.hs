{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-- |
-- Module      : Verismith.Verilog.BitVec
-- Description : Unsigned BitVec implementation.
-- Copyright   : (c) 2019, Yann Herklotz Grave
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Unsigned BitVec implementation.
module Verismith.Verilog.BitVec
  ( BitVecF (..),
    BitVec,
    bitVec,
    select,
  )
where

import Control.DeepSeq (NFData)
import Data.Bits
import Data.Data
import Data.Ratio
import GHC.Generics (Generic)

-- | Bit Vector that stores the bits in an arbitrary container together with the
-- size.
data BitVecF a
  = BitVec
      { width :: {-# UNPACK #-} !Int,
        value :: !a
      }
  deriving (Show, Eq, Ord, Data, Functor, Foldable, Traversable, Generic, NFData)

-- | Specialisation of the above with Integer, so that infinitely large bit
-- vectors can be stored.
type BitVec = BitVecF Integer

instance (Enum a) => Enum (BitVecF a) where
  toEnum i = BitVec (width' $ fromIntegral i) $ toEnum i
  fromEnum (BitVec _ v) = fromEnum v

instance (Num a, Bits a) => Num (BitVecF a) where
  BitVec w1 v1 + BitVec w2 v2 = bitVec (max w1 w2) (v1 + v2)
  BitVec w1 v1 - BitVec w2 v2 = bitVec (max w1 w2) (v1 - v2)
  BitVec w1 v1 * BitVec w2 v2 = bitVec (max w1 w2) (v1 * v2)
  abs = id
  signum (BitVec _ v) = if v == 0 then bitVec 1 0 else bitVec 1 1
  fromInteger i = bitVec (width' i) $ fromInteger i

instance (Integral a, Bits a) => Real (BitVecF a) where
  toRational (BitVec _ n) = fromIntegral n % 1

instance (Integral a, Bits a) => Integral (BitVecF a) where
  quotRem (BitVec w1 v1) (BitVec w2 v2) = both (BitVec $ max w1 w2) $ quotRem v1 v2
  toInteger (BitVec _ v) = toInteger v

instance (Num a, Bits a) => Bits (BitVecF a) where
  BitVec w1 v1 .&. BitVec w2 v2 = bitVec (max w1 w2) (v1 .&. v2)
  BitVec w1 v1 .|. BitVec w2 v2 = bitVec (max w1 w2) (v1 .|. v2)
  BitVec w1 v1 `xor` BitVec w2 v2 = bitVec (max w1 w2) (v1 `xor` v2)
  complement (BitVec w v) = bitVec w $ complement v
  shift (BitVec w v) i = bitVec w $ shift v i
  rotate = rotateBitVec
  bit i = fromInteger $ bit i
  testBit (BitVec _ v) = testBit v
  bitSize (BitVec w _) = w
  bitSizeMaybe (BitVec w _) = Just w
  isSigned _ = False
  popCount (BitVec _ v) = popCount v

instance (Num a, Bits a) => FiniteBits (BitVecF a) where
  finiteBitSize (BitVec w _) = w

instance Bits a => Semigroup (BitVecF a) where
  (BitVec w1 v1) <> (BitVec w2 v2) = BitVec (w1 + w2) (shiftL v1 w2 .|. v2)

instance Bits a => Monoid (BitVecF a) where
  mempty = BitVec 0 zeroBits

-- | BitVecF construction, given width and value.
bitVec :: (Num a, Bits a) => Int -> a -> BitVecF a
bitVec w v = BitVec w' $ v .&. ((2 ^ w') - 1) where w' = max w 0

-- | Bit selection.  LSB is 0.
select ::
  (Integral a, Bits a, Integral b, Bits b) =>
  BitVecF a ->
  (BitVecF b, BitVecF b) ->
  BitVecF a
select (BitVec _ v) (msb, lsb) =
  bitVec (from $ msb - lsb + 1) . shiftR (fromIntegral v) $ from lsb
  where
    from = fromIntegral . value

-- | Rotate bits in a 'BitVec'.
rotateBitVec :: (Num a, Bits a) => BitVecF a -> Int -> BitVecF a
rotateBitVec b@(BitVec s _) n
  | n >= 0 = iterate rotateL1 b !! n
  | otherwise = iterate rotateR1 b !! abs n
  where
    rotateR1 n' = testBits 0 (s - 1) n' .|. shiftR n' 1
    rotateL1 n' = testBits (s - 1) 0 n' .|. shiftL n' 1
    testBits a b' n' = if testBit n' a then bit b' else zeroBits

width' :: Integer -> Int
width' a
  | a == 0 = 1
  | otherwise = width'' a
  where
    width'' a'
      | a' == 0 = 0
      | a' == -1 = 1
      | otherwise = 1 + width'' (shiftR a' 1)

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)
