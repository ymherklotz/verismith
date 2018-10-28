module Main where

import Data.Bits
import Test.QuickCheck hiding ((.&.), (.|.))

type Input = Bool

data Gate = And
          | Or
          | Xor
          | Nand
          | Nor
          deriving (Show)

data Circuit a = In a
               | Node Gate (Circuit a) (Circuit a)
               deriving (Show)

instance Arbitrary Gate where
  arbitrary = elements [And, Or, Xor, Nand, Nor]

instance (Arbitrary a) => Arbitrary (Circuit a) where
  arbitrary = do
    x <- arbitrary
    frequency [(1, return (In x)), (1, arbNode)]
      where
        arbNode = Node <$> arbitrary <*> arbitrary <*> arbitrary

eval :: (Bits a) => Circuit a -> a
eval (In val) = val
eval (Node And c1 c2) = eval c1 .&. eval c2
eval (Node Or c1 c2) = eval c1 .|. eval c2
eval (Node Xor c1 c2) = eval c1 `xor` eval c2
eval (Node Nand c1 c2) = complement $ eval c1 .&. eval c2
eval (Node Nor c1 c2) = complement $ eval c1 .|. eval c2

main :: IO ()
main = sample (arbitrary :: Gen (Circuit Input))
