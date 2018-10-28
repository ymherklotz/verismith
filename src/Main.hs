module Main where

import Data.Bits

newtype Input = Input { getInput :: Bool }
              deriving (Show)

data Gate = And
          | Or
          | Xor
          | Nand
          | Nor
          deriving (Show)

data Circuit = In Input
             | Node Gate Circuit Circuit
             deriving (Show)

eval :: Circuit -> Bool
eval (In val) = getInput val
eval (Node And c1 c2) = eval c1 .&. eval c2
eval (Node Or c1 c2) = eval c1 .|. eval c2
eval (Node Xor c1 c2) = eval c1 `xor` eval c2
eval (Node Nand c1 c2) = complement $ eval c1 .&. eval c2
eval (Node Nor c1 c2) = complement $ eval c1 .|. eval c2

main :: IO ()
main = print . eval $ Node And (In . Input $ True) (In . Input $ True)
