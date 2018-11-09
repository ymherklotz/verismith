module VeriFuzz.Types where

data Gate = And
          | Or
          | Xor
          | Nor
          | Nand
          deriving (Show, Eq, Ord)
