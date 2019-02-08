{-|
Module      : VeriFuzz.Reduce
Description : Test case reducer implementation.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Test case reducer implementation.
-}

module VeriFuzz.Reduce where

import           Control.Lens
import           VeriFuzz.AST
import           VeriFuzz.Mutate

halve :: [a] -> ([a], [a])
halve l  = splitAt (length l `div` 2) l

removeUninitWires :: [ModItem] -> [ModItem]
removeUninitWires ms =  transformOf traverseModItem trans <$> ms
  where
    ids = ms ^.. traverse . modContAssign . contAssignNetLVal

halveModDecl :: ModDecl -> (ModDecl, ModDecl)
halveModDecl m =
  (m & modItems %~ fst . halve, m & modItems %~ snd . halve)
