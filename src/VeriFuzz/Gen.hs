{-|
Module      : VeriFuzz.Gen
Description : Various useful generators.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Various useful generators.
-}

module VeriFuzz.Gen where

import           Control.Lens
import           Data.Foldable   (fold)
import qualified Data.Text       as T
import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import           VeriFuzz.AST
import           VeriFuzz.ASTGen
import           VeriFuzz.Mutate
import           VeriFuzz.Random

toId :: Int -> Identifier
toId = Identifier . ("w"<>) . T.pack . show

toPort :: Identifier -> Gen Port
toPort ident = do
  i <- abs <$> QC.arbitrary
  return $ Port Wire i ident

sumSize :: [Port] -> Int
sumSize ports =
  sum $ ports ^.. traverse . portSize

random :: [Identifier] -> (Expr -> ContAssign) -> Gen ModItem
random ctx fun = do
  expr <- QC.sized (exprWithContext ctx)
  return . ModCA $ fun expr

randomAssigns :: [Identifier] -> [Gen ModItem]
randomAssigns ids = random ids . ContAssign <$> ids

randomOrdAssigns :: [Identifier] -> [Identifier] -> [Gen ModItem]
randomOrdAssigns inp ids =
  snd $ foldr gen (inp, []) ids
  where
    gen cid (i, o) = (cid : i, o ++ [random i $ ContAssign cid])

randomMod :: Int -> Int -> Gen ModDecl
randomMod inps total = do
  let ids = toId <$> [1..total]
  x <- sequence . randomOrdAssigns (take inps ids) $ drop inps ids
  ident <-  sequence $ toPort <$> ids
  let inputs = take inps ident
  let other = drop inps ident
  let y = ModCA . ContAssign "y" . fold $ Id <$> drop inps ids
  let yport = [Port Wire (sumSize other) "y"]
  return . initMod . declareMod other .ModDecl "test_module" yport inputs $ x ++ [y]

fromGraph :: Gen ModDecl
fromGraph = do
  gr <- rDupsCirc <$> QC.resize 100 randomCircuit
  return $ initMod
    .   head
    $   nestUpTo 5 (generateAST gr)
    ^.. getVerilogSrc
    .   traverse
    .   getDescription
