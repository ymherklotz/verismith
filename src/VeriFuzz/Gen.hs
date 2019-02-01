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
import qualified Data.Text       as T
import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import           VeriFuzz.AST
import           VeriFuzz.ASTGen
import           VeriFuzz.Mutate
import           VeriFuzz.Random

random :: [Identifier] -> (Expr -> ContAssign) -> Gen ModItem
random ctx fun = do
  expr <- QC.sized (exprWithContext ctx)
  return . ModCA $ fun expr

randomAssigns :: [Identifier] -> [Gen ModItem]
randomAssigns ids = random ids . ContAssign <$> ids

randomMod :: Gen ModDecl
randomMod = do
  let ids = Identifier . ("w"<>) . T.pack . show <$> [1..100]
  _ <- sequence $ randomAssigns ids
  return $ ModDecl "" [] [] []

fromGraph :: Gen ModDecl
fromGraph = do
  gr <- rDupsCirc <$> QC.resize 100 randomCircuit
  return $ initMod
    .   head
    $   nestUpTo 5 (generateAST gr)
    ^.. getVerilogSrc
    .   traverse
    .   getDescription
