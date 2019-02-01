{-|
Module      : VeriFuzz.Verilog.Gen
Description : Various useful generators.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Various useful generators.
-}

module VeriFuzz.Verilog.Gen where

import qualified Data.Text        as T
import           Test.QuickCheck  (Arbitrary, Gen, arbitrary)
import qualified Test.QuickCheck  as QC
import           VeriFuzz.Circuit
import           VeriFuzz.Verilog

randomMod :: Gen ModDecl
randomMod = do
  let ids = Identifier . ("w"<>) . T.pack . show <$> [1..100]
  moditems <- sequence $ randomAssigns ids
  return $ ModDecl "" [] [] []

fromGraph :: Gen ModDecl
fromGraph = do
  gr <- QC.generate $ rDups <$> QC.resize 100 (randomCircuit)
  return $ initMod
    .   head
    $   (nestUpTo 5 . generateAST $ Circuit gr)
    ^.. getVerilogSrc
    .   traverse
    .   getDescription
