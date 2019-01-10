{-|
Module      : Test.VeriFuzz.Verilog.Arbitrary
Description : Arbitrary instances for the AST.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Arbitrary instances for the AST.
-}

module Test.VeriFuzz.Verilog.Arbitrary where

import           Control.Monad             (replicateM)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Test.QuickCheck           as QC
import           Test.VeriFuzz.Verilog.AST

-- Generate Arbitrary instances for the AST

positiveArb :: (QC.Arbitrary a, Ord a, Num a) => QC.Gen a
positiveArb = QC.suchThat QC.arbitrary (>0)

expr :: Int -> QC.Gen Expr
expr 0 = QC.oneof
  [ Id <$> QC.arbitrary
  , Number <$> positiveArb <*> QC.arbitrary
  , UnOp <$> QC.arbitrary <*> QC.arbitrary
  -- , Str <$> QC.arbitrary
  ]
expr n
  | n > 0 = QC.oneof
    [ Id <$> QC.arbitrary
    , Number <$> positiveArb <*> QC.arbitrary
    , Concat <$> QC.listOf1 (subexpr 4)
    , UnOp <$> QC.arbitrary <*> QC.arbitrary
    -- , Str <$> QC.arbitrary
    , BinOp <$> subexpr 2 <*> QC.arbitrary <*> subexpr 2
    , Cond <$> subexpr 3 <*> subexpr 3 <*> subexpr 3
    ]
  | otherwise = expr 0
  where
    subexpr y = expr (n `div` y)

statement :: Int -> QC.Gen Stmnt
statement 0 = QC.oneof
  [ BlockAssign <$> QC.arbitrary
  , NonBlockAssign <$> QC.arbitrary
  -- , StatCA <$> QC.arbitrary
  , TaskEnable <$> QC.arbitrary
  , SysTaskEnable <$> QC.arbitrary
  ]
statement n
  | n > 0 = QC.oneof
    [ TimeCtrl <$> QC.arbitrary <*> (Just <$> substat 2)
    , SeqBlock <$> QC.listOf1 (substat 4)
    , BlockAssign <$> QC.arbitrary
    , NonBlockAssign <$> QC.arbitrary
    -- , StatCA <$> QC.arbitrary
    , TaskEnable <$> QC.arbitrary
    , SysTaskEnable <$> QC.arbitrary
    ]
  | otherwise = statement 0
  where
    substat y = statement (n `div` y)

modPortGen :: QC.Gen Port
modPortGen = QC.oneof
  [ Port Wire <$> positiveArb <*> QC.arbitrary
  , Port <$> (Reg <$> QC.arbitrary) <*> positiveArb <*> QC.arbitrary
  ]

instance QC.Arbitrary Text where
  arbitrary = T.pack <$> QC.arbitrary

instance QC.Arbitrary Identifier where
  arbitrary = do
    l <- QC.choose (2, 10)
    Identifier . T.pack <$> replicateM l (QC.elements ['a'..'z'])

instance QC.Arbitrary BinaryOperator where
  arbitrary = QC.elements
    [ BinPlus
    , BinMinus
    , BinTimes
    , BinDiv
    , BinMod
    , BinEq
    , BinNEq
    , BinCEq
    , BinCNEq
    , BinLAnd
    , BinLOr
    , BinLT
    , BinLEq
    , BinGT
    , BinGEq
    , BinAnd
    , BinOr
    , BinXor
    , BinXNor
    , BinXNorInv
    , BinPower
    , BinLSL
    , BinLSR
    , BinASL
    , BinASR
    ]

instance QC.Arbitrary UnaryOperator where
  arbitrary = QC.elements
    [ UnPlus
    , UnMinus
    , UnNot
    , UnAnd
    , UnNand
    , UnOr
    , UnNor
    , UnXor
    , UnNxor
    , UnNxorInv
    ]

instance QC.Arbitrary PortDir where
  arbitrary = QC.elements [PortIn, PortOut, PortInOut]

instance QC.Arbitrary PortType where
  arbitrary = QC.oneof [pure Wire, Reg <$> QC.arbitrary]

instance QC.Arbitrary Port where
  arbitrary = Port <$> QC.arbitrary <*> positiveArb <*> QC.arbitrary

instance QC.Arbitrary Delay where
  arbitrary = Delay <$> positiveArb

instance QC.Arbitrary Event where
  arbitrary = EId <$> QC.arbitrary

instance QC.Arbitrary ModConn where
  arbitrary = ModConn <$> QC.arbitrary

instance QC.Arbitrary ConstExpr where
  arbitrary = ConstExpr <$> positiveArb

instance QC.Arbitrary LVal where
  arbitrary = QC.oneof [ RegId <$> QC.arbitrary
                       , RegExpr <$> QC.arbitrary <*> QC.arbitrary
                       , RegSize <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
                       ]

instance QC.Arbitrary Assign where
  arbitrary = Assign <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary Expr where
  arbitrary = QC.sized expr

instance QC.Arbitrary Stmnt where
  arbitrary = QC.sized statement

instance QC.Arbitrary ContAssign where
  arbitrary = ContAssign <$> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary Task where
  arbitrary = Task <$> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary ModItem where
  arbitrary = QC.oneof [ ModCA <$> QC.arbitrary
                       , ModInst <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
                       , Initial <$> QC.arbitrary
                       , Always <$> (EventCtrl <$> QC.arbitrary <*> QC.arbitrary)
                       , Decl <$> pure Nothing <*> QC.arbitrary
                       ]

instance QC.Arbitrary ModDecl where
  arbitrary = ModDecl <$> QC.arbitrary <*> QC.arbitrary
              <*> QC.listOf1 modPortGen <*> QC.arbitrary

instance QC.Arbitrary Description where
  arbitrary = Description <$> QC.arbitrary

instance QC.Arbitrary VerilogSrc where
  arbitrary = VerilogSrc <$> QC.arbitrary
