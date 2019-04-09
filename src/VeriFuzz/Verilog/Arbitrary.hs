{-|
Module      : VeriFuzz.Verilog.Arbitrary
Description : Arb instance for all the types.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Arb instance for all the types.
-}

module VeriFuzz.Verilog.Arbitrary
    (
    -- * Arbitrary
      Arb
    , arb
    , genPositive
    , exprWithContext
    , listOf1
    , listOf
    )
where

import           Control.Monad        (replicateM)
import           Data.List.NonEmpty   (toList)
import qualified Data.Text            as T
import           Hedgehog             (Gen)
import qualified Hedgehog.Gen         as Hog
import qualified Hedgehog.Range       as Hog
import           VeriFuzz.Verilog.AST

listOf1 :: Gen a -> Gen [a]
listOf1 a = toList <$> Hog.nonEmpty (Hog.linear 0 100) a

listOf :: Gen a -> Gen [a]
listOf = Hog.list (Hog.linear 0 100)

genPositive :: Gen Int
genPositive = Hog.filter (>= 0) $ Hog.int (Hog.linear 1 99)

integral :: Gen Integer
integral = Hog.integral (Hog.linear 0 100)

class Arb a where
    arb :: Gen a

instance Arb Identifier where
    arb = do
        l <- genPositive
        Identifier . T.pack <$> replicateM (l + 1) (Hog.element ['a'..'z'])

instance Arb Int where
    arb = Hog.int (Hog.linear 0 100)

instance Arb Integer where
    arb = integral

instance Arb Delay where
    arb = Delay <$> genPositive

instance Arb Event where
    arb = EId <$> arb

instance Arb BinaryOperator where
    arb = Hog.element
        [ BinPlus
        , BinMinus
        , BinTimes
        -- , BinDiv
        -- , BinMod
        , BinEq
        , BinNEq
        -- , BinCEq
        -- , BinCNEq
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
        -- , BinPower
        , BinLSL
        , BinLSR
        , BinASL
        , BinASR
        ]

instance Arb UnaryOperator where
    arb = Hog.element
        [ UnPlus
        , UnMinus
        , UnNot
        , UnLNot
        , UnAnd
        , UnNand
        , UnOr
        , UnNor
        , UnXor
        , UnNxor
        , UnNxorInv
        ]

instance Arb Function where
    arb = Hog.element
        [ SignedFunc
        , UnSignedFunc
        ]

instance Arb Expr where
    arb = Hog.sized expr

exprSafeList :: [Gen Expr]
exprSafeList = [Number <$> genPositive <*> integral]

exprRecList :: (Hog.Size -> Gen Expr) -> [Gen Expr]
exprRecList subexpr =
    [ Number <$> genPositive <*> integral
    , Concat <$> listOf1 (subexpr 8)
    , UnOp
        <$> arb
        <*> subexpr 2
    -- , Str <$> arb
    , BinOp <$> subexpr 2 <*> arb <*> subexpr 2
    , Cond <$> subexpr 3 <*> subexpr 3 <*> subexpr 3
    , Func <$> arb <*> subexpr 2
    ]

expr :: Hog.Size -> Gen Expr
expr n | n == 0    = Hog.choice $ (Id <$> arb) : exprSafeList
       | n > 0     = Hog.choice $ (Id <$> arb) : exprRecList subexpr
       | otherwise = expr 0
    where subexpr y = expr (n `div` y)

exprWithContext :: [Identifier] -> Hog.Size -> Gen Expr
exprWithContext [] n | n == 0    = Hog.choice exprSafeList
                     | n > 0     = Hog.choice $ exprRecList subexpr
                     | otherwise = exprWithContext [] 0
    where subexpr y = exprWithContext [] (n `div` y)
exprWithContext l n
    | n == 0    = Hog.choice $ (Id <$> Hog.element l) : exprSafeList
    | n > 0     = Hog.choice $ (Id <$> Hog.element l) : exprRecList subexpr
    | otherwise = exprWithContext l 0
    where subexpr y = exprWithContext l (n `div` y)

constExpr :: Gen ConstExpr
constExpr = Hog.recursive Hog.choice
            [ ConstNum <$> genPositive <*> arb
            , ParamId <$> arb
            ]
            [ Hog.subtermM constExpr (\e -> ConstUnOp <$> arb <*> pure e)
            , Hog.subtermM2 constExpr constExpr (\a b -> ConstBinOp <$> pure a <*> arb <*> pure b)
            , Hog.subterm3 constExpr constExpr constExpr ConstCond
            ]

instance Arb ConstExpr where
    arb = constExpr

instance Arb Task where
    arb = Task <$> arb <*> listOf arb

instance Arb LVal where
    arb = Hog.choice [ RegId <$> arb
                     , RegExpr <$> arb <*> arb
                     , RegSize <$> arb <*> arb <*> arb
                     ]

instance Arb PortDir where
    arb = Hog.element [PortIn, PortOut, PortInOut]

instance Arb PortType where
    arb = Hog.element [Wire, Reg]

instance Arb Port where
    arb = Port <$> arb <*> arb <*> genPositive <*> arb

instance Arb ModConn where
    arb = ModConn <$> arb

instance Arb Assign where
    arb = Assign <$> arb <*> Hog.maybe arb <*> arb

instance Arb ContAssign where
  arb = ContAssign <$> arb <*> arb

instance Arb Statement where
    arb = Hog.sized statement

statement :: Hog.Size -> Gen Statement
statement n
    | n == 0 = Hog.choice
        [ BlockAssign <$> arb
        , NonBlockAssign <$> arb
        , TaskEnable <$> arb
        , SysTaskEnable <$> arb
        ]
    | n > 0 = Hog.choice
        [ TimeCtrl <$> arb <*> (Just <$> substat 2)
        , SeqBlock <$> listOf1 (substat 4)
        , BlockAssign <$> arb
        , NonBlockAssign <$> arb
        , TaskEnable <$> arb
        , SysTaskEnable <$> arb
        ]
    | otherwise = statement 0
    where substat y = statement (n `div` y)

instance Arb ModItem where
    arb = Hog.choice [ ModCA <$> arb
                     , ModInst <$> arb <*> arb <*> listOf arb
                     , Initial <$> arb
                     , Always <$> (EventCtrl <$> arb <*> Hog.maybe arb)
                     , Decl <$> pure Nothing <*> arb
                     ]

modPortGen :: Gen Port
modPortGen = Port <$> arb <*> arb <*> arb <*> arb

instance Arb Parameter where
    arb = Parameter <$> arb <*> arb

instance Arb LocalParam where
    arb = LocalParam <$> arb <*> arb

instance Arb ModDecl where
    arb = ModDecl <$> arb <*> listOf arb <*> listOf1 modPortGen <*> listOf arb <*> listOf arb

instance Arb Verilog where
    arb = Verilog <$> listOf1 arb

instance Arb Bool where
    arb = Hog.element [True, False]
