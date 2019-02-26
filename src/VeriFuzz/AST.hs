{-|
Module      : VeriFuzz.AST
Description : Definition of the Verilog AST types.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Poratbility : POSIX

Defines the types to build a Verilog AST.
-}

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module VeriFuzz.AST
    ( -- * Top level types
      VerilogSrc(..)
    , getVerilogSrc
    , Description(..)
    , getDescription
    -- * Primitives
    -- ** Identifier
    , Identifier(..)
    , getIdentifier
    -- ** Control
    , Delay(..)
    , getDelay
    , Event(..)
    -- ** Operators
    , BinaryOperator(..)
    , UnaryOperator(..)
    -- ** Task
    , Task(..)
    , taskName
    , taskExpr
    -- ** Left hand side value
    , LVal(..)
    , regId
    , regExprId
    , regExpr
    , regSizeId
    , regSizeMSB
    , regSizeLSB
    , regConc
    -- ** Ports
    , PortDir(..)
    , PortType(..)
    , Port(..)
    , portType
    , portSigned
    , portSize
    , portName
    -- * Expression
    , Expr(..)
    , exprSize
    , exprVal
    , exprId
    , exprConcat
    , exprUnOp
    , exprPrim
    , exprLhs
    , exprBinOp
    , exprRhs
    , exprCond
    , exprTrue
    , exprFalse
    , exprFunc
    , exprBody
    , exprStr
    , exprWithContext
    , traverseExpr
    , ConstExpr(..)
    , constNum
    , Function(..)
    -- * Assignment
    , Assign(..)
    , assignReg
    , assignDelay
    , assignExpr
    , ContAssign(..)
    , contAssignNetLVal
    , contAssignExpr
    -- * Statment
    , Stmnt(..)
    , statDelay
    , statDStat
    , statEvent
    , statEStat
    , statements
    , stmntBA
    , stmntNBA
    , stmntCA
    , stmntTask
    , stmntSysTask
    -- * Module
    , ModDecl(..)
    , modId
    , modOutPorts
    , modInPorts
    , modItems
    , ModItem(..)
    , modContAssign
    , modInstId
    , modInstName
    , modInstConns
    , traverseModItem
    , declDir
    , declPort
    , ModConn(..)
    , modConn
    , modConnName
    , modExpr
    )
where

import           Control.Lens
import           Control.Monad    (replicateM)
import           Data.Data
import           Data.Data.Lens
import           Data.String      (IsString, fromString)
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Traversable (sequenceA)
import qualified Test.QuickCheck  as QC

positiveArb :: (QC.Arbitrary a, Ord a, Num a) => QC.Gen a
positiveArb = QC.suchThat QC.arbitrary (> 0)

-- | Identifier in Verilog. This is just a string of characters that can either
-- be lowercase and uppercase for now. This might change in the future though,
-- as Verilog supports many more characters in Identifiers.
newtype Identifier = Identifier { _getIdentifier :: Text }
                   deriving (Eq, Show, Ord, Data, IsString, Semigroup, Monoid)

makeLenses ''Identifier

instance QC.Arbitrary Identifier where
  arbitrary = do
    l <- QC.choose (2, 10)
    Identifier . T.pack <$> replicateM l (QC.elements ['a'..'z'])

-- | Verilog syntax for adding a delay, which is represented as @#num@.
newtype Delay = Delay { _getDelay :: Int }
                deriving (Eq, Show, Ord, Data, Num)

makeLenses ''Delay

instance QC.Arbitrary Delay where
  arbitrary = Delay <$> positiveArb

-- | Verilog syntax for an event, such as @\@x@, which is used for always blocks
data Event = EId Identifier
           | EExpr Expr
           | EAll
           | EPosEdge Identifier
           | ENegEdge Identifier
           deriving (Eq, Show, Ord, Data)

instance QC.Arbitrary Event where
  arbitrary = EId <$> QC.arbitrary

-- | Binary operators that are currently supported in the verilog generation.
data BinaryOperator = BinPlus    -- ^ @+@
                    | BinMinus   -- ^ @-@
                    | BinTimes   -- ^ @*@
                    | BinDiv     -- ^ @/@
                    | BinMod     -- ^ @%@
                    | BinEq      -- ^ @==@
                    | BinNEq     -- ^ @!=@
                    | BinCEq     -- ^ @===@
                    | BinCNEq    -- ^ @!==@
                    | BinLAnd    -- ^ @&&@
                    | BinLOr     -- ^ @||@
                    | BinLT      -- ^ @<@
                    | BinLEq     -- ^ @<=@
                    | BinGT      -- ^ @>@
                    | BinGEq     -- ^ @>=@
                    | BinAnd     -- ^ @&@
                    | BinOr      -- ^ @|@
                    | BinXor     -- ^ @^@
                    | BinXNor    -- ^ @^~@
                    | BinXNorInv -- ^ @~^@
                    | BinPower   -- ^ @**@
                    | BinLSL     -- ^ @<<@
                    | BinLSR     -- ^ @>>@
                    | BinASL     -- ^ @<<<@
                    | BinASR     -- ^ @>>>@
                    deriving (Eq, Show, Ord, Data)

instance QC.Arbitrary BinaryOperator where
  arbitrary = QC.elements
    [ BinPlus
    , BinMinus
    , BinTimes
--    , BinDiv
--    , BinMod
    , BinEq
    , BinNEq
--    , BinCEq
--    , BinCNEq
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
--    , BinPower
    , BinLSL
    , BinLSR
    , BinASL
    , BinASR
    ]

-- | Unary operators that are currently supported by the generator.
data UnaryOperator = UnPlus    -- ^ @+@
                   | UnMinus   -- ^ @-@
                   | UnLNot    -- ^ @!@
                   | UnNot     -- ^ @~@
                   | UnAnd     -- ^ @&@
                   | UnNand    -- ^ @~&@
                   | UnOr      -- ^ @|@
                   | UnNor     -- ^ @~|@
                   | UnXor     -- ^ @^@
                   | UnNxor    -- ^ @~^@
                   | UnNxorInv -- ^ @^~@
                   deriving (Eq, Show, Ord, Data)

instance QC.Arbitrary UnaryOperator where
  arbitrary = QC.elements
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

data Function = SignedFunc
              | UnSignedFunc
              deriving (Eq, Show, Ord, Data)

instance QC.Arbitrary Function where
  arbitrary = QC.elements
    [ SignedFunc
    , UnSignedFunc
    ]

-- | Verilog expression, which can either be a primary expression, unary
-- expression, binary operator expression or a conditional expression.
data Expr = Number { _exprSize :: Int
                   , _exprVal  :: Integer
                   }
          | Id { _exprId :: Identifier }
          | Concat { _exprConcat :: [Expr] }
          | UnOp { _exprUnOp :: UnaryOperator
                 , _exprPrim :: Expr
                 }
          | BinOp { _exprLhs   :: Expr
                  , _exprBinOp :: BinaryOperator
                  , _exprRhs   :: Expr
                  }
          | Cond { _exprCond  :: Expr
                 , _exprTrue  :: Expr
                 , _exprFalse :: Expr
                 }
          | Func { _exprFunc :: Function
                 , _exprBody :: Expr
                 }
          | Str { _exprStr :: Text }
          deriving (Eq, Show, Ord, Data)

instance Num Expr where
  a + b = BinOp a BinPlus b
  a - b = BinOp a BinMinus b
  a * b = BinOp a BinTimes b
  negate = UnOp UnMinus
  abs = undefined
  signum = undefined
  fromInteger = Number 32 . fromInteger

instance Semigroup Expr where
  (Concat a) <> (Concat b) = Concat $ a <> b
  (Concat a) <> b = Concat $ a <> [b]
  a <> (Concat b) = Concat $ a : b
  a <> b = Concat [a, b]

instance Monoid Expr where
  mempty = Concat []

instance IsString Expr where
  fromString = Str . fromString

instance Plated Expr where
  plate = uniplate

exprSafeList :: [QC.Gen Expr]
exprSafeList = [Number <$> positiveArb <*> QC.arbitrary]

exprRecList :: (Int -> QC.Gen Expr) -> [QC.Gen Expr]
exprRecList subexpr =
    [ Number <$> positiveArb <*> QC.arbitrary
    , Concat <$> QC.listOf1 (subexpr 8)
    , UnOp
        <$> QC.arbitrary
        <*> subexpr 2
    -- , Str <$> QC.arbitrary
    , BinOp <$> subexpr 2 <*> QC.arbitrary <*> subexpr 2
    , Cond <$> subexpr 3 <*> subexpr 3 <*> subexpr 3
    , Func <$> QC.arbitrary <*> subexpr 2
    ]

expr :: Int -> QC.Gen Expr
expr n | n == 0    = QC.oneof $ (Id <$> QC.arbitrary) : exprSafeList
       | n > 0     = QC.oneof $ (Id <$> QC.arbitrary) : exprRecList subexpr
       | otherwise = expr 0
    where subexpr y = expr (n `div` y)

exprWithContext :: [Identifier] -> Int -> QC.Gen Expr
exprWithContext [] n | n == 0    = QC.oneof exprSafeList
                     | n > 0     = QC.oneof $ exprRecList subexpr
                     | otherwise = exprWithContext [] 0
    where subexpr y = exprWithContext [] (n `div` y)
exprWithContext l n
    | n == 0    = QC.oneof $ (Id <$> QC.elements l) : exprSafeList
    | n > 0     = QC.oneof $ (Id <$> QC.elements l) : exprRecList subexpr
    | otherwise = exprWithContext l 0
    where subexpr y = exprWithContext l (n `div` y)

instance QC.Arbitrary Expr where
  arbitrary = QC.sized expr

traverseExpr :: (Applicative f) => (Expr -> f Expr) -> Expr -> f Expr
traverseExpr f (Concat e   ) = Concat <$> sequenceA (f <$> e)
traverseExpr f (UnOp u e   ) = UnOp u <$> f e
traverseExpr f (BinOp l o r) = BinOp <$> f l <*> pure o <*> f r
traverseExpr f (Cond  c l r) = Cond <$> f c <*> f l <*> f r
traverseExpr f (Func fn e  ) = Func fn <$> f e
traverseExpr _ e             = pure e

makeLenses ''Expr

-- | Constant expression, which are known before simulation at compilation time.
newtype ConstExpr = ConstExpr { _constNum :: Int }
                  deriving (Eq, Show, Ord, Data, Num, QC.Arbitrary)

makeLenses ''ConstExpr

data Task = Task { _taskName :: Identifier
                 , _taskExpr :: [Expr]
                 } deriving (Eq, Show, Ord, Data)

makeLenses ''Task

instance QC.Arbitrary Task where
  arbitrary = Task <$> QC.arbitrary <*> QC.arbitrary

-- | Type that represents the left hand side of an assignment, which can be a
-- concatenation such as in:
--
-- @
-- {a, b, c} = 32'h94238;
-- @
data LVal = RegId { _regId :: Identifier}
          | RegExpr { _regExprId :: Identifier
                    , _regExpr   :: Expr
                    }
          | RegSize { _regSizeId  :: Identifier
                    , _regSizeMSB :: ConstExpr
                    , _regSizeLSB :: ConstExpr
                    }
          | RegConcat { _regConc :: [Expr] }
          deriving (Eq, Show, Ord, Data)

makeLenses ''LVal

instance QC.Arbitrary LVal where
  arbitrary = QC.oneof [ RegId <$> QC.arbitrary
                       , RegExpr <$> QC.arbitrary <*> QC.arbitrary
                       , RegSize <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
                       ]

instance IsString LVal where
  fromString = RegId . fromString

-- | Different port direction that are supported in Verilog.
data PortDir = PortIn    -- ^ Input direction for port (@input@).
             | PortOut   -- ^ Output direction for port (@output@).
             | PortInOut -- ^ Inout direction for port (@inout@).
             deriving (Eq, Show, Ord, Data)

instance QC.Arbitrary PortDir where
  arbitrary = QC.elements [PortIn, PortOut, PortInOut]

-- | Currently, only @wire@ and @reg@ are supported, as the other net types are
-- not that common and not a priority.
data PortType = Wire
              | Reg
              deriving (Eq, Show, Ord, Data)

instance QC.Arbitrary PortType where
  arbitrary = QC.elements [Wire, Reg]

makeLenses ''PortType

-- | Port declaration. It contains information about the type of the port, the
-- size, and the port name. It used to also contain information about if it was
-- an input or output port. However, this is not always necessary and was more
-- cumbersome than useful, as a lot of ports can be declared without input and
-- output port.
--
-- This is now implemented inside 'ModDecl' itself, which uses a list of output
-- and input ports.
data Port = Port { _portType   :: PortType
                 , _portSigned :: Bool
                 , _portSize   :: Int
                 , _portName   :: Identifier
                 } deriving (Eq, Show, Ord, Data)

makeLenses ''Port

instance QC.Arbitrary Port where
  arbitrary = Port <$> QC.arbitrary <*> QC.arbitrary
              <*> positiveArb <*> QC.arbitrary

-- | This is currently a type because direct module declaration should also be
-- added:
--
-- @
-- mod a(.y(y1), .x1(x11), .x2(x22));
-- @
data ModConn = ModConn { _modConn :: Expr }
             | ModConnNamed { _modConnName :: Identifier
                            , _modExpr     :: Expr
                            }
             deriving (Eq, Show, Ord, Data)

makeLenses ''ModConn

instance QC.Arbitrary ModConn where
  arbitrary = ModConn <$> QC.arbitrary

data Assign = Assign { _assignReg   :: LVal
                     , _assignDelay :: Maybe Delay
                     , _assignExpr  :: Expr
                     } deriving (Eq, Show, Ord, Data)

makeLenses ''Assign

instance QC.Arbitrary Assign where
  arbitrary = Assign <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

data ContAssign = ContAssign { _contAssignNetLVal :: Identifier
                             , _contAssignExpr    :: Expr
                             } deriving (Eq, Show, Ord, Data)

makeLenses ''ContAssign

instance QC.Arbitrary ContAssign where
  arbitrary = ContAssign <$> QC.arbitrary <*> QC.arbitrary

-- | Statements in Verilog.
data Stmnt = TimeCtrl { _statDelay :: Delay
                      , _statDStat :: Maybe Stmnt
                      }                             -- ^ Time control (@#NUM@)
           | EventCtrl { _statEvent :: Event
                       , _statEStat :: Maybe Stmnt
                       }
           | SeqBlock { _statements :: [Stmnt] }    -- ^ Sequential block (@begin ... end@)
           | BlockAssign { _stmntBA :: Assign }     -- ^ blocking assignment (@=@)
           | NonBlockAssign { _stmntNBA :: Assign } -- ^ Non blocking assignment (@<=@)
           | StatCA { _stmntCA :: ContAssign }      -- ^ Stmnt continuous assignment. May not be correct.
           | TaskEnable { _stmntTask :: Task}
           | SysTaskEnable { _stmntSysTask :: Task}
           deriving (Eq, Show, Ord, Data)

makeLenses ''Stmnt

instance Semigroup Stmnt where
  (SeqBlock a) <> (SeqBlock b) = SeqBlock $ a <> b
  (SeqBlock a) <> b = SeqBlock $ a <> [b]
  a <> (SeqBlock b) = SeqBlock $ a : b
  a <> b = SeqBlock [a, b]

instance Monoid Stmnt where
  mempty = SeqBlock []

statement :: Int -> QC.Gen Stmnt
statement n
    | n == 0 = QC.oneof
        [ BlockAssign <$> QC.arbitrary
        , NonBlockAssign <$> QC.arbitrary
    -- , StatCA <$> QC.arbitrary
        , TaskEnable <$> QC.arbitrary
        , SysTaskEnable <$> QC.arbitrary
        ]
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
    where substat y = statement (n `div` y)

instance QC.Arbitrary Stmnt where
  arbitrary = QC.sized statement

-- | Module item which is the body of the module expression.
data ModItem = ModCA { _modContAssign :: ContAssign}
             | ModInst { _modInstId    :: Identifier
                       , _modInstName  :: Identifier
                       , _modInstConns :: [ModConn]
                       }
             | Initial Stmnt
             | Always Stmnt
             | Decl { _declDir  :: Maybe PortDir
                    , _declPort :: Port
                    }
             deriving (Eq, Show, Ord, Data)

makeLenses ''ModItem

instance QC.Arbitrary ModItem where
  arbitrary = QC.oneof [ ModCA <$> QC.arbitrary
                       , ModInst <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
                       , Initial <$> QC.arbitrary
                       , Always <$> (EventCtrl <$> QC.arbitrary <*> QC.arbitrary)
                       , Decl <$> pure Nothing <*> QC.arbitrary
                       ]

-- | 'module' module_identifier [list_of_ports] ';' { module_item } 'end_module'
data ModDecl = ModDecl { _modId       :: Identifier
                       , _modOutPorts :: [Port]
                       , _modInPorts  :: [Port]
                       , _modItems    :: [ModItem]
                       } deriving (Eq, Show, Ord, Data)

traverseModConn :: (Applicative f) => (Expr -> f Expr) -> ModConn -> f ModConn
traverseModConn f (ModConn e       ) = ModConn <$> f e
traverseModConn f (ModConnNamed a e) = ModConnNamed a <$> f e

traverseModItem :: (Applicative f) => (Expr -> f Expr) -> ModItem -> f ModItem
traverseModItem f (ModCA (ContAssign a e)) = ModCA . ContAssign a <$> f e
traverseModItem f (ModInst a b e) =
    ModInst a b <$> sequenceA (traverseModConn f <$> e)
traverseModItem _ e = pure e

makeLenses ''ModDecl

modPortGen :: QC.Gen Port
modPortGen =
    Port <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary ModDecl where
  arbitrary = ModDecl <$> QC.arbitrary <*> QC.arbitrary <*> QC.listOf1 modPortGen <*> QC.arbitrary

-- | Description of the Verilog module.
newtype Description = Description { _getDescription :: ModDecl }
                    deriving (Eq, Show, Ord, Data, QC.Arbitrary)

makeLenses ''Description

-- | The complete sourcetext for the Verilog module.
newtype VerilogSrc = VerilogSrc { _getVerilogSrc :: [Description] }
                   deriving (Eq, Show, Ord, Data, QC.Arbitrary, Semigroup, Monoid)

makeLenses ''VerilogSrc
