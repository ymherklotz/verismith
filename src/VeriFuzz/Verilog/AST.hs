{-|
Module      : VeriFuzz.Verilog.AST
Description : Definition of the Verilog AST types.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Poratbility : POSIX

Defines the types to build a Verilog AST.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module VeriFuzz.Verilog.AST where

import           Control.Lens     (makeLenses, (^.))
import           Data.String      (IsString, fromString)
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Traversable (sequenceA)
import qualified QuickCheck       as QC

-- | 'Source' class which determines that source code is able to be generated
-- from the data structure using 'genSource'. This will be stored in 'Text' and
-- can then be processed further.
class Source a where
  genSource :: a -> Text

positiveArb :: (QC.Arbitrary a, Ord a, Num a) => QC.Gen a
positiveArb = QC.suchThat QC.arbitrary (>0)

instance QC.Arbitrary Text where
  arbitrary = T.pack <$> QC.arbitrary

-- | Identifier in Verilog. This is just a string of characters that can either
-- be lowercase and uppercase for now. This might change in the future though,
-- as Verilog supports many more characters in Identifiers.
newtype Identifier = Identifier { _getIdentifier :: Text }
                   deriving (Eq, IsString, Semigroup, Monoid)

makeLenses ''Identifier

instance Show Identifier where
  show i = T.unpack $ i ^. getIdentifier

instance QC.Arbitrary Identifier where
  arbitrary = do
    l <- QC.choose (2, 10)
    Identifier . T.pack <$> replicateM l (QC.elements ['a'..'z'])

-- | Verilog syntax for adding a delay, which is represented as @#num@.
newtype Delay = Delay { _delay :: Int }
                deriving (Eq)

instance Num Delay where
  Delay a + Delay b = Delay $ a + b
  Delay a - Delay b = Delay $ a - b
  Delay a * Delay b = Delay $ a * b
  negate (Delay a) = Delay $ negate a
  abs (Delay a) = Delay $ abs a
  signum (Delay a) = Delay $ signum a
  fromInteger = Delay . fromInteger

instance QC.Arbitrary Delay where
  arbitrary = Delay <$> positiveArb

-- | Verilog syntax for an event, such as @\@x@, which is used for always blocks
data Event = EId Identifier
           | EExpr Expr
           | EAll
           deriving (Eq)

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
                    deriving (Eq)

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

-- | Unary operators that are currently supported by the generator.
data UnaryOperator = UnPlus    -- ^ @+@
                   | UnMinus   -- ^ @-@
                   | UnNot     -- ^ @!@
                   | UnAnd     -- ^ @&@
                   | UnNand    -- ^ @~&@
                   | UnOr      -- ^ @|@
                   | UnNor     -- ^ @~|@
                   | UnXor     -- ^ @^@
                   | UnNxor    -- ^ @~^@
                   | UnNxorInv -- ^ @^~@
                   deriving (Eq)

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

-- | Verilog expression, which can either be a primary expression, unary
-- expression, binary operator expression or a conditional expression.
data Expr = Number { _numSize :: Int
                   , _numVal  :: Integer
                   }
          | Id { _exprId :: Identifier }
          | Concat { _concatExpr :: [Expr] }
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
          | Str { _exprStr :: Text }
          deriving (Eq)

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

instance QC.Arbitrary Expr where
  arbitrary = QC.sized expr

traverseExpr :: (Applicative f) => (Expr -> f Expr) -> Expr -> f Expr
traverseExpr f (Concat e)     = Concat <$> sequenceA (f <$> e)
traverseExpr f (UnOp un e)    = UnOp un <$> f e
traverseExpr f (BinOp l op r) = BinOp <$> f l <*> pure op <*> f r
traverseExpr f (Cond c l r)   = Cond <$> f c <*> f l <*> f r
traverseExpr _ e              = pure e

makeLenses ''Expr

-- | Constant expression, which are known before simulation at compilation time.
newtype ConstExpr = ConstExpr { _constNum :: Int }
                  deriving (Eq, Num)

instance QC.Arbitrary ConstExpr where
  arbitrary = ConstExpr <$> positiveArb

-- | Type that represents the left hand side of an assignment, which can be a
-- concatenation such as in:
--
-- @
-- {a, b, c} = 32'h94238;
-- @
data LVal = RegId Identifier
          | RegExpr { _regExprId :: Identifier
                    , _regExpr   :: Expr
                    }
          | RegSize { _regSizeId  :: Identifier
                    , _regSizeMSB :: ConstExpr
                    , _regSizeLSB :: ConstExpr
                    }
          | RegConcat { _regConc :: [Expr] }
          deriving (Eq)

makeLenses ''LVal

instance QC.Arbitrary LVal where
  arbitrary = QC.oneof [ RegId <$> QC.arbitrary
                       , RegExpr <$> QC.arbitrary <*> QC.arbitrary
                       , RegSize <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
                       ]

-- | Different port direction that are supported in Verilog.
data PortDir = PortIn    -- ^ Input direction for port (@input@).
             | PortOut   -- ^ Output direction for port (@output@).
             | PortInOut -- ^ Inout direction for port (@inout@).
             deriving (Eq)

instance QC.Arbitrary PortDir where
  arbitrary = QC.elements [PortIn, PortOut, PortInOut]

-- | Currently, only @wire@ and @reg@ are supported, as the other net types are
-- not that common and not a priority.
data PortType = Wire
              | Reg { _regSigned :: Bool }
              deriving (Eq)

instance QC.Arbitrary PortType where
  arbitrary = QC.oneof [pure Wire, Reg <$> QC.arbitrary]

makeLenses ''PortType

-- | Port declaration. It contains information about the type of the port, the
-- size, and the port name. It used to also contain information about if it was
-- an input or output port. However, this is not always necessary and was more
-- cumbersome than useful, as a lot of ports can be declared without input and
-- output port.
--
-- This is now implemented inside 'ModDecl' itself, which uses a list of output
-- and input ports.
data Port = Port { _portType :: PortType
                 , _portSize :: Int
                 , _portName :: Identifier
                 } deriving (Eq)

makeLenses ''Port

instance QC.Arbitrary Port where
  arbitrary = Port <$> QC.arbitrary <*> positiveArb <*> QC.arbitrary

-- | This is currently a type because direct module declaration should also be
-- added:
--
-- @
-- mod a(.y(y1), .x1(x11), .x2(x22));
-- @
newtype ModConn = ModConn { _modConn :: Expr }
                deriving (Eq)

makeLenses ''ModConn

instance QC.Arbitrary ModConn where
  arbitrary = ModConn <$> QC.arbitrary

data Assign = Assign { _assignReg   :: LVal
                     , _assignDelay :: Maybe Delay
                     , _assignExpr  :: Expr
                     } deriving (Eq)

instance QC.Arbitrary Assign where
  arbitrary = Assign <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

data ContAssign = ContAssign { _contAssignNetLVal :: Identifier
                             , _contAssignExpr    :: Expr
                             } deriving (Eq)

makeLenses ''ContAssign

instance QC.Arbitrary ContAssign where
  arbitrary = ContAssign <$> QC.arbitrary <*> QC.arbitrary

-- | Stmnts in Verilog.
data Stmnt = TimeCtrl { _statDelay :: Delay
                      , _statDStat :: Maybe Stmnt
                      }                              -- ^ Time control (@#NUM@)
           | EventCtrl { _statEvent :: Event
                       , _statEStat :: Maybe Stmnt
                       }
           | SeqBlock { _statements :: [Stmnt] } -- ^ Sequential block (@begin ... end@)
           | BlockAssign Assign                      -- ^ blocking assignment (@=@)
           | NonBlockAssign Assign                   -- ^ Non blocking assignment (@<=@)
           | StatCA ContAssign                       -- ^ Stmnt continuous assignment. May not be correct.
           | TaskEnable Task
           | SysTaskEnable Task
           deriving (Eq)

instance Semigroup Stmnt where
  (SeqBlock a) <> (SeqBlock b) = SeqBlock $ a <> b
  (SeqBlock a) <> b = SeqBlock $ a <> [b]
  a <> (SeqBlock b) = SeqBlock $ a : b
  a <> b = SeqBlock [a, b]

instance Monoid Stmnt where
  mempty = SeqBlock []

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

instance QC.Arbitrary Stmnt where
  arbitrary = QC.sized statement

data Task = Task { _taskName :: Identifier
                 , _taskExpr :: [Expr]
                 } deriving (Eq)

makeLenses ''Task

instance QC.Arbitrary Task where
  arbitrary = Task <$> QC.arbitrary <*> QC.arbitrary

-- | Module item which is the body of the module expression.
data ModItem = ModCA ContAssign
             | ModInst { _modInstId    :: Identifier
                       , _modInstName  :: Identifier
                       , _modInstConns :: [ModConn]
                       }
             | Initial Stmnt
             | Always Stmnt
             | Decl { declDir  :: Maybe PortDir
                    , declPort :: Port
                    }
             deriving (Eq)

makeLenses ''ModItem

instance QC.Arbitrary ModItem where
  arbitrary = QC.oneof [ ModCA <$> QC.arbitrary
                       , ModInst <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
                       , Initial <$> QC.arbitrary
                       , Always <$> (EventCtrl <$> QC.arbitrary <*> QC.arbitrary)
                       , Decl <$> pure Nothing <*> QC.arbitrary
                       ]

-- | 'module' module_identifier [list_of_ports] ';' { module_item } 'end_module'
data ModDecl = ModDecl { _moduleId    :: Identifier
                       , _modOutPorts :: [Port]
                       , _modInPorts  :: [Port]
                       , _moduleItems :: [ModItem]
                       } deriving (Eq)

makeLenses ''ModDecl

modPortGen :: QC.Gen Port
modPortGen = QC.oneof
  [ Port Wire <$> positiveArb <*> QC.arbitrary
  , Port <$> (Reg <$> QC.arbitrary) <*> positiveArb <*> QC.arbitrary
  ]


instance QC.Arbitrary ModDecl where
  arbitrary = ModDecl <$> QC.arbitrary <*> QC.arbitrary
              <*> QC.listOf1 modPortGen <*> QC.arbitrary

-- | Description of the Verilog module.
newtype Description = Description { _getDescription :: ModDecl }
                    deriving (Eq)

makeLenses ''Description

instance QC.Arbitrary Description where
  arbitrary = Description <$> QC.arbitrary

-- | The complete sourcetext for the Verilog module.
newtype VerilogSrc = VerilogSrc { _getVerilogSrc :: [Description] }
                   deriving (Eq)

makeLenses ''VerilogSrc

instance Semigroup VerilogSrc where
  VerilogSrc a <> VerilogSrc b = VerilogSrc $ a ++ b

instance Monoid VerilogSrc where
  mempty = VerilogSrc []

instance QC.Arbitrary VerilogSrc where
  arbitrary = VerilogSrc <$> QC.arbitrary
