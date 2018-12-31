{-|
Module      : Test.VeriFuzz.Verilog.AST
Description : Definition of the Verilog AST types.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Poratbility : POSIX

Defines the types to build a Verilog AST.
-}

{-# LANGUAGE TemplateHaskell #-}

module Test.VeriFuzz.Verilog.AST where

import           Control.Lens
import qualified Data.Graph.Inductive       as G
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Test.QuickCheck            as QC
import           Test.VeriFuzz.Circuit
import           Test.VeriFuzz.Graph.Random

class Source a where
  genSource :: a -> Text

-- | Identifier in Verilog. This is just a string of characters that can either
-- be lowercase and uppercase for now. This might change in the future though,
-- as Verilog supports many more characters in Identifiers.
newtype Identifier = Identifier { _getIdentifier :: Text }
                   deriving (Show, Eq, Ord)

instance IsString Identifier where
  fromString = Identifier . T.pack

instance Semigroup Identifier where
  (Identifier a) <> (Identifier b) = Identifier (a <> b)

instance Monoid Identifier where
  mempty = Identifier mempty

newtype Delay = Delay { _delay :: Int }
                deriving (Show, Eq, Ord)

data Event = EId Identifier
           | EExpr Expr
           | EAll
           deriving (Show, Eq, Ord)

data RegLVal = RegId Identifier
             | RegExpr { _regExprId :: Identifier
                       , _regExpr   :: Expr
                       }
             | RegSize { _regSizeId  :: Identifier
                       , _regSizeMSB :: ConstExpr
                       , _regSizeLSB :: ConstExpr
                       }
             deriving (Show, Eq, Ord)

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
                    deriving (Show, Eq, Ord)

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
                   deriving (Show, Eq, Ord)

-- | Verilog expression, which can either be a primary expression, unary
-- expression, binary operator expression or a conditional expression.
data Expr = Number { _numSize :: Int
                   , _numVal  :: Int
                   }
          | Id { _exprId :: Identifier }
          | Concat { _concatExpr :: [Expr] }
          | UnOp { _exprUnOp       :: UnaryOperator
                       , _exprPrim :: Expr
                       }
          | BinOp { _exprLhs    :: Expr
                   , _exprBinOp :: BinaryOperator
                   , _exprRhs   :: Expr
                   }
          | Cond { _exprCond      :: Expr
                     , _exprTrue  :: Expr
                     , _exprFalse :: Expr
                     }
          | Str { _exprStr :: Text }
          deriving (Show, Eq, Ord)

instance Num Expr where
  a + b = BinOp a BinPlus b
  a - b = BinOp a BinMinus b
  a * b = BinOp a BinTimes b
  negate = UnOp UnMinus
  abs = undefined
  signum = undefined
  fromInteger = Number 32 . fromInteger

instance Semigroup Expr where
  a <> b = mconcat [a, b]

instance Monoid Expr where
  mempty = 0
  mconcat = Concat

newtype ConstExpr = ConstExpr { _constNum :: Int }
                  deriving (Show, Eq, Ord)

instance Num ConstExpr where
  ConstExpr a + ConstExpr b = ConstExpr $ a + b
  ConstExpr a * ConstExpr b = ConstExpr $ a * b
  ConstExpr a - ConstExpr b = ConstExpr $ a - b
  abs (ConstExpr a) = ConstExpr $ abs a
  signum (ConstExpr a) = ConstExpr $ signum a
  fromInteger = ConstExpr . fromInteger

-- | Different port direction that are supported in Verilog.
data PortDir = PortIn    -- ^ Input direction for port (@input@).
             | PortOut   -- ^ Output direction for port (@output@).
             | PortInOut -- ^ Inout direction for port (@inout@).
             deriving (Show, Eq, Ord)

data PortType = Wire
              | Reg { _regSigned :: Bool }
              deriving (Show, Eq, Ord)

-- | Port declaration.
data Port = Port { _portType :: PortType
                 , _portSize :: Int
                 , _portName :: Identifier
                 } deriving (Show, Eq, Ord)

newtype ModConn = ModConn { _modConn :: Expr }
                deriving (Show, Eq, Ord)

data Assign = Assign { _assignReg   :: RegLVal
                     , _assignDelay :: Maybe Delay
                     , _assignExpr  :: Expr
                     } deriving (Show, Eq, Ord)

data ContAssign = ContAssign { _contAssignNetLVal :: Identifier
                             , _contAssignExpr    :: Expr
                             } deriving (Show, Eq, Ord)

-- | Stmnts in Verilog.
data Stmnt = TimeCtrl { _statDelay     :: Delay
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
               | EmptyStat
               deriving (Show, Eq, Ord)

instance Semigroup Stmnt where
  a <> b = mconcat [a, b]

instance Monoid Stmnt where
  mempty = EmptyStat
  mconcat = SeqBlock

data Task = Task { _taskName :: Identifier
                 , _taskExpr :: [Expr]
                 } deriving (Show, Eq, Ord)

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
             deriving (Show, Eq, Ord)

-- | 'module' module_identifier [list_of_ports] ';' { module_item } 'end_module'
data ModDecl = ModDecl { _moduleId    :: Identifier
                       , _modOutPorts :: [Port]
                       , _modInPorts  :: [Port]
                       , _moduleItems :: [ModItem]
                       } deriving (Show, Eq, Ord)

-- | Description of the Verilog module.
newtype Description = Description { _getDescription :: ModDecl }
                    deriving (Show, Eq, Ord)

-- | The complete sourcetext for the Verilog module.
newtype VerilogSrc = VerilogSrc { _getVerilogSrc :: [Description] }
                   deriving (Show, Eq, Ord)

instance Semigroup VerilogSrc where
  VerilogSrc a <> VerilogSrc b = VerilogSrc $ a ++ b

instance Monoid VerilogSrc where
  mempty = VerilogSrc []

-- Traversal Instance

traverseExpr :: Traversal' Expr Expr
traverseExpr _ (Number s v)   = pure $ Number s v
traverseExpr _ (Id id)        = pure $ Id id
traverseExpr f (Concat e)     = Concat <$> (sequenceA $ f <$> e)
traverseExpr f (UnOp un e)    = UnOp un <$> f e
traverseExpr f (BinOp l op r) = BinOp <$> f l <*> pure op <*> f r
traverseExpr f (Cond c l r)   = Cond <$> f c <*> f l <*> f r

-- Create all the necessary lenses

makeLenses ''Identifier
makeLenses ''VerilogSrc
makeLenses ''Description
makeLenses ''ModDecl
makeLenses ''ModItem
makeLenses ''Port
makeLenses ''PortDir
makeLenses ''BinaryOperator
makeLenses ''UnaryOperator
makeLenses ''Expr
makeLenses ''ContAssign
makeLenses ''PortType

-- Make all the necessary prisms

makePrisms ''Expr
makePrisms ''ModItem
makePrisms ''ModConn
