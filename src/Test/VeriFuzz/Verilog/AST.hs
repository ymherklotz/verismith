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

-- | 'Source' class which determines that source code is able to be generated
-- from the data structure using 'genSource'. This will be stored in 'Text' and
-- can then be processed further.
class Source a where
  genSource :: a -> Text

-- | Identifier in Verilog. This is just a string of characters that can either
-- be lowercase and uppercase for now. This might change in the future though,
-- as Verilog supports many more characters in Identifiers.
newtype Identifier = Identifier { _getIdentifier :: Text }
                   deriving (Eq)

instance IsString Identifier where
  fromString = Identifier . T.pack

instance Semigroup Identifier where
  (Identifier a) <> (Identifier b) = Identifier (a <> b)

instance Monoid Identifier where
  mempty = Identifier mempty

-- | Verilog syntax for adding a delay, which is represented as @#num@.
newtype Delay = Delay { _delay :: Int }
                deriving (Eq)

-- | Verilog syntax for an event, such as @\@x@, which is used for always blocks
data Event = EId Identifier
           | EExpr Expr
           | EAll
           deriving (Eq)

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

-- | Verilog expression, which can either be a primary expression, unary
-- expression, binary operator expression or a conditional expression.
data Expr = Number { _numSize :: Int
                   , _numVal  :: Int
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
  mempty = 0

instance IsString Expr where
  fromString = Str . fromString

-- | Constant expression, which are known before simulation at compilation time.
newtype ConstExpr = ConstExpr { _constNum :: Int }
                  deriving (Eq)

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
             deriving (Eq)

-- | Currently, only @wire@ and @reg@ are supported, as the other net types are
-- not that common and not a priority.
data PortType = Wire
              | Reg { _regSigned :: Bool }
              deriving (Eq)

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

-- | This is currently a type because direct module declaration should also be
-- added:
--
-- @
-- mod a(.y(y1), .x1(x11), .x2(x22));
-- @
newtype ModConn = ModConn { _modConn :: Expr }
                deriving (Eq)

data Assign = Assign { _assignReg   :: LVal
                     , _assignDelay :: Maybe Delay
                     , _assignExpr  :: Expr
                     } deriving (Eq)

data ContAssign = ContAssign { _contAssignNetLVal :: Identifier
                             , _contAssignExpr    :: Expr
                             } deriving (Eq)

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
               deriving (Eq)

instance Semigroup Stmnt where
  (SeqBlock a) <> (SeqBlock b) = SeqBlock $ a <> b
  (SeqBlock a) <> b = SeqBlock $ a <> [b]
  a <> (SeqBlock b) = SeqBlock $ a : b
  a <> b = SeqBlock [a, b]

instance Monoid Stmnt where
  mempty = EmptyStat

data Task = Task { _taskName :: Identifier
                 , _taskExpr :: [Expr]
                 } deriving (Eq)

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

-- | 'module' module_identifier [list_of_ports] ';' { module_item } 'end_module'
data ModDecl = ModDecl { _moduleId    :: Identifier
                       , _modOutPorts :: [Port]
                       , _modInPorts  :: [Port]
                       , _moduleItems :: [ModItem]
                       } deriving (Eq)

-- | Description of the Verilog module.
newtype Description = Description { _getDescription :: ModDecl }
                    deriving (Eq)

-- | The complete sourcetext for the Verilog module.
newtype VerilogSrc = VerilogSrc { _getVerilogSrc :: [Description] }
                   deriving (Eq)

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
