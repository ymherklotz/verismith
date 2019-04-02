{-|
Module      : VeriFuzz.Verilog.AST
Description : Definition of the Verilog AST types.
Copyright   : (c) 2018-2019, Yann Herklotz
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Poratbility : POSIX

Defines the types to build a Verilog AST.
-}

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module VeriFuzz.Verilog.AST
    ( -- * Top level types
      Verilog(..)
    , getVerilog
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
    , Statement(..)
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
    , stmntCondExpr
    , stmntCondTrue
    , stmntCondFalse
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
    -- * Useful Lenses and Traversals
    , getModule
    , getSourceId
    -- * Arbitrary
    , Arb
    , arb
    , genPositive
    )
where

import           Control.Lens
import           Control.Monad      (replicateM)
import           Data.Data
import           Data.Data.Lens
import           Data.List.NonEmpty (toList)
import           Data.String        (IsString, fromString)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Traversable   (sequenceA)
import           Hedgehog           (Gen)
import qualified Hedgehog.Gen       as Hog
import qualified Hedgehog.Range     as Hog

-- | Identifier in Verilog. This is just a string of characters that can either
-- be lowercase and uppercase for now. This might change in the future though,
-- as Verilog supports many more characters in Identifiers.
newtype Identifier = Identifier { _getIdentifier :: Text }
                   deriving (Eq, Show, Ord, Data, IsString, Semigroup, Monoid)

-- | Verilog syntax for adding a delay, which is represented as @#num@.
newtype Delay = Delay { _getDelay :: Int }
                deriving (Eq, Show, Ord, Data, Num)

-- | Verilog syntax for an event, such as @\@x@, which is used for always blocks
data Event = EId {-# UNPACK #-} !Identifier
           | EExpr !Expr
           | EAll
           | EPosEdge {-# UNPACK #-} !Identifier
           | ENegEdge {-# UNPACK #-} !Identifier
           deriving (Eq, Show, Ord, Data)

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

data Function = SignedFunc
              | UnSignedFunc
              deriving (Eq, Show, Ord, Data)

-- | Verilog expression, which can either be a primary expression, unary
-- expression, binary operator expression or a conditional expression.
data Expr = Number { _exprSize :: {-# UNPACK #-} !Int
                   , _exprVal  :: Integer
                   }
          | Id { _exprId :: {-# UNPACK #-} !Identifier }
          | Concat { _exprConcat :: [Expr] }
          | UnOp { _exprUnOp :: !UnaryOperator
                 , _exprPrim :: Expr
                 }
          | BinOp { _exprLhs   :: Expr
                  , _exprBinOp :: !BinaryOperator
                  , _exprRhs   :: Expr
                  }
          | Cond { _exprCond  :: Expr
                 , _exprTrue  :: Expr
                 , _exprFalse :: Expr
                 }
          | Func { _exprFunc :: !Function
                 , _exprBody :: Expr
                 }
          | Str { _exprStr :: {-# UNPACK #-} !Text }
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

traverseExpr :: (Applicative f) => (Expr -> f Expr) -> Expr -> f Expr
traverseExpr f (Concat e   ) = Concat <$> sequenceA (f <$> e)
traverseExpr f (UnOp u e   ) = UnOp u <$> f e
traverseExpr f (BinOp l o r) = BinOp <$> f l <*> pure o <*> f r
traverseExpr f (Cond  c l r) = Cond <$> f c <*> f l <*> f r
traverseExpr f (Func fn e  ) = Func fn <$> f e
traverseExpr _ e             = pure e

-- | Constant expression, which are known before simulation at compilation time.
newtype ConstExpr = ConstExpr { _constNum :: Int }
                  deriving (Eq, Show, Ord, Data, Num)

data Task = Task { _taskName :: {-# UNPACK #-} !Identifier
                 , _taskExpr :: [Expr]
                 } deriving (Eq, Show, Ord, Data)

-- | Type that represents the left hand side of an assignment, which can be a
-- concatenation such as in:
--
-- @
-- {a, b, c} = 32'h94238;
-- @
data LVal = RegId { _regId :: {-# UNPACK #-} !Identifier }
          | RegExpr { _regExprId :: {-# UNPACK #-} !Identifier
                    , _regExpr   :: !Expr
                    }
          | RegSize { _regSizeId  :: {-# UNPACK #-} !Identifier
                    , _regSizeMSB :: !ConstExpr
                    , _regSizeLSB :: !ConstExpr
                    }
          | RegConcat { _regConc :: [Expr] }
          deriving (Eq, Show, Ord, Data)

instance IsString LVal where
  fromString = RegId . fromString

-- | Different port direction that are supported in Verilog.
data PortDir = PortIn    -- ^ Input direction for port (@input@).
             | PortOut   -- ^ Output direction for port (@output@).
             | PortInOut -- ^ Inout direction for port (@inout@).
             deriving (Eq, Show, Ord, Data)

-- | Currently, only @wire@ and @reg@ are supported, as the other net types are
-- not that common and not a priority.
data PortType = Wire
              | Reg
              deriving (Eq, Show, Ord, Data)

-- | Port declaration. It contains information about the type of the port, the
-- size, and the port name. It used to also contain information about if it was
-- an input or output port. However, this is not always necessary and was more
-- cumbersome than useful, as a lot of ports can be declared without input and
-- output port.
--
-- This is now implemented inside 'ModDecl' itself, which uses a list of output
-- and input ports.
data Port = Port { _portType   :: !PortType
                 , _portSigned :: !Bool
                 , _portSize   :: {-# UNPACK #-} !Int
                 , _portName   :: {-# UNPACK #-} !Identifier
                 } deriving (Eq, Show, Ord, Data)

-- | This is currently a type because direct module declaration should also be
-- added:
--
-- @
-- mod a(.y(y1), .x1(x11), .x2(x22));
-- @
data ModConn = ModConn { _modConn :: !Expr }
             | ModConnNamed { _modConnName :: {-# UNPACK #-} !Identifier
                            , _modExpr     :: !Expr
                            }
             deriving (Eq, Show, Ord, Data)

data Assign = Assign { _assignReg   :: !LVal
                     , _assignDelay :: !(Maybe Delay)
                     , _assignExpr  :: !Expr
                     } deriving (Eq, Show, Ord, Data)

data ContAssign = ContAssign { _contAssignNetLVal :: {-# UNPACK #-} !Identifier
                             , _contAssignExpr    :: !Expr
                             } deriving (Eq, Show, Ord, Data)

-- | Statements in Verilog.
data Statement = TimeCtrl { _statDelay :: {-# UNPACK #-} !Delay
                          , _statDStat :: Maybe Statement
                          }                                -- ^ Time control (@#NUM@)
           | EventCtrl { _statEvent :: !Event
                       , _statEStat :: Maybe Statement
                       }
           | SeqBlock { _statements   :: [Statement] }     -- ^ Sequential block (@begin ... end@)
           | BlockAssign { _stmntBA      :: !Assign }     -- ^ blocking assignment (@=@)
           | NonBlockAssign { _stmntNBA     :: !Assign }     -- ^ Non blocking assignment (@<=@)
           | StatCA { _stmntCA      :: !ContAssign } -- ^ Statement continuous assignment. May not be correct.
           | TaskEnable { _stmntTask    :: !Task }
           | SysTaskEnable { _stmntSysTask :: !Task }
           | CondStmnt { _stmntCondExpr  :: Expr
                       , _stmntCondTrue  :: Maybe Statement
                       , _stmntCondFalse :: Maybe Statement
                       }
           deriving (Eq, Show, Ord, Data)

instance Semigroup Statement where
  (SeqBlock a) <> (SeqBlock b) = SeqBlock $ a <> b
  (SeqBlock a) <> b = SeqBlock $ a <> [b]
  a <> (SeqBlock b) = SeqBlock $ a : b
  a <> b = SeqBlock [a, b]

instance Monoid Statement where
  mempty = SeqBlock []

-- | Module item which is the body of the module expression.
data ModItem = ModCA { _modContAssign :: !ContAssign }
             | ModInst { _modInstId    :: {-# UNPACK #-} !Identifier
                       , _modInstName  :: {-# UNPACK #-} !Identifier
                       , _modInstConns :: [ModConn]
                       }
             | Initial !Statement
             | Always !Statement
             | Decl { _declDir  :: !(Maybe PortDir)
                    , _declPort :: !Port
                    }
             deriving (Eq, Show, Ord, Data)

-- | 'module' module_identifier [list_of_ports] ';' { module_item } 'end_module'
data ModDecl = ModDecl { _modId       :: {-# UNPACK #-} !Identifier
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

-- | Description of the Verilog module.
newtype Description = Description { _getDescription :: ModDecl }
                    deriving (Eq, Show, Ord, Data)

-- | The complete sourcetext for the Verilog module.
newtype Verilog = Verilog { _getVerilog :: [Description] }
                   deriving (Eq, Show, Ord, Data, Semigroup, Monoid)

makeLenses ''Identifier
makeLenses ''Delay
makeLenses ''Expr
makeLenses ''ConstExpr
makeLenses ''Task
makeLenses ''LVal
makeLenses ''PortType
makeLenses ''Port
makeLenses ''ModConn
makeLenses ''Assign
makeLenses ''ContAssign
makeLenses ''Statement
makeLenses ''ModItem
makeLenses ''ModDecl
makeLenses ''Description
makeLenses ''Verilog

getModule :: Traversal' Verilog ModDecl
getModule = getVerilog . traverse . getDescription
{-# INLINE getModule #-}

getSourceId :: Traversal' Verilog Text
getSourceId = getModule . modId . getIdentifier
{-# INLINE getSourceId #-}

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

instance Arb Int where
    arb = Hog.int (Hog.linear 0 100)

instance Arb ConstExpr where
    arb = ConstExpr <$> Hog.int (Hog.linear 0 100)

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
    -- , StatCA <$> arb
        , TaskEnable <$> arb
        , SysTaskEnable <$> arb
        ]
    | n > 0 = Hog.choice
        [ TimeCtrl <$> arb <*> (Just <$> substat 2)
        , SeqBlock <$> listOf1 (substat 4)
        , BlockAssign <$> arb
        , NonBlockAssign <$> arb
    -- , StatCA <$> arb
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

instance Arb ModDecl where
    arb = ModDecl <$> arb <*> listOf arb <*> listOf1 modPortGen <*> listOf arb

instance Arb Description where
    arb = Description <$> arb

instance Arb Verilog where
    arb = Verilog <$> listOf1 arb

instance Arb Bool where
    arb = Hog.element [True, False]
