{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Verismith.Verilog.AST
-- Description : Definition of the Verilog AST types.
-- Copyright   : (c) 2018-2019, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Poratbility : POSIX
--
-- Defines the types to build a Verilog AST.
module Verismith.Verilog.AST
  ( -- * Top level types
    SourceInfo (..),
    infoTop,
    infoSrc,
    Verilog (..),

    -- * Primitives

    -- ** Identifier
    Identifier (..),

    -- ** Control
    Delay (..),
    Event (..),

    -- ** Operators
    BinaryOperator (..),
    UnaryOperator (..),

    -- ** Task
    Task (..),
    taskName,
    taskExpr,

    -- ** Left hand side value
    LVal (..),
    regId,
    regExprId,
    regExpr,
    regSizeId,
    regSizeRange,
    regConc,

    -- ** Ports
    PortDir (..),
    PortType (..),
    Port (..),
    portType,
    portSigned,
    portSize,
    portName,

    -- * Expression
    Expr (..),
    _Id,
    ConstExpr (..),
    ConstExprF (..),
    constToExpr,
    exprToConst,
    Range (..),
    constNum,
    constParamId,
    constConcat,
    constUnOp,
    constPrim,
    constLhs,
    constBinOp,
    constRhs,
    constCond,
    constTrue,
    constFalse,
    constStr,

    -- * Assignment
    Assign (..),
    assignReg,
    assignDelay,
    assignExpr,
    ContAssign (..),
    contAssignNetLVal,
    contAssignExpr,

    -- ** Parameters
    Parameter (..),
    paramIdent,
    paramValue,
    LocalParam (..),
    localParamIdent,
    localParamValue,

    -- * Statment
    CaseType (..),
    CasePair (..),
    Statement (..),
    statDelay,
    statDStat,
    statEvent,
    statEStat,
    statements,
    stmntBA,
    stmntNBA,
    stmntTask,
    stmntSysTask,
    stmntCondExpr,
    stmntCondTrue,
    stmntCondFalse,
    stmntCaseType,
    stmntCaseExpr,
    stmntCasePair,
    stmntCaseDefault,
    forAssign,
    forExpr,
    forIncr,
    forStmnt,

    -- * Module
    ModDecl (..),
    modId,
    modOutPorts,
    modInPorts,
    modItems,
    modParams,
    _ModDeclAnn,
    _ModDecl,
    ModItem (..),
    modContAssign,
    modInstId,
    modInstName,
    modInstConns,
    _Initial,
    _Always,
    paramDecl,
    localParamDecl,
    traverseModItem,
    declDir,
    declPort,
    declVal,
    ModConn (..),
    modConnName,
    modExpr,

    -- * Useful Lenses and Traversals
    aModule,
    getModule,
    getSourceId,
    mainModule,
    Annotations (..),
  )
where

import Control.DeepSeq (NFData)
import Control.Lens hiding ((<|))
import Data.Data
import Data.Data.Lens
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List.NonEmpty ((<|), NonEmpty (..))
import Data.String (IsString, fromString)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Verismith.Verilog.BitVec

class Functor m => Annotations m where
  removeAnn :: m a -> m a
  clearAnn :: m a -> m ()
  clearAnn = fmap (\_ -> ()) . removeAnn
  collectAnn :: m a -> [a]

-- | Identifier in Verilog. This is just a string of characters that can either
-- be lowercase and uppercase for now. This might change in the future though,
-- as Verilog supports many more characters in Identifiers.
newtype Identifier = Identifier {getIdentifier :: Text}
  deriving (Eq, Show, Ord, Data, Generic, NFData)

$(makeWrapped ''Identifier)

instance IsString Identifier where
  fromString = Identifier . pack

instance Semigroup Identifier where
  Identifier a <> Identifier b = Identifier $ a <> b

instance Monoid Identifier where
  mempty = Identifier mempty

-- | Verilog syntax for adding a delay, which is represented as @#num@.
newtype Delay = Delay {_getDelay :: Int}
  deriving (Eq, Show, Ord, Data, Generic, NFData)

$(makeWrapped ''Delay)

instance Num Delay where
  Delay a + Delay b = Delay $ a + b
  Delay a - Delay b = Delay $ a - b
  Delay a * Delay b = Delay $ a * b
  negate (Delay a) = Delay $ negate a
  abs (Delay a) = Delay $ abs a
  signum (Delay a) = Delay $ signum a
  fromInteger = Delay . fromInteger

-- | Binary operators that are currently supported in the verilog generation.
data BinaryOperator
  = BinPlus
  | BinMinus
  | BinTimes
  | BinDiv
  | BinMod
  | BinEq
  | BinNEq
  | BinCEq
  | BinCNEq
  | BinLAnd
  | BinLOr
  | BinLT
  | BinLEq
  | BinGT
  | BinGEq
  | BinAnd
  | BinOr
  | BinXor
  | BinXNor
  | BinXNorInv
  | BinPower
  | BinLSL
  | BinLSR
  | BinASL
  | BinASR
  deriving (Eq, Show, Ord, Data, Generic, NFData)

-- | Unary operators that are currently supported by the generator.
data UnaryOperator
  = UnPlus
  | UnMinus
  | UnLNot
  | UnNot
  | UnAnd
  | UnNand
  | UnOr
  | UnNor
  | UnXor
  | UnNxor
  | UnNxorInv
  deriving (Eq, Show, Ord, Data, Generic, NFData)

-- | Constant expression, which are known before simulation at compile time.
data ConstExpr
  = ConstNum
      { _constNum :: {-# UNPACK #-} !BitVec
      }
  | ParamId
      { _constParamId :: {-# UNPACK #-} !Identifier
      }
  | ConstConcat
      { _constConcat :: !(NonEmpty ConstExpr)
      }
  | ConstUnOp
      { _constUnOp :: !UnaryOperator,
        _constPrim :: !ConstExpr
      }
  | ConstBinOp
      { _constLhs :: !ConstExpr,
        _constBinOp :: !BinaryOperator,
        _constRhs :: !ConstExpr
      }
  | ConstCond
      { _constCond :: !ConstExpr,
        _constTrue :: !ConstExpr,
        _constFalse :: !ConstExpr
      }
  | ConstStr
      { _constStr :: {-# UNPACK #-} !Text
      }
  deriving (Eq, Show, Ord, Data, Generic, NFData)

$(makeLenses ''ConstExpr)

$(makeBaseFunctor ''ConstExpr)

constToExpr :: ConstExpr -> Expr
constToExpr (ConstNum a) = Number a
constToExpr (ParamId a) = Id a
constToExpr (ConstConcat a) = Concat $ fmap constToExpr a
constToExpr (ConstUnOp a b) = UnOp a $ constToExpr b
constToExpr (ConstBinOp a b c) = BinOp (constToExpr a) b $ constToExpr c
constToExpr (ConstCond a b c) =
  Cond (constToExpr a) (constToExpr b) $ constToExpr c
constToExpr (ConstStr a) = Str a

exprToConst :: Expr -> ConstExpr
exprToConst (Number a) = ConstNum a
exprToConst (Id a) = ParamId a
exprToConst (Concat a) = ConstConcat $ fmap exprToConst a
exprToConst (UnOp a b) = ConstUnOp a $ exprToConst b
exprToConst (BinOp a b c) = ConstBinOp (exprToConst a) b $ exprToConst c
exprToConst (Cond a b c) =
  ConstCond (exprToConst a) (exprToConst b) $ exprToConst c
exprToConst (Str a) = ConstStr a
exprToConst _ = error "Not a constant expression"

instance Num ConstExpr where
  a + b = ConstBinOp a BinPlus b
  a - b = ConstBinOp a BinMinus b
  a * b = ConstBinOp a BinTimes b
  negate = ConstUnOp UnMinus
  abs = undefined
  signum = undefined
  fromInteger = ConstNum . fromInteger

instance Semigroup ConstExpr where
  (ConstConcat a) <> (ConstConcat b) = ConstConcat $ a <> b
  (ConstConcat a) <> b = ConstConcat $ a <> (b :| [])
  a <> (ConstConcat b) = ConstConcat $ a <| b
  a <> b = ConstConcat $ a <| b :| []

instance Monoid ConstExpr where
  mempty = ConstNum 0

instance IsString ConstExpr where
  fromString = ConstStr . fromString

instance Plated ConstExpr where
  plate = uniplate

-- | Range that can be associated with any port or left hand side. Contains the
-- msb and lsb bits as 'ConstExpr'. This means that they can be generated using
-- parameters, which can in turn be changed at synthesis time.
data Range
  = Range
      { rangeMSB :: !ConstExpr,
        rangeLSB :: !ConstExpr
      }
  deriving (Eq, Show, Ord, Data, Generic, NFData)

instance Num Range where
  (Range s1 a) + (Range s2 b) = Range (s1 + s2) $ a + b
  (Range s1 a) - (Range s2 b) = Range (s1 - s2) . max 0 $ a - b
  (Range s1 a) * (Range s2 b) = Range (s1 * s2) $ a * b
  negate = undefined
  abs = id
  signum _ = 1
  fromInteger = flip Range 0 . fromInteger . (-) 1

-- | Verilog expression, which can either be a primary expression, unary
-- expression, binary operator expression or a conditional expression.
data Expr
  = Number {-# UNPACK #-} !BitVec
  | Id {-# UNPACK #-} !Identifier
  | VecSelect {-# UNPACK #-} !Identifier !Expr
  | RangeSelect {-# UNPACK #-} !Identifier !Range
  | Concat !(NonEmpty Expr)
  | UnOp !UnaryOperator !Expr
  | BinOp !Expr !BinaryOperator !Expr
  | Cond !Expr !Expr !Expr
  | Appl !Identifier !Expr
  | Str {-# UNPACK #-} !Text
  deriving (Eq, Show, Ord, Data, Generic, NFData)

$(makeLenses ''Expr)
$(makePrisms ''Expr)

$(makeBaseFunctor ''Expr)

instance Num Expr where
  a + b = BinOp a BinPlus b
  a - b = BinOp a BinMinus b
  a * b = BinOp a BinTimes b
  negate = UnOp UnMinus
  abs = undefined
  signum = undefined
  fromInteger = Number . fromInteger

instance Semigroup Expr where
  (Concat a) <> (Concat b) = Concat $ a <> b
  (Concat a) <> b = Concat $ a <> (b :| [])
  a <> (Concat b) = Concat $ a <| b
  a <> b = Concat $ a <| b :| []

instance Monoid Expr where
  mempty = Number 0

instance IsString Expr where
  fromString = Str . fromString

instance Plated Expr where
  plate = uniplate

-- | Verilog syntax for an event, such as @\@x@, which is used for always blocks
data Event
  = EId {-# UNPACK #-} !Identifier
  | EExpr !Expr
  | EAll
  | EPosEdge {-# UNPACK #-} !Identifier
  | ENegEdge {-# UNPACK #-} !Identifier
  | EOr !Event !Event
  | EComb !Event !Event
  deriving (Eq, Show, Ord, Data, Generic, NFData)

$(makeBaseFunctor ''Event)

instance Plated Event where
  plate = uniplate

-- | Task call, which is similar to function calls.
data Task
  = Task
      { _taskName :: {-# UNPACK #-} !Identifier,
        _taskExpr :: [Expr]
      }
  deriving (Eq, Show, Ord, Data, Generic, NFData)

$(makeLenses ''Task)

-- | Type that represents the left hand side of an assignment, which can be a
-- concatenation such as in:
--
-- @
-- {a, b, c} = 32'h94238;
-- @
data LVal
  = RegId
      { _regId :: {-# UNPACK #-} !Identifier
      }
  | RegExpr
      { _regExprId :: {-# UNPACK #-} !Identifier,
        _regExpr :: !Expr
      }
  | RegSize
      { _regSizeId :: {-# UNPACK #-} !Identifier,
        _regSizeRange :: {-# UNPACK #-} !Range
      }
  | RegConcat
      { _regConc :: [Expr]
      }
  deriving (Eq, Show, Ord, Data, Generic, NFData)

$(makeLenses ''LVal)

instance IsString LVal where
  fromString = RegId . fromString

-- | Different port direction that are supported in Verilog.
data PortDir
  = PortIn
  | PortOut
  | PortInOut
  deriving (Eq, Show, Ord, Data, Generic, NFData)

-- | Currently, only @wire@ and @reg@ are supported, as the other net types are
-- not that common and not a priority.
data PortType
  = Wire
  | Reg
  deriving (Eq, Show, Ord, Data, Generic, NFData)

$(makeLenses ''PortType)

-- | Port declaration. It contains information about the type of the port, the
-- size, and the port name. It used to also contain information about if it was
-- an input or output port. However, this is not always necessary and was more
-- cumbersome than useful, as a lot of ports can be declared without input and
-- output port.
--
-- This is now implemented inside '(ModDecl ann)' itself, which uses a list of output
-- and input ports.
data Port
  = Port
      { _portType :: !PortType,
        _portSigned :: !Bool,
        _portSize :: {-# UNPACK #-} !Range,
        _portName :: {-# UNPACK #-} !Identifier
      }
  deriving (Eq, Show, Ord, Data, Generic, NFData)

$(makeLenses ''Port)

-- | This is currently a type because direct module declaration should also be
-- added:
--
-- @
-- mod a(.y(y1), .x1(x11), .x2(x22));
-- @
data ModConn
  = ModConn
      { _modExpr :: !Expr
      }
  | ModConnNamed
      { _modConnName :: {-# UNPACK #-} !Identifier,
        _modExpr :: !Expr
      }
  deriving (Eq, Show, Ord, Data, Generic, NFData)

$(makeLenses ''ModConn)

data Assign
  = Assign
      { _assignReg :: !LVal,
        _assignDelay :: !(Maybe Delay),
        _assignExpr :: !Expr
      }
  deriving (Eq, Show, Ord, Data, Generic, NFData)

$(makeLenses ''Assign)

-- | Type for continuous assignment.
--
-- @
-- assign x = 2'b1;
-- @
data ContAssign
  = ContAssign
      { _contAssignNetLVal :: {-# UNPACK #-} !Identifier,
        _contAssignExpr :: !Expr
      }
  deriving (Eq, Show, Ord, Data, Generic, NFData)

$(makeLenses ''ContAssign)

-- | Case pair which contains an expression followed by a statement which will
-- get executed if the expression matches the expression in the case statement.
data CasePair a
  = CasePair
      { _casePairExpr :: !Expr,
        _casePairStmnt :: !(Statement a)
      }
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

traverseStmntCasePair ::
  Functor f =>
  (Statement a1 -> f (Statement a2)) ->
  CasePair a1 ->
  f (CasePair a2)
traverseStmntCasePair f (CasePair a s) = CasePair a <$> f s

-- | Type of case statement, which determines how it is interpreted.
data CaseType
  = CaseStandard
  | CaseX
  | CaseZ
  deriving (Eq, Show, Ord, Data, Generic, NFData)

-- | Statements in Verilog.
data Statement a
  = -- | Time control (@#NUM@)
    TimeCtrl
      { _statDelay :: {-# UNPACK #-} !Delay,
        _statDStat :: Maybe (Statement a)
      }
  | EventCtrl
      { _statEvent :: !Event,
        _statEStat :: Maybe (Statement a)
      }
  | -- | Sequential block (@begin ... end@)
    SeqBlock {_statements :: [Statement a]}
  | -- | blocking assignment (@=@)
    BlockAssign {_stmntBA :: !Assign}
  | -- | Non blocking assignment (@<=@)
    NonBlockAssign {_stmntNBA :: !Assign}
  | TaskEnable {_stmntTask :: !Task}
  | SysTaskEnable {_stmntSysTask :: !Task}
  | CondStmnt
      { _stmntCondExpr :: Expr,
        _stmntCondTrue :: Maybe (Statement a),
        _stmntCondFalse :: Maybe (Statement a)
      }
  | StmntCase
      { _stmntCaseType :: !CaseType,
        _stmntCaseExpr :: !Expr,
        _stmntCasePair :: ![CasePair a],
        _stmntCaseDefault :: !(Maybe (Statement a))
      }
  | -- | Loop bounds shall be statically computable for a for loop.
    ForLoop
      { _forAssign :: !Assign,
        _forExpr :: Expr,
        _forIncr :: !Assign,
        _forStmnt :: Statement a
      }
  | StmntAnn a (Statement a)
  deriving (Eq, Show, Ord, Data, Functor, Generic, NFData)

$(makeLenses ''Statement)

instance Plated (Statement a) where
  plate f (TimeCtrl d s) = TimeCtrl d <$> traverse f s
  plate f (EventCtrl d s) = EventCtrl d <$> traverse f s
  plate f (SeqBlock s) = SeqBlock <$> traverse f s
  plate f (CondStmnt e s1 s2) = CondStmnt e <$> traverse f s1 <*> traverse f s2
  plate f (StmntCase a b c d) =
    StmntCase a b <$> traverse (traverseStmntCasePair f) c
      <*> traverse f d
  plate f (ForLoop a b c d) = ForLoop a b c <$> f d
  plate _ a = pure a

instance Semigroup (Statement a) where
  (SeqBlock a) <> (SeqBlock b) = SeqBlock $ a <> b
  (SeqBlock a) <> b = SeqBlock $ a <> [b]
  a <> (SeqBlock b) = SeqBlock $ a : b
  a <> b = SeqBlock [a, b]

instance Monoid (Statement a) where
  mempty = SeqBlock []

instance Annotations Statement where
  removeAnn (StmntAnn _ s) = removeAnn s
  removeAnn (TimeCtrl e s) = TimeCtrl e $ fmap removeAnn s
  removeAnn (EventCtrl e s) = EventCtrl e $ fmap removeAnn s
  removeAnn (SeqBlock s) = SeqBlock $ fmap removeAnn s
  removeAnn (CondStmnt c ms1 ms2) = CondStmnt c (fmap removeAnn ms1) $ fmap removeAnn ms2
  removeAnn (StmntCase ct ce cp cdef) = StmntCase ct ce (fmap removeAnn cp) $ fmap removeAnn cdef
  removeAnn (ForLoop a b c s) = ForLoop a b c $ removeAnn s
  removeAnn s = s
  collectAnn (StmntAnn _ s) = collectAnn s
  collectAnn (TimeCtrl _ s) = concatMap collectAnn s
  collectAnn (EventCtrl _ s) = concatMap collectAnn s
  collectAnn (SeqBlock s) = concatMap collectAnn s
  collectAnn (CondStmnt _ ms1 ms2) = concatMap collectAnn ms1 <> concatMap collectAnn ms2
  collectAnn (StmntCase _ _ cp cdef) =  concatMap collectAnn cp <> concatMap collectAnn cdef
  collectAnn (ForLoop _ _ _ s) = collectAnn s
  collectAnn _ = []

instance Annotations CasePair where
  removeAnn (CasePair e s) = CasePair e $ removeAnn s
  collectAnn (CasePair _ s) = collectAnn s

-- | Parameter that can be assigned in blocks or modules using @parameter@.
data Parameter
  = Parameter
      { _paramIdent :: {-# UNPACK #-} !Identifier,
        _paramValue :: ConstExpr
      }
  deriving (Eq, Show, Ord, Data, Generic, NFData)

$(makeLenses ''Parameter)

-- | Local parameter that can be assigned anywhere using @localparam@. It cannot
-- be changed by initialising the module.
data LocalParam
  = LocalParam
      { _localParamIdent :: {-# UNPACK #-} !Identifier,
        _localParamValue :: ConstExpr
      }
  deriving (Eq, Show, Ord, Data, Generic, NFData)

$(makeLenses ''LocalParam)

-- | Module item which is the body of the module expression.
data ModItem a
  = ModCA {_modContAssign :: !ContAssign}
  | ModInst
      { _modInstId :: {-# UNPACK #-} !Identifier,
        _modInstDecl :: [ModConn],
        _modInstName :: {-# UNPACK #-} !Identifier,
        _modInstConns :: [ModConn]
      }
  | Initial !(Statement a)
  | Always !(Statement a)
  | Property
      { _moditemPropLabel :: {-# UNPACK #-} !Identifier,
        _moditemPropEvent :: !Event,
        _moditemPropBodyL :: Maybe Expr,
        _moditemPropBodyR :: Expr
      }
  | Decl
      { _declDir :: !(Maybe PortDir),
        _declPort :: !Port,
        _declVal :: Maybe ConstExpr
      }
  | ParamDecl {_paramDecl :: NonEmpty Parameter}
  | LocalParamDecl {_localParamDecl :: NonEmpty LocalParam}
  | ModItemAnn a (ModItem a)
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

$(makePrisms ''ModItem)

$(makeLenses ''ModItem)

instance Annotations ModItem where
  removeAnn (ModItemAnn _ mi) = removeAnn mi
  removeAnn (Initial s) = Initial $ removeAnn s
  removeAnn (Always s) = Always $ removeAnn s
  removeAnn mi = mi
  collectAnn (ModItemAnn _ mi) = collectAnn mi
  collectAnn (Initial s) = collectAnn s
  collectAnn (Always s) = collectAnn s
  collectAnn mi = []

-- | 'module' module_identifier [list_of_ports] ';' { module_item } 'end_module'
data ModDecl a
  = ModDecl
      { _modId :: {-# UNPACK #-} !Identifier,
        _modOutPorts :: ![Port],
        _modInPorts :: ![Port],
        _modItems :: ![ModItem a],
        _modParams :: ![Parameter]
      }
  | ModDeclAnn a (ModDecl a)
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

instance Plated (ModDecl a) where
  plate f (ModDeclAnn b m) = ModDeclAnn b <$> plate f m
  plate _ m = pure m

$(makeLenses ''ModDecl)
$(makePrisms ''ModDecl)

instance Annotations ModDecl where
  removeAnn (ModDecl i out inp mis params) = ModDecl i out inp (fmap removeAnn mis) params
  removeAnn (ModDeclAnn _ mi) = mi
  collectAnn (ModDecl _ _ _ mis _) = concatMap collectAnn mis
  collectAnn (ModDeclAnn a mi) = a : collectAnn mi

traverseModConn :: (Applicative f) => (Expr -> f Expr) -> ModConn -> f ModConn
traverseModConn f (ModConn e) = ModConn <$> f e
traverseModConn f (ModConnNamed a e) = ModConnNamed a <$> f e

traverseModItem :: (Applicative f) => (Expr -> f Expr) -> (ModItem ann) -> f (ModItem ann)
traverseModItem f (ModCA (ContAssign a e)) = ModCA . ContAssign a <$> f e
traverseModItem f (ModInst a b c e) =
  ModInst a b c <$> sequenceA (traverseModConn f <$> e)
traverseModItem _ e = pure e

-- | The complete sourcetext for the Verilog module.
newtype Verilog a = Verilog {getVerilog :: [ModDecl a]}
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

$(makeWrapped ''Verilog)

instance Semigroup (Verilog a) where
  Verilog a <> Verilog b = Verilog $ a <> b

instance Monoid (Verilog a) where
  mempty = Verilog mempty

instance Annotations Verilog where
  removeAnn (Verilog v) = Verilog $ fmap removeAnn v

-- | Top level type which contains all the source code and associated
-- information.
data SourceInfo a
  = SourceInfo
      { _infoTop :: {-# UNPACK #-} !Text,
        _infoSrc :: !(Verilog a)
      }
  deriving (Eq, Show, Ord, Functor, Data, Generic, NFData)

$(makeLenses ''SourceInfo)

instance Semigroup (SourceInfo a) where
  (SourceInfo t v) <> (SourceInfo _ v2) = SourceInfo t $ v <> v2

instance Monoid (SourceInfo a) where
  mempty = SourceInfo mempty mempty

instance Annotations SourceInfo where
  removeAnn (SourceInfo t v) = SourceInfo t $ removeAnn v

-- | Attributes which can be set to various nodes in the AST.
--
-- @
-- (* synthesis *)
-- @
data Attribute
  = AttrAssign Identifier ConstExpr
  | AttrName Identifier
  deriving (Eq, Show, Ord, Data, Generic, NFData)

-- | Annotations which can be added to the AST. These are supported in all the
-- nodes of the AST and a custom type can be declared for them.
data Annotation a
  = Ann a
  | AnnAttrs [Attribute]
  deriving (Eq, Show, Ord, Data, Generic, NFData)

getModule :: Traversal' (Verilog a) (ModDecl a)
getModule = _Wrapped . traverse
{-# INLINE getModule #-}

getSourceId :: Traversal' (Verilog a) Text
getSourceId = getModule . modId . _Wrapped
{-# INLINE getSourceId #-}

-- | May need to change this to Traversal to be safe. For now it will fail when
-- the main has not been properly set with.
aModule :: Identifier -> Lens' (SourceInfo a) (ModDecl a)
aModule t = lens get_ set_
  where
    set_ (SourceInfo top main) v =
      SourceInfo top (main & getModule %~ update (getIdentifier t) v)
    update top v m@(ModDecl (Identifier i) _ _ _ _)
      | i == top = v
      | otherwise = m
    update top v (ModDeclAnn _ m) = update top v m
    get_ (SourceInfo _ main) =
      head . filter (f $ getIdentifier t) $ main ^.. getModule
    f top (ModDecl (Identifier i) _ _ _ _) = i == top
    f top (ModDeclAnn _ m) = f top m

-- | May need to change this to Traversal to be safe. For now it will fail when
-- the main has not been properly set with.
mainModule :: Lens' (SourceInfo a) (ModDecl a)
mainModule = lens get_ set_
  where
    set_ (SourceInfo top main) v =
      SourceInfo top (main & getModule %~ update top v)
    update top v m@(ModDecl (Identifier i) _ _ _ _)
      | i == top = v
      | otherwise = m
    update top v (ModDeclAnn _ m) = update top v m
    get_ (SourceInfo top main) = head . filter (f top) $ main ^.. getModule
    f top (ModDecl (Identifier i) _ _ _ _) = i == top
    f top (ModDeclAnn _ m) = f top m
