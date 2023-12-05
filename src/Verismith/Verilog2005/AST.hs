-- Module      : Verismith.Verilog2005.AST
-- Description : Partial Verilog 2005 AST.
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Verismith.Verilog2005.AST
  ( GenMinTypMax (..),
    MinTypMax,
    CMinTypMax,
    Identified (..),
    identIdent,
    defaultIdM,
    UnaryOperator (..),
    BinaryOperator (..),
    Number (..),
    GenPrim (..),
    HierIdent (..),
    hiPath,
    GenDimRange (..),
    DimRange (..),
    CDimRange (..),
    GenExpr (..),
    Expr (..),
    CExpr (..),
    Attribute (..),
    Attributed (..),
    AttrIded (..),
    aiData,
    Dir (..),
    Range2 (..),
    GenRangeExpr (..),
    RangeExpr,
    CRangeExpr,
    NumIdent (..),
    Delay3 (..),
    Delay2 (..),
    Delay1 (..),
    SignRange (..),
    EventPrefix (..),
    SpecTerm (..),
    ComType (..),
    FunParType (..),
    NetType (..),
    Strength (..),
    DriveStrength (..),
    dsDefault,
    ChargeStrength (..),
    LValue (..),
    NetLValue,
    VarLValue,
    NetAssign (..),
    VarAssign (..),
    ParamAssign (..),
    PortAssign (..),
    EventPrim (..),
    EventControl (..),
    DelayEventControl (..),
    ProcContAssign (..),
    LoopStatement (..),
    CaseItem (..),
    Statement (..),
    AttrStmt,
    MybStmt,
    NInputType (..),
    GateInst (..),
    EdgeDesc (..),
    TimingCheckEvent (..),
    ControlledTimingCheckEvent (..),
    STCArgs (..),
    STCAddArgs (..),
    SystemTimingCheck (..),
    ModulePathCondition (..),
    SpecPath (..),
    SpecifyItem (..),
    ParamOver (..),
    poIdent,
    SpecParamDecl (..),
    SpecParam (..),
    NetKind (..),
    BlockDecl (..),
    StdBlockDecl,
    TaskFunType (..),
    ModGenDecl (..),
    GenCaseItem (..),
    ModGenItem (..),
    ModuleItem (..),
    Parameter (..),
    PortDecl (..),
    GenerateRegion (..),
    GenerateItem (..),
    GenerateBlock (..),
    ModuleBlock (..),
    mbIdent,
    SigLevel (..),
    ZOX (..),
    CombRow (..),
    Edge (..),
    SeqIn (..),
    SeqRow (..),
    PrimTable (..),
    PrimitiveBlock (..),
    pbIdent,
    Dot1Ident (..),
    LLU (..),
    ConfigItem (..),
    ciCell_inst,
    ConfigBlock (..),
    cbIdent,
    cbBody,
    Verilog2005 (..),
    SourceInfo (..),
    SystemFunction (..),
    Logic (..),
    sfMap,
    BXZ (..),
    OXZ (..),
    HXZ (..),
  )
where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Data
import Data.Data.Lens
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Vector.Unboxed as V
import GHC.Generics (Generic)
import Numeric.Natural
import Verismith.Verilog2005.Token (BXZ (..), HXZ (..), OXZ (..), ZOX (..))

-- | Minimum, Typical, Maximum
data GenMinTypMax et
  = MTMSingle !et
  | MTMFull
      { _MTMMin :: !et,
        _MTMTyp :: !et,
        _MTMMax :: !et
      }
  deriving (Show, Eq, Ord, Data, Generic)

type CMinTypMax = GenMinTypMax CExpr

type MinTypMax = GenMinTypMax Expr

-- | Quickly add an identifier to all members of a sum type, other uses are discouraged
data Identified t = Identified {_identIdent :: !ByteString, _identData :: !t}
  deriving (Show, Eq, Ord, Data, Generic)

defaultIdM = Identified "" Nothing

-- | Unary operators
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
  | UnXNor
  deriving (Eq, Ord, Data, Generic, Enum, Bounded)

instance Show UnaryOperator where
  show x = case x of
    UnPlus -> "+"
    UnMinus -> "-"
    UnLNot -> "!"
    UnNot -> "~"
    UnAnd -> "&"
    UnNand -> "~&"
    UnOr -> "|"
    UnNor -> "~|"
    UnXor -> "^"
    UnXNor -> "~^"

-- | Binary operators
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
  | BinPower
  | BinLSL
  | BinLSR
  | BinASL
  | BinASR
  deriving (Eq, Ord, Data, Generic, Enum, Bounded)

instance Show BinaryOperator where
  show x = case x of
    BinPlus -> "+"
    BinMinus -> "-"
    BinTimes -> "*"
    BinDiv -> "/"
    BinMod -> "%"
    BinEq -> "=="
    BinNEq -> "!="
    BinCEq -> "==="
    BinCNEq -> "!=="
    BinLAnd -> "&&"
    BinLOr -> "||"
    BinLT -> "<"
    BinLEq -> "<="
    BinGT -> ">"
    BinGEq -> ">="
    BinAnd -> "&"
    BinOr -> "|"
    BinXor -> "^"
    BinXNor -> "~^"
    BinPower -> "**"
    BinLSL -> "<<"
    BinLSR -> ">>"
    BinASL -> "<<<"
    BinASR -> ">>>"

data Number
  = NBinary !(NonEmpty BXZ)
  | NOctal !(NonEmpty OXZ)
  | NDecimal !Natural
  | NHex !(NonEmpty HXZ)
  | NXZ !Bool
  deriving (Show, Eq, Ord, Data, Generic)

-- | Parametric primary expression
data GenPrim i r
  = PrimNumber
      { _PNSize :: !(Maybe Natural),
        _PNSigned :: !Bool,
        _PNValue :: !Number
      }
  | PrimReal !ByteString
  | PrimIdent
      { _PIIdent :: !i,
        _PISub :: !r
      }
  | PrimConcat !(NonEmpty (GenExpr i r))
  | PrimMultConcat
      { _PMCMul :: !CExpr,
        _PMCExpr :: !(NonEmpty (GenExpr i r))
      }
  | PrimFun
      { _PFIdent :: !i,
        _PFAttr :: ![Attribute],
        _PFArg :: ![GenExpr i r]
      }
  | PrimSysFun
      { _PSFIdent :: !ByteString,
        _PSFArg :: ![GenExpr i r]
      }
  | PrimMinTypMax !(GenMinTypMax (GenExpr i r))
  | PrimString !ByteString
  deriving (Show, Eq, Ord, Data, Generic)

-- | Hierarchical identifier
data HierIdent = HierIdent {_hiPath :: ![Identified (Maybe CExpr)], _hiIdent :: !ByteString}
  deriving (Show, Eq, Ord, Data, Generic)

-- | Indexing for dimension and range
data GenDimRange e = GenDimRange {_GDRDim :: ![e], _GDRRange :: !(GenRangeExpr e)}
  deriving (Show, Eq, Ord, Data, Generic)

newtype DimRange = DimRange (GenDimRange Expr)
  deriving (Show, Eq, Ord, Data, Generic)

newtype CDimRange = CDimRange (GenDimRange CExpr)
  deriving (Show, Eq, Ord, Data, Generic)

-- | Parametric expression
data GenExpr i r
  = ExprPrim !(GenPrim i r)
  | ExprUnOp
      { _EUOp :: !UnaryOperator,
        _EUAttr :: ![Attribute],
        _EUPrim :: !(GenPrim i r)
      }
  | ExprBinOp
      { _EBLhs :: !(GenExpr i r),
        _EBOp :: !BinaryOperator,
        _EBAttr :: ![Attribute],
        _EBRhs :: !(GenExpr i r)
      }
  | ExprCond
      { _ECCond :: !(GenExpr i r),
        _ECAttr :: ![Attribute],
        _ECTrue :: !(GenExpr i r),
        _ECFalse :: !(GenExpr i r)
      }
  deriving (Show, Eq, Ord, Data, Generic)

newtype CExpr = CExpr (GenExpr ByteString (Maybe CRangeExpr))
  deriving (Show, Eq, Ord, Data, Generic)

newtype Expr = Expr (GenExpr HierIdent (Maybe DimRange))
  deriving (Show, Eq, Ord, Data, Generic)

-- | Attributes which can be set to various nodes in the AST.
type Attribute = Identified (Maybe CExpr)

data Attributed t = Attributed {_attrAttr :: ![Attribute], _attrData :: !t}
  deriving (Show, Eq, Ord, Data, Generic)

data AttrIded t = AttrIded {_aiAttr :: ![Attribute], _aiIdent :: !ByteString, _aiData :: !t}
  deriving (Show, Eq, Ord, Data, Generic)

-- | Directions
data Dir = DirIn | DirOut | DirInOut
  deriving (Eq, Ord, Bounded, Enum, Data, Generic)

instance Show Dir where
  show x = case x of DirIn -> "input"; DirOut -> "output"; DirInOut -> "inout"

-- | Range2
data Range2 = Range2 {_R2MSB :: !CExpr, _R2LSB :: !CExpr}
  deriving (Show, Eq, Ord, Data, Generic)

-- | Range expressions
data GenRangeExpr e
  = GRESingle !e
  | GREPair !Range2
  | GREBaseOff
      { _GREBase :: !e,
        _GREMin_plus :: !Bool,
        _GREOffset :: !CExpr
      }
  deriving (Show, Eq, Ord, Data, Generic)

type RangeExpr = GenRangeExpr Expr

type CRangeExpr = GenRangeExpr CExpr

-- | Number or Identifier
data NumIdent
  = NIIdent !ByteString
  | NIReal !ByteString
  | NINumber !Natural
  deriving (Show, Eq, Ord, Data, Generic)

-- | Delay3
data Delay3
  = D3Base !NumIdent
  | D31 !MinTypMax
  | D32 !MinTypMax !MinTypMax
  | D33 !MinTypMax !MinTypMax !MinTypMax
  deriving (Show, Eq, Ord, Data, Generic)

-- | Delay2
data Delay2
  = D2Base !NumIdent
  | D21 !MinTypMax
  | D22 !MinTypMax !MinTypMax
  deriving (Show, Eq, Ord, Data, Generic)

-- | Delay1
data Delay1
  = D1Base !NumIdent
  | D11 !MinTypMax
  deriving (Show, Eq, Ord, Data, Generic)

-- | Signedness and range are often together
data SignRange = SignRange {_SRSign :: !Bool, _SRRange :: !(Maybe Range2)}
  deriving (Show, Eq, Ord, Data, Generic)

-- | Event expression prefix
data EventPrefix = EPAny | EPPos | EPNeg
  deriving (Show, Eq, Ord, Bounded, Enum, Data, Generic)

-- | Specify terminal
type SpecTerm = Identified (Maybe CRangeExpr)

-- | Common types for variables, parameters, functions and tasks
data ComType = CTInteger | CTReal | CTRealtime | CTTime
  deriving (Eq, Ord, Bounded, Enum, Data, Generic)

instance Show ComType where
  show x = case x of
    CTInteger -> "integer"
    CTReal -> "real"
    CTRealtime -> "realtime"
    CTTime -> "time"

-- | Function and Parameter type
data FunParType
  = FPTComType !ComType
  | FPTSignRange !SignRange
  deriving (Show, Eq, Ord, Data, Generic)

-- | Net type
data NetType
  = NTSupply1
  | NTSupply0
  | NTTri
  | NTTriAnd
  | NTTriOr
  | NTTri1
  | NTTri0
  | NTUwire
  | NTWire
  | NTWAnd
  | NTWOr
  deriving (Eq, Ord, Bounded, Enum, Data, Generic)

instance Show NetType where
  show x = case x of
    NTSupply1 -> "supply1"
    NTSupply0 -> "supply0"
    NTTri -> "tri"
    NTTriAnd -> "triand"
    NTTriOr -> "trior"
    NTTri1 -> "tri1"
    NTTri0 -> "tri0"
    NTUwire -> "uwire"
    NTWire -> "wire"
    NTWAnd -> "wand"
    NTWOr -> "wor"

-- | Net drive strengths
data Strength = StrSupply | StrStrong {-default-} | StrPull | StrWeak
  deriving (Eq, Ord, Bounded, Enum, Data, Generic)

instance Show Strength where
  show x = case x of
    StrSupply -> "supply"
    StrStrong -> "strong"
    StrPull -> "pull"
    StrWeak -> "weak"

data DriveStrength
  = DSNormal
      { _DS0 :: !Strength,
        _DS1 :: !Strength
      }
  | DSHighZ
      { _DSHZ :: !Bool,
        _DSStr :: !Strength
      }
  deriving (Show, Eq, Ord, Data, Generic)

dsDefault = DSNormal {_DS0 = StrStrong, _DS1 = StrStrong}

-- | Capacitor charge
data ChargeStrength = CSSmall | CSMedium {-default-} | CSLarge
  deriving (Eq, Ord, Bounded, Enum, Data, Generic)

instance Show ChargeStrength where
  show x = case x of CSSmall -> "(small)"; CSMedium -> "(medium)"; CSLarge -> "(large)"

-- | Left side of assignments
data LValue dr
  = LVSingle
      { _LVIdent :: !HierIdent,
        _LVDimRange :: !(Maybe dr)
      }
  | LVConcat !(NonEmpty (LValue dr))
  deriving (Show, Eq, Ord, Data, Generic)

type NetLValue = LValue CDimRange

type VarLValue = LValue DimRange

-- | Net assignment
data NetAssign = NetAssign {_NALValue :: !NetLValue, _NAValue :: !Expr}
  deriving (Show, Eq, Ord, Data, Generic)

-- | Variable assignment
data VarAssign = VarAssign {_VALValue :: !VarLValue, _VAValue :: !Expr}
  deriving (Show, Eq, Ord, Data, Generic)

-- | Parameter assignment list
data ParamAssign
  = ParamPositional ![Expr]
  | ParamNamed ![Identified (Maybe MinTypMax)]
  deriving (Show, Eq, Ord, Data, Generic)

-- | Port assignment list
data PortAssign
  = PortNamed ![AttrIded (Maybe Expr)]
  | PortPositional ![Attributed (Maybe Expr)]
  deriving (Show, Eq, Ord, Data, Generic)

-- | Event primitive
data EventPrim = EventPrim {_EPOp :: !EventPrefix, _EPExpr :: !Expr}
  deriving (Show, Eq, Ord, Data, Generic)

-- | Event control
data EventControl
  = ECIdent !HierIdent
  | ECExpr !(NonEmpty EventPrim)
  | ECDeps
  deriving (Show, Eq, Ord, Data, Generic)

-- | Delay or Event control
data DelayEventControl
  = DECDelay !Delay1
  | DECEvent !EventControl
  | DECRepeat
      { _DECRExpr :: !Expr,
        _DECREvent :: !EventControl
      }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Procedural continuous assignment
data ProcContAssign
  = PCAAssign !VarAssign
  | PCADeassign !VarLValue
  | PCAForce !(Either VarAssign NetAssign)
  | PCARelease !(Either VarLValue NetLValue)
  deriving (Show, Eq, Ord, Data, Generic)

-- | Loop statement
data LoopStatement
  = LSForever
  | LSRepeat !Expr
  | LSWhile !Expr
  | LSFor
      { _LSFInit :: !VarAssign,
        _LSFCond :: !Expr,
        _LSFUpd :: !VarAssign
      }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Case item
data CaseItem = CaseItem {_CIPat :: !(NonEmpty Expr), _CIVal :: !MybStmt}
  deriving (Show, Eq, Ord, Data, Generic)

-- | Statement
data Statement
  = SBlockAssign
      { _SBABlock :: !Bool,
        _SBAAssign :: !VarAssign,
        _SBADelev :: !(Maybe DelayEventControl)
      }
  | SProcContAssign !ProcContAssign
  | SCase
      { _SCType :: !ZOX,
        _SCExpr :: !Expr,
        _SCBody :: ![CaseItem],
        _SCDef :: !MybStmt
      }
  | SIf
      { _SIExpr :: !Expr,
        _SITrue :: !MybStmt,
        _SIFalse :: !MybStmt
      }
  | SDisable !HierIdent
  | SProcTimingControl
      { _SPTCControl :: !(Either Delay1 EventControl),
        _SPTCStmt :: !MybStmt
      }
  | SEventTrigger
      { _SETIdent :: !HierIdent,
        _SETIndex :: ![Expr]
      }
  | SWait
      { _SWExpr :: !Expr,
        _SWStmt :: !MybStmt
      }
  | SLoop
      { _SLHead :: !LoopStatement,
        _SLBody :: !AttrStmt
      }
  | SBlock
      { _SBHeader :: !(Maybe (ByteString, [Parameter], [Parameter], StdBlockDecl)),
        _SBPar_seq :: !Bool,
        _SBStmt :: ![AttrStmt]
      }
  | SSysTaskEnable
      { _SSTEIdent :: !ByteString,
        _SSTEArgs :: ![Maybe Expr]
      }
  | STaskEnable
      { _STEIdent :: !HierIdent,
        _STEArgs :: ![Expr]
      }
  deriving (Show, Eq, Ord, Data, Generic)

type AttrStmt = Attributed Statement

type MybStmt = Attributed (Maybe Statement)

-- | N-input logic gate types
data NInputType = NITAnd | NITOr | NITXor
  deriving (Eq, Ord, Bounded, Enum, Data, Generic)

instance Show NInputType where
  show x = case x of NITAnd -> "and"; NITOr -> "or"; NITXor -> "xor"

-- | Gate instantiation
data GateInst
  = GICMos
      { _GICMR :: !Bool,
        _GICMDelay :: !(Maybe Delay3),
        _GICMOutput :: !NetLValue,
        _GICMInput :: !Expr,
        _GICMNControl :: !Expr,
        _GICMPControl :: !Expr
      }
  | GIEnable
      { _GIER :: !Bool,
        _GIE1_0 :: !Bool,
        _GIEStrength :: !DriveStrength,
        _GIEDelay :: !(Maybe Delay3),
        _GIEOutput :: !NetLValue,
        _GIEInput :: !Expr,
        _GIEEnable :: !Expr
      }
  | GIMos
      { _GIMR :: !Bool,
        _GIMN_P :: !Bool,
        _GIMDelay :: !(Maybe Delay3),
        _GIMOutput :: !NetLValue,
        _GIMInput :: !Expr,
        _GIMEnable :: !Expr
      }
  | GINIn
      { _GINIType :: !NInputType,
        _GININ :: !Bool,
        _GINIStrength :: !DriveStrength,
        _GINIDelay :: !(Maybe Delay2),
        _GINIOutput :: !NetLValue,
        _GINIInput :: !(NonEmpty Expr)
      }
  | GINOut
      { _GINOR :: !Bool,
        _GINOStrength :: !DriveStrength,
        _GINODelay :: !(Maybe Delay2),
        _GINOOutput :: !(NonEmpty NetLValue),
        _GINOInput :: !Expr
      }
  | GIPassEn
      { _GIPER :: !Bool,
        _GIPE1_0 :: !Bool,
        _GIPEDelay :: !(Maybe Delay2),
        _GIPELhs :: !NetLValue,
        _GIPERhs :: !NetLValue,
        _GIPEEnable :: !Expr
      }
  | GIPass
      { _GIPR :: !Bool,
        _GIPLhs :: !NetLValue,
        _GIPRhs :: !NetLValue
      }
  | GIPull
      { _GIPUp_down :: !Bool,
        _GIPStrength :: !DriveStrength,
        _GIPOutput :: !NetLValue
      }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Edge descriptor, a 10 Bool array (01, 0x, 0z, 10, 1x, 1z, x0, x1, z0, z1)
type EdgeDesc = V.Vector Bool

-- | Timing check (controlled) event
data TimingCheckEvent = TimingCheckEvent
  { _TCEEvCtl :: !(Maybe EdgeDesc),
    _TCESpecTerm :: !SpecTerm,
    _TCETimChkCond :: !(Maybe (Bool, Expr))
  }
  deriving (Show, Eq, Ord, Data, Generic)

data ControlledTimingCheckEvent = ControlledTimingCheckEvent
  { _CTCEEvCtl :: !EdgeDesc,
    _CTCESpecTerm :: !SpecTerm,
    _CTCETimChkCond :: !(Maybe (Bool, Expr))
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | System timing check common arguments
data STCArgs = STCArgs
  { _STCADataEvent :: !TimingCheckEvent,
    _STCARefEvent :: !TimingCheckEvent,
    _STCATimChkLim :: !Expr,
    _STCANotifier :: !ByteString
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Setuphold and Recrem additionnal arguments
data STCAddArgs = STCAddArgs
  { _STCAATimChkLim :: !Expr,
    _STCAAStampCond :: !(Maybe MinTypMax),
    _STCAAChkTimCond :: !(Maybe MinTypMax),
    _STCAADelayedRef :: !(Identified (Maybe CMinTypMax)),
    _STCAADelayedData :: !(Identified (Maybe CMinTypMax))
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | System timing check
data SystemTimingCheck
  = STCSetup !STCArgs
  | STCHold !STCArgs
  | STCSetupHold
      { _STCSHArgs :: !STCArgs,
        _STCSHAddArgs :: !STCAddArgs
      }
  | STCRecovery !STCArgs
  | STCRemoval !STCArgs
  | STCRecrem
      { _STCRArgs :: !STCArgs,
        _STCRAddArgs :: !STCAddArgs
      }
  | STCSkew !STCArgs
  | STCTimeSkew
      { _STCTSArgs :: !STCArgs,
        _STCTSEvBased :: !(Maybe CExpr),
        _STCTSRemActive :: !(Maybe CExpr)
      }
  | STCFullSkew
      { _STCFSArgs :: !STCArgs,
        _STCFSTimChkLim :: !Expr,
        _STCFSEvBased :: !(Maybe CExpr),
        _STCFSRemActive :: !(Maybe CExpr)
      }
  | STCPeriod
      { _STCPCRefEvent :: !ControlledTimingCheckEvent,
        _STCPTimCtlLim :: !Expr,
        _STCPNotif :: !ByteString
      }
  | STCWidth
      { _STCWRefEvent :: !ControlledTimingCheckEvent,
        _STCWTimCtlLim :: !Expr,
        _STCWThresh :: !(Maybe CExpr),
        _STCWNotif :: !ByteString
      }
  | STCNoChange
      { _STCNCRefEvent :: !TimingCheckEvent,
        _STCNCDataEvent :: !TimingCheckEvent,
        _STCNCStartEdgeOff :: !MinTypMax,
        _STCNCEndEdgeOff :: !MinTypMax,
        _STCNCNotif :: !ByteString
      }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Module path condition
data ModulePathCondition
  = MPCCond !(GenExpr ByteString ())
  | MPCNone
  | MPCAlways
  deriving (Show, Eq, Ord, Data, Generic)

-- | Specify path declaration
data SpecPath
  = SPParallel
      { _SPPInput :: !SpecTerm,
        _SPPOutput :: !SpecTerm
      }
  | SPFull
      { _SPFInput :: !(NonEmpty SpecTerm),
        _SPFOutput :: !(NonEmpty SpecTerm)
      }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Specify block item
data SpecifyItem
  = SIPulsestyle
      { _SIPEvent_detect :: !Bool,
        _SIPPathOutput :: !SpecTerm
      }
  | SIShowcancelled
      { _SISCancelled :: !Bool,
        _SISPathOutput :: !SpecTerm
      }
  | SIPathDeclaration
      { _SIPDCond :: !ModulePathCondition,
        _SIPDConn :: !SpecPath,
        _SIPDPolarity :: !(Maybe Bool),
        _SIPDEDS :: !(Maybe (Expr, Maybe Bool)),
        _SIPDValue :: !(NonEmpty CMinTypMax) -- length 1, 2, 3, 6 or 12
      }
  | SISystemTimingCheck !SystemTimingCheck
  deriving (Show, Eq, Ord, Data, Generic)

-- | Parameter override
data ParamOver = ParamOver
  { _poAttr :: ![Attribute],
    _poIdent :: !HierIdent,
    _poValue :: !CMinTypMax
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Specparam declaration
data SpecParamDecl
  = SPDAssign !(Identified CMinTypMax)
  | SPDPathPulse -- Not accurate input/output as it is ambiguous
      { _SPDPInput :: !SpecTerm,
        _SPDPOutput :: !SpecTerm,
        _SPDPReject :: !CMinTypMax,
        _SPDPError :: !CMinTypMax
      }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Module or Generate specify parameters
data SpecParam = SpecParam
  { _SPRange :: !(Maybe Range2),
    _SPParam :: !SpecParamDecl
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Net type + trireg variants
data NetKind
  = NKNet
      { _NKNType :: !NetType,
        _NKNDrive :: !(Either [Range2] (DriveStrength, Expr))
      }
  | NKTriD
      { _NKTDDrive :: !DriveStrength,
        _NKTDValue :: !Expr
      }
  | NKTriC
      { _NKTCCharge :: !ChargeStrength,
        _NKTCDim :: ![Range2]
      }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Block declaration
data BlockDecl t
  = BDReg
      { _BDRgSR :: !SignRange,
        _BDRgData :: !t
      }
  | BDInt !t
  | BDReal !t
  | BDTime !t
  | BDRealTime !t
  | BDEvent ![Range2]
  deriving (Show, Eq, Ord, Data, Generic)

type StdBlockDecl = [AttrIded (BlockDecl [Range2])]

-- | Task and Function argument types
data TaskFunType
  = TFTRegSignRange
      { _TFTRSRReg :: !Bool,
        _TFTRSRSignRange :: !SignRange
      }
  | TFTComType !ComType
  deriving (Show, Eq, Ord, Data, Generic)

-- | Module or Generate declaration
data ModGenDecl
  = MGDBlockDecl !(BlockDecl (Either [Range2] CExpr))
  | MGDNet
      { _MGDNType :: !NetKind,
        _MGDNSigned :: !Bool,
        _MGDNVector :: !(Maybe (Maybe Bool, Range2)),
        _MGDNDelay :: !(Maybe Delay3)
      }
  | MGDGenVar
  | MGDTask
      { _MGDTAuto :: !Bool,
        _MGDTPort :: ![AttrIded (Dir, TaskFunType)],
        _MGDTParam :: ![Parameter],
        _MGDTLocalParam :: ![Parameter],
        _MGDTDecl :: !StdBlockDecl,
        _MGDTBody :: !MybStmt
      }
  | -- TODO: Function statement because they're restricted
    MGDFunc
      { _MGDFAuto :: !Bool,
        _MGDFType :: !(Maybe FunParType),
        _MGDFPort :: ![AttrIded TaskFunType],
        _MGDFParam :: ![Parameter],
        _MGDFLocalParam :: ![Parameter],
        _MGDFDecl :: !StdBlockDecl,
        _MGDFBody :: !Statement
      }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Case generate branch
data GenCaseItem = GenCaseItem {_GCIPat :: !(NonEmpty CExpr), _GCIVal :: !(Maybe GenerateBlock)}
  deriving (Show, Eq, Ord, Data, Generic)

-- | Module or Generate item
data ModGenItem
  = MGIContAss
      { _MGICAStrength :: !DriveStrength,
        _MGICADelay :: !(Maybe Delay3),
        _MGICAAssign :: !NetAssign
      }
  | MGIGateInst
      { _MGIGIIdent :: !ByteString,
        _MGIGIRange :: !(Maybe Range2),
        _MGIGIInst :: !GateInst
      }
  | MGIUDPInst
      { _MGIUDPIUDP :: !ByteString,
        _MGIUDPIStrength :: !DriveStrength,
        _MGIUDPIDelay :: !(Maybe Delay2),
        _MGIUDPIIdent :: !ByteString,
        _MGIUDPIRange :: !(Maybe Range2),
        _MGIUDPILValue :: !NetLValue,
        _MGIUDPIArgs :: !(NonEmpty Expr)
      }
  | MGIModInst
      { _MGIMIMod :: !ByteString,
        _MGIMIParams :: !ParamAssign,
        _MGIMIIdent :: !ByteString,
        _MGIMIRange :: !(Maybe Range2),
        _MGIMIPort :: !PortAssign
      }
  | MGIUnknownInst -- Sometimes identifying what is instantiated is impossible
      { _MGIUIType :: !ByteString,
        _MGIUIParam :: !(Maybe (Either Expr (Expr, Expr))),
        _MGIUIIdent :: !ByteString,
        _MGIUIRange :: !(Maybe Range2),
        _MGIUIArg0 :: !NetLValue,
        _MGIUIArgs :: !(NonEmpty Expr)
      }
  | MGIInitial !AttrStmt
  | MGIAlways !AttrStmt
  | MGILoopGen
      { _MGILGInit :: !(Identified CExpr),
        _MGILGCond :: !CExpr,
        _MGILGUpd :: !(Identified CExpr),
        _MGILGBody :: !GenerateBlock
      }
  | MGIIf
      { _MGIIExpr :: !CExpr,
        _MGIITrue :: !(Maybe GenerateBlock),
        _MGIIFalse :: !(Maybe GenerateBlock)
      }
  | MGICase
      { _MGICExpr :: !CExpr,
        _MGICBranch :: ![GenCaseItem],
        _MGICDefault :: !(Maybe GenerateBlock)
      }
  deriving (Show, Eq, Ord, Data, Generic)

instance Plated ModGenItem where
  plate = uniplate

-- | Module item: body of module
data ModuleItem
  = MIMGI !(Attributed ModGenItem)
  | MIGenReg !GenerateRegion
  | MISpecBlock
      { _MISBSpecParam :: ![SpecParam],
        _MISBBody :: ![SpecifyItem]
      }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Parameter
data Parameter = Parameter
  { _paramAttr :: ![Attribute],
    _paramIdent :: !ByteString,
    _paramType :: !FunParType,
    _paramValue :: !CMinTypMax
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Module port declaration
data PortDecl
  = PDIn
      { _PDIType :: !(Maybe NetType),
        _PDISR :: !SignRange
      }
  | PDInOut
      { _PDIOType :: !(Maybe NetType),
        _PDIOSR :: !SignRange
      }
  | PDOut
      { _PDOType :: !(Maybe NetType),
        _PDOSR :: !SignRange
      }
  | PDOutReg
      { _PDORSR :: !SignRange,
        _PDORVal :: !(Maybe CExpr)
      }
  | PDOutVar
      { _PDOVInt_time :: !Bool,
        _PDOVVal :: !(Maybe CExpr)
      }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Generate block without name
data GenerateRegion = GenerateRegion
  { _GRLocalParam :: ![Parameter],
    _GRDefParam :: ![ParamOver],
    _GRDecl :: ![AttrIded ModGenDecl],
    _GRBody :: ![Attributed ModGenItem]
  }
  deriving (Show, Eq, Ord, Data, Generic)

instance Semigroup GenerateRegion where
  (<>) gra grb =
    GenerateRegion
      { _GRLocalParam = _GRLocalParam gra <> _GRLocalParam grb,
        _GRDefParam = _GRDefParam gra <> _GRDefParam grb,
        _GRDecl = _GRDecl gra <> _GRDecl grb,
        _GRBody = _GRBody gra <> _GRBody grb
      }

instance Monoid GenerateRegion where
  mempty = GenerateRegion [] [] [] []

-- | Generate region single item
data GenerateItem
  = GIParam !(NonEmpty Parameter)
  | GIParamOver !(NonEmpty ParamOver)
  | GIMGD !(NonEmpty (AttrIded ModGenDecl))
  | GIMGI !(NonEmpty (Attributed ModGenItem))
  deriving (Show, Eq, Ord, Data, Generic)

-- | GenerateBlock
data GenerateBlock = GBSingle !GenerateItem | GBBlock !(Identified GenerateRegion)
  deriving (Show, Eq, Ord, Data, Generic)

-- TODO upwards

-- $(makeLenses '')

-- $(makePrisms '')

-- | Module block
data ModuleBlock = ModuleBlock
  { _mbAttr :: ![Attribute],
    _mbIdent :: !ByteString,
    _mbPortInter :: ![Identified [Identified (Maybe CRangeExpr)]],
    _mbPortDecl :: ![AttrIded PortDecl],
    _mbParam :: ![Parameter],
    _mbLocalParam :: ![Parameter],
    _mbDecl :: ![AttrIded ModGenDecl],
    _mbSpecParam :: ![Attributed SpecParam],
    _mbBody :: ![ModuleItem],
    _mbTimescale :: !(Maybe (Int, Int)),
    _mbCell :: !Bool,
    _mbPull :: !(Maybe Bool),
    _mbDefNetType :: !(Maybe NetType)
  }
  deriving (Show, Eq, Ord, Data, Generic)

instance Semigroup ModuleBlock where
  (<>) mba mbb =
    mba
      { _mbPortDecl = _mbPortDecl mba <> _mbPortDecl mbb,
        _mbParam = _mbParam mba <> _mbParam mbb,
        _mbLocalParam = _mbLocalParam mba <> _mbLocalParam mbb,
        _mbDecl = _mbDecl mba <> _mbDecl mbb,
        _mbSpecParam = _mbSpecParam mba <> _mbSpecParam mbb,
        _mbBody = _mbBody mba <> _mbBody mbb
      }

instance Monoid ModuleBlock where
  mempty = ModuleBlock [] "" [] [] [] [] [] [] [] Nothing False Nothing (Just NTWire)

-- | Signal level
data SigLevel = L0 | L1 | LX | LQ | LB
  deriving (Eq, Ord, Bounded, Enum, Data, Generic)

instance Show SigLevel where
  show x = case x of L0 -> "0"; L1 -> "1"; LX -> "x"; LQ -> "?"; LB -> "b"

-- | Combinatorial table row
data CombRow = CombRow {_crInput :: !(NonEmpty SigLevel), _crOutput :: !ZOX}
  deriving (Show, Eq, Ord, Data, Generic)

-- | Edge specifier
data Edge
  = EdgePos_neg !Bool
  | EdgeDesc
      { _EDFrom :: !SigLevel,
        _EDTo :: !SigLevel
      }
  deriving (Eq, Ord, Data, Generic)

instance Show Edge where
  show x = case x of
    EdgePos_neg b -> if b then "p" else "n"
    EdgeDesc {_EDFrom = f, _EDTo = t} -> '(' : show f ++ show t ++ ")"

-- | Seqential table inputs: a list of input levels with at most 1 edge specifier
data SeqIn
  = SIComb !(NonEmpty SigLevel)
  | SISeq ![SigLevel] !Edge ![SigLevel]
  deriving (Eq, Ord, Data, Generic)

instance Show SeqIn where
  show x = case x of
    SIComb l -> concatMap show l
    SISeq l0 e l1 -> concatMap show l0 ++ show e ++ concatMap show l1

-- | Sequential table row
data SeqRow = SeqRow
  { _srowInput :: !SeqIn,
    _srowState :: !SigLevel,
    _srowNext :: !(Maybe ZOX)
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Primitive transition table
data PrimTable
  = CombTable !(NonEmpty CombRow)
  | SeqTable
      { _stReg :: !(Either CExpr [Attribute]),
        _stInit :: !(Maybe ZOX),
        _stRow :: !(NonEmpty SeqRow)
      }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Primitive block
data PrimitiveBlock = PrimitiveBlock
  { _pbAttr :: ![Attribute],
    _pbIdent :: !ByteString,
    _pbOutput :: !([Attribute], ByteString),
    _pbInput :: !(NonEmpty ([Attribute], ByteString)),
    _pbBody :: !PrimTable
  }
  deriving (Show, Eq, Ord, Data, Generic)

data Dot1Ident = Dot1Ident {_d1iLib :: !(Maybe ByteString), _d1iCell :: !ByteString}
  deriving (Show, Eq, Ord, Data, Generic)

-- | Liblist or Use
data LLU
  = LLULiblist ![ByteString]
  | LLUUse
      { _lluUIdent :: !Dot1Ident,
        _lluUConfig :: !Bool
      }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Items in a config block
data ConfigItem = ConfigItem
  { _ciCell_inst :: !(Either Dot1Ident (NonEmpty ByteString)),
    _ciLLU :: !LLU
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Config Block: Identifier, Design lines, Configuration items
data ConfigBlock = ConfigBlock
  { _cbIdent :: !ByteString,
    _cbDesign :: ![Dot1Ident],
    _cbBody :: ![ConfigItem],
    _cbDef :: ![ByteString]
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Internal representation of Verilog2005 AST
data Verilog2005 = Verilog2005
  { _vModule :: ![ModuleBlock],
    _vDefParam :: ![ParamOver],
    _vPrimitive :: ![PrimitiveBlock],
    _vConfig :: ![ConfigBlock]
  }
  deriving (Show, Eq, Ord, Data, Generic)

instance Semigroup Verilog2005 where
  (<>) v2a v2b =
    v2a
      { _vModule = _vModule v2a <> _vModule v2b,
        _vDefParam = _vDefParam v2a <> _vDefParam v2b,
        _vPrimitive = _vPrimitive v2a <> _vPrimitive v2b,
        _vConfig = _vConfig v2a <> _vConfig v2b
      }

instance Monoid Verilog2005 where
  mempty = Verilog2005 [] [] [] []

data SourceInfo = SourceInfo
  { _srcTopModule :: ByteString,
    _srcSource :: Verilog2005
  }

$(makeLenses ''Identified)
$(makeLenses ''HierIdent)
$(makeLenses ''Attributed)
$(makeLenses ''AttrIded)
$(makeLenses ''ParamOver)
$(makeLenses ''Parameter)
$(makeLenses ''ModuleBlock)
$(makeLenses ''CombRow)
$(makeLenses ''SeqRow)
$(makeLenses ''PrimTable)
$(makePrisms ''PrimTable)
$(makeLenses ''PrimitiveBlock)
$(makeLenses ''Dot1Ident)
$(makeLenses ''LLU)
$(makePrisms ''LLU)
$(makeLenses ''ConfigItem)
$(makeLenses ''ConfigBlock)
$(makeLenses ''Verilog2005)
$(makeLenses ''SourceInfo)

data Logic = LAnd | LOr | LNand | LNor
  deriving (Eq, Data)

instance Show Logic where
  show x = case x of LAnd -> "and"; LOr -> "or"; LNand -> "nand"; LNor -> "nor"

data SystemFunction
  = SFDisplay
  | SFDisplayb
  | SFDisplayh
  | SFDisplayo
  | SFStrobe
  | SFStrobeb
  | SFStrobeh
  | SFStrobeo
  | SFWrite
  | SFWriteb
  | SFWriteh
  | SFWriteo
  | SFMonitor
  | SFMonitorb
  | SFMonitorh
  | SFMonitoro
  | SFMonitoroff
  | SFMonitoron
  | SFFclose
  | SFFdisplay
  | SFFdisplayb
  | SFFdisplayh
  | SFFdisplayo
  | SFFstrobe
  | SFFstrobeb
  | SFFstrobeh
  | SFFstrobeo
  | SFSwrite
  | SFSwriteb
  | SFSwriteh
  | SFSwriteo
  | SFFscanf
  | SFFread
  | SFFseek
  | SFFflush
  | SFFeof
  | SFSdfannotate
  | SFFopen
  | SFFwrite
  | SFFwriteb
  | SFFwriteh
  | SFFwriteo
  | SFFmonitor
  | SFFmonitorb
  | SFFmonitorh
  | SFFmonitoro
  | SFSformat
  | SFFgetc
  | SFUngetc
  | SFFgets
  | SFSscanf
  | SFRewind
  | SFFtell
  | SFFerror
  | SFReadmemb
  | SFReadmemh
  | SFPrinttimescale
  | SFTimeformat
  | SFFinish
  | SFStop
  | SFQinitialize
  | SFQremove
  | SFQexam
  | SFQadd
  | SFQfull
  | SFRealtime
  | SFTime
  | SFStime
  | SFBitstoreal
  | SFItor
  | SFSigned
  | SFRealtobits
  | SFRtoi
  | SFUnsigned
  | SFRandom
  | SFDisterlang
  | SFDistnormal
  | SFDistt
  | SFDistchisquare
  | SFDistexponential
  | SFDistpoisson
  | SFDistuniform
  | SFClog2
  | SFLn
  | SFLog10
  | SFExp
  | SFSqrt
  | SFPow
  | SFFloor
  | SFCeil
  | SFSin
  | SFCos
  | SFTan
  | SFAsin
  | SFAcos
  | SFAtan
  | SFAtan2
  | SFHypot
  | SFSinh
  | SFCosh
  | SFTanh
  | SFAsinh
  | SFAcosh
  | SFAtanh
  | SFTestplusargs
  | SFValueplusargs
  | SFPla
      { _SFPSync :: !Bool,
        _SFPLogic :: !Logic,
        _SFPPla_arr :: !Bool
      }
  deriving (Eq, Data)

instance Show SystemFunction where
  show x = case x of
    SFDisplay -> "display"
    SFDisplayb -> "displayb"
    SFDisplayh -> "displayh"
    SFDisplayo -> "displayo"
    SFStrobe -> "strobe"
    SFStrobeb -> "strobeb"
    SFStrobeh -> "strobeh"
    SFStrobeo -> "strobeo"
    SFWrite -> "write"
    SFWriteb -> "writeb"
    SFWriteh -> "writeh"
    SFWriteo -> "writeo"
    SFMonitor -> "monitor"
    SFMonitorb -> "monitorb"
    SFMonitorh -> "monitorh"
    SFMonitoro -> "monitoro"
    SFMonitoroff -> "monitoroff"
    SFMonitoron -> "monitoron"
    SFFclose -> "fclose"
    SFFdisplay -> "fdisplay"
    SFFdisplayb -> "fdisplayb"
    SFFdisplayh -> "fdisplayh"
    SFFdisplayo -> "fdisplayo"
    SFFstrobe -> "fstrobe"
    SFFstrobeb -> "fstrobeb"
    SFFstrobeh -> "fstrobeh"
    SFFstrobeo -> "fstrobeo"
    SFSwrite -> "swrite"
    SFSwriteb -> "swriteb"
    SFSwriteh -> "swriteh"
    SFSwriteo -> "swriteo"
    SFFscanf -> "fscanf"
    SFFread -> "fread"
    SFFseek -> "fseek"
    SFFflush -> "fflush"
    SFFeof -> "feof"
    SFSdfannotate -> "sdf_annotate"
    SFFopen -> "fopen"
    SFFwrite -> "fwrite"
    SFFwriteb -> "fwriteb"
    SFFwriteh -> "fwriteh"
    SFFwriteo -> "fwriteo"
    SFFmonitor -> "fmonitor"
    SFFmonitorb -> "fmonitorb"
    SFFmonitorh -> "fmonitorh"
    SFFmonitoro -> "fmonitoro"
    SFSformat -> "sformat"
    SFFgetc -> "fgetc"
    SFUngetc -> "ungetc"
    SFFgets -> "gets"
    SFSscanf -> "sscanf"
    SFRewind -> "rewind"
    SFFtell -> "ftell"
    SFFerror -> "ferror"
    SFReadmemb -> "readmemb"
    SFReadmemh -> "readmemh"
    SFPrinttimescale -> "printtimescale"
    SFTimeformat -> "timeformat"
    SFFinish -> "finish"
    SFStop -> "stop"
    SFQinitialize -> "q_initialize"
    SFQremove -> "q_remove"
    SFQexam -> "q_exam"
    SFQadd -> "q_add"
    SFQfull -> "q_full"
    SFRealtime -> "realtime"
    SFTime -> "time"
    SFStime -> "stime"
    SFBitstoreal -> "bitstoreal"
    SFItor -> "itor"
    SFSigned -> "signed"
    SFRealtobits -> "realtobits"
    SFRtoi -> "rtoi"
    SFUnsigned -> "unsigned"
    SFRandom -> "random"
    SFDisterlang -> "dist_erlang"
    SFDistnormal -> "dist_normal"
    SFDistt -> "dist_t"
    SFDistchisquare -> "dist_chi_square"
    SFDistexponential -> "dist_exponential"
    SFDistpoisson -> "dist_poisson"
    SFDistuniform -> "dist_uniform"
    SFClog2 -> "clog2"
    SFLn -> "ln"
    SFLog10 -> "log10"
    SFExp -> "exp"
    SFSqrt -> "sqrt"
    SFPow -> "pow"
    SFFloor -> "floor"
    SFCeil -> "ceil"
    SFSin -> "sin"
    SFCos -> "cos"
    SFTan -> "tan"
    SFAsin -> "asin"
    SFAcos -> "acos"
    SFAtan -> "atan"
    SFAtan2 -> "atan2"
    SFHypot -> "hypot"
    SFSinh -> "sinh"
    SFCosh -> "cosh"
    SFTanh -> "tanh"
    SFAsinh -> "asinh"
    SFAcosh -> "acosh"
    SFAtanh -> "atanh"
    SFTestplusargs -> "test$plusargs"
    SFValueplusargs -> "value$plusargs"
    SFPla {_SFPSync = True, _SFPLogic = LAnd, _SFPPla_arr = False} -> "sync$and$array"
    SFPla {_SFPSync = True, _SFPLogic = LAnd, _SFPPla_arr = True} -> "sync$and$plane"
    SFPla {_SFPSync = True, _SFPLogic = LOr, _SFPPla_arr = False} -> "sync$or$array"
    SFPla {_SFPSync = True, _SFPLogic = LOr, _SFPPla_arr = True} -> "sync$or$plane"
    SFPla {_SFPSync = True, _SFPLogic = LNand, _SFPPla_arr = False} -> "sync$nand$array"
    SFPla {_SFPSync = True, _SFPLogic = LNand, _SFPPla_arr = True} -> "sync$nand$plane"
    SFPla {_SFPSync = True, _SFPLogic = LNor, _SFPPla_arr = False} -> "sync$nor$array"
    SFPla {_SFPSync = True, _SFPLogic = LNor, _SFPPla_arr = True} -> "sync$nor$plane"
    SFPla {_SFPSync = False, _SFPLogic = LAnd, _SFPPla_arr = False} -> "async$and$array"
    SFPla {_SFPSync = False, _SFPLogic = LAnd, _SFPPla_arr = True} -> "async$and$plane"
    SFPla {_SFPSync = False, _SFPLogic = LOr, _SFPPla_arr = False} -> "async$or$array"
    SFPla {_SFPSync = False, _SFPLogic = LOr, _SFPPla_arr = True} -> "async$or$plane"
    SFPla {_SFPSync = False, _SFPLogic = LNand, _SFPPla_arr = False} -> "async$nand$array"
    SFPla {_SFPSync = False, _SFPLogic = LNand, _SFPPla_arr = True} -> "async$nand$plane"
    SFPla {_SFPSync = False, _SFPLogic = LNor, _SFPPla_arr = False} -> "async$nor$array"
    SFPla {_SFPSync = False, _SFPLogic = LNor, _SFPPla_arr = True} -> "async$nor$plane"

sfMap :: HashMap.HashMap ByteString SystemFunction
sfMap =
  HashMap.fromList
    [ ("display", SFDisplay),
      ("displayb", SFDisplayb),
      ("displayh", SFDisplayh),
      ("displayo", SFDisplayo),
      ("strobe", SFStrobe),
      ("strobeb", SFStrobeb),
      ("strobeh", SFStrobeh),
      ("strobeo", SFStrobeo),
      ("write", SFWrite),
      ("writeb", SFWriteb),
      ("writeh", SFWriteh),
      ("writeo", SFWriteo),
      ("monitor", SFMonitor),
      ("monitorb", SFMonitorb),
      ("monitorh", SFMonitorh),
      ("monitoro", SFMonitoro),
      ("monitoroff", SFMonitoroff),
      ("monitoron", SFMonitoron),
      ("fclose", SFFclose),
      ("fdisplay", SFFdisplay),
      ("fdisplayb", SFFdisplayb),
      ("fdisplayh", SFFdisplayh),
      ("fdisplayo", SFFdisplayo),
      ("fstrobe", SFFstrobe),
      ("fstrobeb", SFFstrobeb),
      ("fstrobeh", SFFstrobeh),
      ("fstrobeo", SFFstrobeo),
      ("swrite", SFSwrite),
      ("swriteb", SFSwriteb),
      ("swriteh", SFSwriteh),
      ("swriteo", SFSwriteo),
      ("fscanf", SFFscanf),
      ("fread", SFFread),
      ("fseek", SFFseek),
      ("fflush", SFFflush),
      ("feof", SFFeof),
      ("sdf_annotate", SFSdfannotate),
      ("fopen", SFFopen),
      ("fwrite", SFFwrite),
      ("fwriteb", SFFwriteb),
      ("fwriteh", SFFwriteh),
      ("fwriteo", SFFwriteo),
      ("fmonitor", SFFmonitor),
      ("fmonitorb", SFFmonitorb),
      ("fmonitorh", SFFmonitorh),
      ("fmonitoro", SFFmonitoro),
      ("sformat", SFSformat),
      ("fgetc", SFFgetc),
      ("ungetc", SFUngetc),
      ("gets", SFFgets),
      ("sscanf", SFSscanf),
      ("rewind", SFRewind),
      ("ftell", SFFtell),
      ("ferror", SFFerror),
      ("readmemb", SFReadmemb),
      ("readmemh", SFReadmemh),
      ("printtimescale", SFPrinttimescale),
      ("timeformat", SFTimeformat),
      ("finish", SFFinish),
      ("stop", SFStop),
      ("q_initialize", SFQinitialize),
      ("q_remove", SFQremove),
      ("q_exam", SFQexam),
      ("q_add", SFQadd),
      ("q_full", SFQfull),
      ("realtime", SFRealtime),
      ("time", SFTime),
      ("stime", SFStime),
      ("bitstoreal", SFBitstoreal),
      ("itor", SFItor),
      ("signed", SFSigned),
      ("realtobits", SFRealtobits),
      ("rtoi", SFRtoi),
      ("unsigned", SFUnsigned),
      ("random", SFRandom),
      ("dist_erlang", SFDisterlang),
      ("dist_normal", SFDistnormal),
      ("dist_t", SFDistt),
      ("dist_chi_square", SFDistchisquare),
      ("dist_exponential", SFDistexponential),
      ("dist_poisson", SFDistpoisson),
      ("dist_uniform", SFDistuniform),
      ("clog2", SFClog2),
      ("ln", SFLn),
      ("log10", SFLog10),
      ("exp", SFExp),
      ("sqrt", SFSqrt),
      ("pow", SFPow),
      ("floor", SFFloor),
      ("ceil", SFCeil),
      ("sin", SFSin),
      ("cos", SFCos),
      ("tan", SFTan),
      ("asin", SFAsin),
      ("acos", SFAcos),
      ("atan", SFAtan),
      ("atan2", SFAtan2),
      ("hypot", SFHypot),
      ("sinh", SFSinh),
      ("cosh", SFCosh),
      ("tanh", SFTanh),
      ("asinh", SFAsinh),
      ("acosh", SFAcosh),
      ("atanh", SFAtanh),
      ("test$plusargs", SFTestplusargs),
      ("value$plusargs", SFValueplusargs),
      ("sync$and$array", SFPla {_SFPSync = True, _SFPLogic = LAnd, _SFPPla_arr = False}),
      ("sync$and$plane", SFPla {_SFPSync = True, _SFPLogic = LAnd, _SFPPla_arr = True}),
      ("sync$or$array", SFPla {_SFPSync = True, _SFPLogic = LOr, _SFPPla_arr = False}),
      ("sync$or$plane", SFPla {_SFPSync = True, _SFPLogic = LOr, _SFPPla_arr = True}),
      ("sync$nand$array", SFPla {_SFPSync = True, _SFPLogic = LNand, _SFPPla_arr = False}),
      ("sync$nand$plane", SFPla {_SFPSync = True, _SFPLogic = LNand, _SFPPla_arr = True}),
      ("sync$nor$array", SFPla {_SFPSync = True, _SFPLogic = LNor, _SFPPla_arr = False}),
      ("sync$nor$plane", SFPla {_SFPSync = True, _SFPLogic = LNor, _SFPPla_arr = True}),
      ("async$and$array", SFPla {_SFPSync = False, _SFPLogic = LAnd, _SFPPla_arr = False}),
      ("async$and$plane", SFPla {_SFPSync = False, _SFPLogic = LAnd, _SFPPla_arr = True}),
      ("async$or$array", SFPla {_SFPSync = False, _SFPLogic = LOr, _SFPPla_arr = False}),
      ("async$or$plane", SFPla {_SFPSync = False, _SFPLogic = LOr, _SFPPla_arr = True}),
      ("async$nand$array", SFPla {_SFPSync = False, _SFPLogic = LNand, _SFPPla_arr = False}),
      ("async$nand$plane", SFPla {_SFPSync = False, _SFPLogic = LNand, _SFPPla_arr = True}),
      ("async$nor$array", SFPla {_SFPSync = False, _SFPLogic = LNor, _SFPPla_arr = False}),
      ("async$nor$plane", SFPla {_SFPSync = False, _SFPLogic = LNor, _SFPPla_arr = True})
    ]
