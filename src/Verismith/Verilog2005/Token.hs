-- Module      : Verismith.Verilog2005.Token
-- Description : Tokens for Verilog 2005 lexing and parsing.
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX
{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DeriveGeneric #-}

module Verismith.Verilog2005.Token
  ( PosToken (..),
    Position (..),
    PSource (..),
    helperShowPositions,
    showWithPosition,
    Token (..),
    AFRNP (..),
    BXZ (..),
    OXZ (..),
    HXZ (..),
    ZOX (..),
    Base (..),
  )
where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics (Generic)
import Numeric.Natural
import Text.Printf (printf)
import Verismith.Utils

data PosToken = PosToken
  { _ptPos :: !(NonEmpty Position),
    _ptToken :: !Token
  }
  deriving (Eq)

instance Show PosToken where
  show (PosToken _ t) = show t

data Position = Position
  { _posLine :: !Word,
    _posColumn :: !Word,
    _posSource :: !PSource
  }
  deriving (Eq)

data PSource
  = PSFile !String
  | PSDefine !LBS.ByteString
  | PSLine
    { _pslFile :: !String,
      _pslEntering :: !Bool
    }
  deriving (Eq)

instance Show PSource where
  show x = case x of
    PSFile f -> "file " ++ f
    PSDefine t -> "macro replacement \"" ++ map w2c (LBS.unpack t) ++ "\""
    PSLine f _ -> "file " ++ f

instance Show Position where
  show (Position l c s) = printf "line %d, column %d of " l c ++ show s

helperShowPositions :: NonEmpty Position -> String
helperShowPositions =
  foldrMap1
    show
    $ \a@(Position _ _ s) b -> show a
        ++ case s of {PSFile _ -> " included"; PSDefine _ -> ""; PSLine _ _ -> " set"}
        ++ " from "
        ++ b

showWithPosition :: PosToken -> String
showWithPosition (PosToken p t) = show t ++ " at " ++ helperShowPositions p

data AFRNP = AFRNPA | AFRNPF | AFRNPR | AFRNPN | AFRNPP
  deriving (Eq, Ord, Data, Generic, NFData)

data BXZ = BXZ0 | BXZ1 | BXZX | BXZZ
  deriving (Eq, Ord, Bounded, Enum, Data, Generic, NFData)

data OXZ = OXZ0 | OXZ1 | OXZ2 | OXZ3 | OXZ4 | OXZ5 | OXZ6 | OXZ7 | OXZX | OXZZ
  deriving (Eq, Ord, Bounded, Enum, Data, Generic, NFData)

data HXZ
  = HXZ0
  | HXZ1
  | HXZ2
  | HXZ3
  | HXZ4
  | HXZ5
  | HXZ6
  | HXZ7
  | HXZ8
  | HXZ9
  | HXZA
  | HXZB
  | HXZC
  | HXZD
  | HXZE
  | HXZF
  | HXZX
  | HXZZ
  deriving (Eq, Ord, Bounded, Enum, Data, Generic, NFData)

data ZOX = ZOXZ | ZOXO | ZOXX
  deriving (Eq, Ord, Bounded, Enum, Data, Generic, NFData)

data Base = BBin | BOct | BDec | BHex
  deriving (Eq, Ord, Data, Generic, NFData)

instance Show AFRNP where
  show x =
    case x of AFRNPA -> "*"; AFRNPF -> "f"; AFRNPR -> "r"; AFRNPN -> "n"; AFRNPP -> "p"

instance Show ZOX where
  show x = case x of ZOXZ -> "0"; ZOXO -> "1"; ZOXX -> "x"

instance Show BXZ where
  show x = case x of BXZ0 -> "0"; BXZ1 -> "1"; BXZX -> "x"; BXZZ -> "z"

instance Show OXZ where
  show x = case x of
    OXZ0 -> "0"
    OXZ1 -> "1"
    OXZ2 -> "2"
    OXZ3 -> "3"
    OXZ4 -> "4"
    OXZ5 -> "5"
    OXZ6 -> "6"
    OXZ7 -> "7"
    OXZX -> "x"
    OXZZ -> "z"

instance Show HXZ where
  show x = case x of
    HXZ0 -> "0"
    HXZ1 -> "1"
    HXZ2 -> "2"
    HXZ3 -> "3"
    HXZ4 -> "4"
    HXZ5 -> "5"
    HXZ6 -> "6"
    HXZ7 -> "7"
    HXZ8 -> "8"
    HXZ9 -> "9"
    HXZA -> "A"
    HXZB -> "B"
    HXZC -> "C"
    HXZD -> "D"
    HXZE -> "E"
    HXZF -> "F"
    HXZX -> "x"
    HXZZ -> "z"

instance Show Base where
  show x = case x of BBin -> "b"; BOct -> "o"; BDec -> "d"; BHex -> "h"

data Token
  = IdSimple !B.ByteString
  | IdEscaped !B.ByteString
  | IdSystem !B.ByteString
  | LitReal !B.ByteString
  | LitString !B.ByteString
  | NumberBase !Bool !Base
  | LitXZ !Bool
  | LitBinary ![BXZ]
  | LitDecimal !Natural
  | LitOctal ![OXZ]
  | LitHex ![HXZ]
  | TableOut !ZOX
  | TableIn !Bool
  | TableEdge !AFRNP
  | EdgeEdge !BXZ !BXZ
  | TknPP !B.ByteString
  | AmBar
  | AmHat
  | AmAmp
  | AmTildeHat
  | UnTilde
  | UnBang
  | UnTildeAmp
  | UnTildeBar
  | BinSlash
  | BinPercent
  | BinLt
  | BinGt
  | BinEqEq
  | BinBangEq
  | BinEqEqEq
  | BinBangEqEq
  | BinAmpAmp
  | BinBarBar
  | BinAsterAster
  | BinGtEq
  | BinLtLt
  | BinGtGt
  | BinLtLtLt
  | BinGtGtGt
  | SymParenL
  | SymParenR
  | SymBrackL
  | SymBrackR
  | SymBraceL
  | SymBraceR
  | SymAt
  | SymPound
  | SymDollar
  | SymAster
  | SymDot
  | SymComma
  | SymColon
  | SymSemi
  | SymEq
  | SymDash
  | SymPlus
  | SymParenAster
  | SymAsterParen
  | SymQuestion
  | SymLtEq
  | SymPlusColon
  | SymDashColon
  | SymDashGt
  | SymEqGt
  | SymAsterGt
  | SymAmpAmpAmp
  | CDCelldefine
  | CDDefaultnettype
  | CDEndcelldefine
  | CDInclude
  | CDNounconnecteddrive
  | CDResetall
  | CDTimescale
  | CDTSInt !Int
  | CDTSUnit !Int
  | CDUnconnecteddrive
  | CDBeginKeywords
  | CDEndKeywords
-- | CDPragma
-- | CDEndPragma
  | KWAlways
  | KWAnd
  | KWAssign
  | KWAutomatic
  | KWBegin
  | KWBuf
  | KWBufif0
  | KWBufif1
  | KWCase
  | KWCasex
  | KWCasez
  | KWCell
  | KWCmos
  | KWConfig
  | KWDeassign
  | KWDefault
  | KWDefparam
  | KWDesign
  | KWDisable
  | KWEdge
  | KWElse
  | KWEnd
  | KWEndcase
  | KWEndconfig
  | KWEndfunction
  | KWEndgenerate
  | KWEndmodule
  | KWEndprimitive
  | KWEndspecify
  | KWEndtable
  | KWEndtask
  | KWEvent
  | KWFor
  | KWForce
  | KWForever
  | KWFork
  | KWFunction
  | KWGenerate
  | KWGenvar
  | KWHighz0
  | KWHighz1
  | KWIf
  | KWIfnone
  | KWIncdir
  | KWInclude
  | KWInitial
  | KWInout
  | KWInput
  | KWInstance
  | KWInteger
  | KWJoin
  | KWLarge
  | KWLiblist
  | KWLibrary
  | KWLocalparam
  | KWMacromodule
  | KWMedium
  | KWModule
  | KWNand
  | KWNegedge
  | KWNmos
  | KWNor
  | KWNoshowcancelled
  | KWNot
  | KWNotif0
  | KWNotif1
  | KWOr
  | KWOutput
  | KWParameter
  | KWPmos
  | KWPosedge
  | KWPrimitive
  | KWPull0
  | KWPull1
  | KWPulldown
  | KWPullup
  | KWPulsestyleonevent
  | KWPulsestyleondetect
  | KWRcmos
  | KWReal
  | KWRealtime
  | KWReg
  | KWRelease
  | KWRepeat
  | KWRnmos
  | KWRpmos
  | KWRtran
  | KWRtranif0
  | KWRtranif1
  | KWScalared
  | KWShowcancelled
  | KWSigned
  | KWSmall
  | KWSpecify
  | KWSpecparam
  | KWStrong0
  | KWStrong1
  | KWSupply0
  | KWSupply1
  | KWTable
  | KWTask
  | KWTime
  | KWTran
  | KWTranif0
  | KWTranif1
  | KWTri
  | KWTri0
  | KWTri1
  | KWTriand
  | KWTrior
  | KWTrireg
  | KWUnsigned
  | KWUse
  | KWUwire
  | KWVectored
  | KWWait
  | KWWand
  | KWWeak0
  | KWWeak1
  | KWWhile
  | KWWire
  | KWWor
  | KWXnor
  | KWXor
  | TokSVKeyword !B.ByteString
  deriving (Eq, Data)

instance Show Token where
  show x = case x of
    IdSimple s -> unpackChars s
    IdEscaped s -> unpackChars s
    IdSystem s -> '$' : unpackChars s
    LitReal s -> unpackChars s
    LitString s -> '"' : unpackChars s ++ "\""
    NumberBase s b -> '\'' : if s then 's' : show b else show b
    LitXZ b -> if b then "x" else "z"
    LitBinary l -> concatMap show l
    LitDecimal i -> show i
    LitOctal l -> concatMap show l
    LitHex l -> concatMap show l
    TableOut x -> show x
    TableIn b -> if b then "b" else "?"
    TableEdge x -> show x
    EdgeEdge e0 e1 -> show e0 ++ show e1
    TknPP s -> "PATHPULSE$" ++ unpackChars s
    AmBar -> "|"
    AmHat -> "^"
    AmAmp -> "&"
    AmTildeHat -> "~^ or ^~"
    UnTilde -> "~"
    UnBang -> "!"
    UnTildeAmp -> "~&"
    UnTildeBar -> "~|"
    BinSlash -> "/"
    BinPercent -> "%"
    BinLt -> "<"
    BinGt -> ">"
    BinEqEq -> "=="
    BinBangEq -> "!="
    BinEqEqEq -> "==="
    BinBangEqEq -> "!=="
    BinAmpAmp -> "&&"
    BinBarBar -> "||"
    BinAsterAster -> "**"
    BinGtEq -> ">="
    BinLtLt -> "<<"
    BinGtGt -> ">>"
    BinLtLtLt -> "<<<"
    BinGtGtGt -> ">>>"
    SymParenL -> "'('"
    SymParenR -> "')'"
    SymBrackL -> "'['"
    SymBrackR -> "']'"
    SymBraceL -> "'{'"
    SymBraceR -> "'}'"
    SymAt -> "@"
    SymPound -> "#"
    SymDollar -> "$"
    SymAster -> "*"
    SymDot -> "'.'"
    SymComma -> "','"
    SymColon -> "':'"
    SymSemi -> "';'"
    SymEq -> "="
    SymDash -> "-"
    SymPlus -> "+"
    SymParenAster -> "'(*'"
    SymAsterParen -> "'*)'"
    SymQuestion -> "?"
    SymLtEq -> "<="
    SymPlusColon -> "'+:'"
    SymDashColon -> "'-:'"
    SymDashGt -> "->"
    SymEqGt -> "=>"
    SymAsterGt -> "*>"
    SymAmpAmpAmp -> "&&&"
    CDCelldefine -> "`celldefine"
    CDDefaultnettype -> "`default_nettype"
    CDEndcelldefine -> "`endcelldefine"
    CDInclude -> "`include"
    CDNounconnecteddrive -> "`nounconnected_drive"
    CDResetall -> "`resetall"
    CDTimescale -> "`timescale"
    CDTSInt i -> '1' : case i of 0 -> ""; 1 -> "0"; 2 -> "00"
    CDTSUnit i ->
      case i of { 0 -> ""; -1 -> "m"; -2 -> "u"; -3 -> "n"; -4 -> "p"; -5 -> "f" } ++ "s"
    CDUnconnecteddrive -> "`unconnected_drive"
    CDBeginKeywords -> "`begin_keywords"
    CDEndKeywords -> "`end_keywords"
-- CDPragma
-- CDEndPragma
    KWAlways -> "always"
    KWAnd -> "and"
    KWAssign -> "assign"
    KWAutomatic -> "automatic"
    KWBegin -> "begin"
    KWBuf -> "buf"
    KWBufif0 -> "bufif0"
    KWBufif1 -> "bufif1"
    KWCase -> "case"
    KWCasex -> "casex"
    KWCasez -> "casez"
    KWCell -> "cell"
    KWCmos -> "cmos"
    KWConfig -> "config"
    KWDeassign -> "deassign"
    KWDefault -> "default"
    KWDefparam -> "defparam"
    KWDesign -> "design"
    KWDisable -> "disable"
    KWEdge -> "edge"
    KWElse -> "else"
    KWEnd -> "end"
    KWEndcase -> "endcase"
    KWEndconfig -> "endconfig"
    KWEndfunction -> "endfunction"
    KWEndgenerate -> "endgenerate"
    KWEndmodule -> "endmodule"
    KWEndprimitive -> "endprimitive"
    KWEndspecify -> "endspecify"
    KWEndtable -> "endtable"
    KWEndtask -> "endtask"
    KWEvent -> "event"
    KWFor -> "for"
    KWForce -> "force"
    KWForever -> "forever"
    KWFork -> "fork"
    KWFunction -> "function"
    KWGenerate -> "generate"
    KWGenvar -> "genvar"
    KWHighz0 -> "highz0"
    KWHighz1 -> "highz1"
    KWIf -> "if"
    KWIfnone -> "ifnone"
    KWIncdir -> "incdir"
    KWInclude -> "include"
    KWInitial -> "initial"
    KWInout -> "inout"
    KWInput -> "input"
    KWInstance -> "instance"
    KWInteger -> "integer"
    KWJoin -> "join"
    KWLarge -> "large"
    KWLiblist -> "liblist"
    KWLibrary -> "library"
    KWLocalparam -> "localparam"
    KWMacromodule -> "macromodule"
    KWMedium -> "medium"
    KWModule -> "module"
    KWNand -> "nand"
    KWNegedge -> "negedge"
    KWNmos -> "nmos"
    KWNor -> "now"
    KWNoshowcancelled -> "noshowcancelled"
    KWNot -> "not"
    KWNotif0 -> "notif0"
    KWNotif1 -> "notif1"
    KWOr -> "or"
    KWOutput -> "output"
    KWParameter -> "parameter"
    KWPmos -> "pmos"
    KWPosedge -> "posedge"
    KWPrimitive -> "primitive"
    KWPull0 -> "pull0"
    KWPull1 -> "pull1"
    KWPulldown -> "pulldown"
    KWPullup -> "pullup"
    KWPulsestyleonevent -> "pulsestyle_onevent"
    KWPulsestyleondetect -> "plusestyle_ondetect"
    KWRcmos -> "rcmos"
    KWReal -> "real"
    KWRealtime -> "realtime"
    KWReg -> "reg"
    KWRelease -> "release"
    KWRepeat -> "repeat"
    KWRnmos -> "rnmos"
    KWRpmos -> "rpmos"
    KWRtran -> "rtan"
    KWRtranif0 -> "rtranif0"
    KWRtranif1 -> "rtranif1"
    KWScalared -> "scalared"
    KWShowcancelled -> "showcancelled"
    KWSigned -> "signed"
    KWSmall -> "small"
    KWSpecify -> "specify"
    KWSpecparam -> "specparam"
    KWStrong0 -> "strong0"
    KWStrong1 -> "strong1"
    KWSupply0 -> "supply0"
    KWSupply1 -> "supply1"
    KWTable -> "table"
    KWTask -> "task"
    KWTime -> "time"
    KWTran -> "tran"
    KWTranif0 -> "tranif0"
    KWTranif1 -> "tranif1"
    KWTri -> "tri"
    KWTri0 -> "tri0"
    KWTri1 -> "tri1"
    KWTriand -> "triand"
    KWTrior -> "trior"
    KWTrireg -> "trireg"
    KWUnsigned -> "unsigned"
    KWUse -> "use"
    KWUwire -> "uwire"
    KWVectored -> "vectored"
    KWWait -> "wait"
    KWWand -> "wand"
    KWWeak0 -> "weak0"
    KWWeak1 -> "weak1"
    KWWhile -> "while"
    KWWire -> "wire"
    KWWor -> "wor"
    KWXnor -> "xnor"
    KWXor -> "xor"
    TokSVKeyword kw -> unpackChars kw
