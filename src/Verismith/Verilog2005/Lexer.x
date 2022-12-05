-- Module      : Verismith.Verilog2005.Lexer
-- Description : Partial Verilog 2005 lexer to reconstruct the AST.
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : abandonned
-- Portability : POSIX

{
{-# OPTIONS_GHC -w #-}

module Verismith.Verilog2005.Lexer
  ( alexScanTokens
  , parseDecimal
  , isKW
  )
where

import Verismith.Verilog2005.Token
import qualified Data.ByteString as SBS
import Data.ByteString.Internal (c2w)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (mapMaybe)
import Text.Printf (printf)
import Numeric.Natural

}

%encoding "latin1"
%wrapper "monad-bytestring"

$white = [\ \t\n\f]
$all = [\0-\255]

-- | Comments and white space
@oneLineComment = "//" .*
@blockComment   = "/*" [$all # \*]* (\*+ [$all # [\*\/]] [$all # \*]*)* \*+ \/

-- | Operators
@unaryOperator     = [~!] | "~&" | "~|"
@binaryOperator    = [\/\<\>\%] | "==" | "!=" | "===" | "!==" | "&&" | "||" | "**" | ">=" | "<<" | ">>" | "<<<" | ">>>"

-- | Identifiers
$identifierFirstChar = [a-z A-Z _]
$identifierChar      = [a-z A-Z 0-9 _ \$]
@escapedIdentifier   = \\ [$all # $white]*
@simpleIdentifier    = $identifierFirstChar $identifierChar*
@systemIdentifier    = \$ $identifierChar+
@compilerDirective   = ` $identifierChar+

-- | Table symbols, and edges
$tableout  = [01 xX]
$tablein   = [bB \?]
$tableedge = [fF pP rR nN \*]
@edgedesc  = [01] [01 xX zZ] | [xX zZ] [01]

-- | Numeric building blocks
$decimalDigit = 0-9
$specialDigit = [xX zZ \?]
$binaryDigit  = [$specialDigit 0-1]
$octalDigit   = [$specialDigit 0-7]
$hexDigit     = [$specialDigit 0-9 a-f A-F]

@unsigned = $decimalDigit (_ | $decimalDigit)*
@xzValue  = $specialDigit _*
@binValue = $binaryDigit (_ | $binaryDigit)*
@octValue = $octalDigit (_ | $octalDigit)*
@hexValue = $hexDigit (_ | $hexDigit)*

@fixed = @unsigned \. @unsigned
@real  = @fixed | @unsigned (\. @unsigned)? [eE] [\+\-]? @unsigned

-- | Strings

$stringChar = [$all # [\\"]]
@string   = \" $stringChar* (\\ . $stringChar*)* \"

-- | Timescale tokens
@tsvalue = 1(00?)?
@tsunit  = [munpf]?s

tokens :-
  <0,table,edge,pre2,pre8,pre10,pre16,ts0,ts1,ts2,ts3,ts4> {
    $white+ ;
    @blockComment ;
    @oneLineComment ;
  }

  <0> {
    @simpleIdentifier  { toa kwident  }
    @escapedIdentifier { to escSimpleIdent }
    @systemIdentifier  { to (IdSystem . SBS.tail) }
    @compilerDirective { toa (cdcident . SBS.tail) }
    @real   { to LitReal   }
    @string { to LitString }
    \'[sS]?[bB] { toa (numberBase BBin) }
    \'[sS]?[oO] { toa (numberBase BOct) }
    \'[sS]?[dD] { toa (numberBase BDec) }
    \'[sS]?[hH] { toa (numberBase BHex) }
    & { tok AmAmp }
    ! { tok UnBang }
    = { tok SymEq }
    @ { tok SymAt }
    \| { tok AmBar     }
    \^ { tok AmHat     }
    \~ { tok UnTilde   }
    \/ { tok BinSlash  }
    \% { tok BinPercent }
    \< { tok BinLt     }
    \> { tok BinGt     }
    \{ { tok SymBraceL }
    \} { tok SymBraceR }
    \+ { tok SymPlus   }
    \? { tok SymQuestion }
    \# { tok SymPound  }
    \* { tok SymAster  }
    \. { tok SymDot    }
    \$ { tok SymDollar }
    "^~" { tok AmTildeHat } -- Sorry
    "~^" { tok AmTildeHat }
    "~&" { tok UnTildeAmp }
    "~|" { tok UnTildeBar }
    "==" { tok BinEqEq    }
    "!=" { tok BinBangEq  }
    "&&" { tok BinAmpAmp  }
    "||" { tok BinBarBar  }
    "**" { tok BinAsterAster }
    ">=" { tok BinGtEq    }
    "<<" { tok BinLtLt    }
    ">>" { tok BinGtGt    }
    "(*" { tok SymParenAster }
    "*)" { tok SymAsterParen }
    "<=" { tok SymLtEq    }
    "+:" { tok SymPlusColon }
    "-:" { tok SymDashColon }
    "->" { tok SymDashGt  }
    "=>" { tok SymEqGt    }
    "*>" { tok SymAsterGt }
    "===" { tok BinEqEqEq }
    "!==" { tok BinBangEqEq }
    "<<<" { tok BinLtLtLt }
    ">>>" { tok BinGtGtGt }
    "&&&" { tok SymAmpAmpAmp }
  }

  <ts0> @tsvalue { andBegin (to tsValue) ts1 }
  <ts1> @tsunit  { andBegin (to tsUnit)  ts2 }
  <ts2> \/       { begin                 ts3 }
  <ts3> @tsvalue { andBegin (to tsValue) ts4 }
  <ts4> @tsunit  { andBegin (to tsUnit)  0   }

  <pre2>    @binValue { andBegin (to binary)  0 }
  <pre8>    @octValue { andBegin (to octal)   0 }
  <pre16>   @hexValue { andBegin (to hex)     0 }
  <pre10>   @xzValue  { andBegin (to xz)      0 }
  <0,pre10> @unsigned { andBegin (to decimal) 0 }


  <0,table> {
    \( { tok SymParenL }
    \) { tok SymParenR }
    \- { tok SymDash   }
    :  { tok SymColon  }
    \; { tok SymSemi   }
  }
  <0,edge> {
    \, { tok SymComma  }
    \[ { tok SymBrackL }
    \] { andBegin (tok SymBrackR) 0 }
  }

  -- Reusing token constructors
  <edge>  @edgedesc  { to edgeDesc }
  <table> {
    $tableout  { to tableOut }
    $tablein   { to tableIn  }
    $tableedge { to tableEdge }
    "endtable" { andBegin (tok KWEndtable) 0 }
  }

{

alexEOF :: Alex PosToken
alexEOF = return PosToken { _PTPos = Nowhere, _PTToken = TokEof }

gets :: AlexInput -> Int64 -> SBS.ByteString
gets (_, _, s, _) l = ByteString.toStrict $ ByteString.take l s

tok :: Token -> AlexAction PosToken
tok t (AlexPn _ l c, _, _, _) _ = return PosToken { _PTPos = Somewhere l c, _PTToken = t }

to :: (SBS.ByteString -> Token) -> AlexAction PosToken
to f ai s = tok (f (gets ai s)) ai s

toa :: (SBS.ByteString -> Alex Token) -> AlexAction PosToken
toa f ai s = do
  token <- f $ gets ai s
  tok token ai s

tsValue :: SBS.ByteString -> Token
tsValue s = CDTSInt (SBS.length s - 1)
tsUnit :: SBS.ByteString -> Token
tsUnit s = CDTSUnit
  $ case s of { "s" -> 0 ; "ms" -> -3 ; "us" -> -6 ; "ns" -> -9 ; "ps" -> -12 ; "fs" -> -15 }

numberBase :: Base -> SBS.ByteString -> Alex Token
numberBase ba bs = do
  alexSetStartCode (case ba of { BBin -> pre2 ; BOct -> pre8 ; BDec -> pre10 ; BHex -> pre16 })
  return $ NumberBase (SBS.length bs == 3) ba

binary :: SBS.ByteString -> Token
binary s = LitBinary $ mapMaybe (\c -> case () of
  () | c == c2w '0' -> Just BXZ0
  () | c == c2w '1' -> Just BXZ1
  () | c == c2w 'x' || c == c2w 'X' -> Just BXZX
  () | c == c2w 'z' || c == c2w 'Z' || c == c2w '?' -> Just BXZZ
  () -> Nothing) $ SBS.unpack s
octal :: SBS.ByteString -> Token
octal s = LitOctal $ mapMaybe (\c -> case () of
  () | c2w '0' <= c && c <= c2w '7' -> Just $ toEnum $ fromIntegral $ c - c2w '0'
  () | c == c2w 'x' || c == c2w 'X' -> Just OXZX
  () | c == c2w 'z' || c == c2w 'Z' || c == c2w '?' -> Just OXZZ
  () -> Nothing) $ SBS.unpack s
hex :: SBS.ByteString -> Token
hex s = LitHex $ mapMaybe (\c -> case () of
  () | c2w '0' <= c && c <= c2w '9' -> Just $ toEnum $ fromIntegral $ c - c2w '0'
  () | c2w 'a' <= c && c <= c2w 'f' -> Just $ toEnum $ fromIntegral $ c + 10 - c2w 'a'
  () | c2w 'A' <= c && c <= c2w 'F' -> Just $ toEnum $ fromIntegral $ c + 10 - c2w 'A'
  () | c == c2w 'x' || c == c2w 'X' -> Just HXZX
  () | c == c2w 'z' || c == c2w 'Z' || c == c2w '?' -> Just HXZZ
  () -> Nothing) $ SBS.unpack s
xz :: SBS.ByteString -> Token
xz s = LitXZ $ let c = SBS.head s in c == c2w 'x' || c == c2w 'X'
parseDecimal :: SBS.ByteString -> Natural
parseDecimal = fromInteger .  SBS.foldl
  (\acc d -> if c2w '0' <= d && d <= c2w '9' then 10*acc + toInteger (d - c2w '0') else acc) 0
decimal :: SBS.ByteString -> Token
decimal = LitDecimal . parseDecimal

unbxz :: SBS.ByteString -> BXZ
unbxz s = case s of
  { "0" -> BXZ0 ; "1" -> BXZ1 ; "x" -> BXZX ; "X" -> BXZX ; "z" -> BXZZ ; "Z" -> BXZZ }
edgeDesc :: SBS.ByteString -> Token
edgeDesc s = EdgeEdge (unbxz $ SBS.init s) (unbxz $ SBS.tail s)
tableOut :: SBS.ByteString -> Token
tableOut s = TableOut $ case s of { "0" -> ZOXZ ; "1" -> ZOXO ; "x" -> ZOXX ; "X" -> ZOXX }
tableIn :: SBS.ByteString -> Token
tableIn s = TableIn $ case s of { "b" -> True ; "B" -> True ; "?" -> False }
tableEdge :: SBS.ByteString -> Token
tableEdge s = TableEdge $ case s of
  { "*" -> AFRNPA ; "f" -> AFRNPF ; "F" -> AFRNPF ; "r" -> AFRNPR ; "R" -> AFRNPR
  ; "n" -> AFRNPN ; "N" -> AFRNPN ; "p" -> AFRNPP ; "P" -> AFRNPP }

cdcident :: SBS.ByteString -> Alex Token
cdcident s = HashMap.findWithDefault
  (alexError $ printf "Compiler directive %s not supported, preprocess input file" $ show s) s cdMap
  -- the default case should look at defined identifiers and add to the input
  -- then send the result of running alexMonadScan
  -- or fail as is done currently if the identifier is not defined

kwident :: SBS.ByteString -> Alex Token
kwident s = case SBS.stripPrefix "PATHPULSE$" s of
  Just ss -> return $ TknPP ss
  Nothing -> HashMap.findWithDefault (return $ IdSimple s) s kwMap

escSimpleIdent :: SBS.ByteString -> Token
escSimpleIdent s = case SBS.uncons ss of
  Just (c, t) | testfirst c && not (isKW ss)
    && SBS.all (\c -> testfirst c || (c2w '0' <= c && c <= c2w '9') || c == c2w '$') t
    -> IdSimple ss
  _ -> IdEscaped s
  where
    testfirst c = (c2w 'A' <= c && c <= c2w 'Z') || (c2w 'a' <= c && c <= c2w 'z') || c == c2w '_'
    ss = SBS.tail s

cdMap :: HashMap.HashMap SBS.ByteString (Alex Token)
cdMap = HashMap.fromList
  $ ("timescale", alexSetStartCode ts0 >> return CDTimescale)
  -- `include would go here and change state or parse the next token immediately
  -- `define would also go here and change state to store input as is in a map
  : map (\(x, y) -> (x, return y))
  [ ("celldefine", CDCelldefine)
  , ("default_nettype", CDDefaultnettype)
  -- , ("default_decay_time", CDUnknown)
  -- , ("default_trireg_strength", CDUnknown)
  -- , ("delay_mode_distributed", CDUnknown)
  -- , ("delay_mode_path", CDUnknown)
  -- , ("delay_mode_unit", CDUnknown)
  -- , ("delay_mode_zero", CDUnknown)
  , ("endcelldefine", CDEndcelldefine)
  , ("line", CDLine)
  , ("nounconnected_drive", CDNounconnecteddrive)
  -- , ("pragma", CDPragma) -- Another layer of hell
  , ("resetall", CDResetall)
  , ("unconnected_drive", CDUnconnecteddrive)
  ]

isKW :: SBS.ByteString -> Bool
isKW s = HashMap.member s kwMap
kwMap :: HashMap.HashMap SBS.ByteString (Alex Token)
kwMap = HashMap.fromList
  $ ("edge", alexSetStartCode edge >> return KWEdge)
  : ("endtable", alexSetStartCode 0 >> return KWEndtable)
  : ("table", alexSetStartCode table >> return KWTable)
  : map (\(x, y) -> (x, return y))
  [ ("always", KWAlways)
  , ("and", KWAnd)
  , ("assign", KWAssign)
  , ("automatic", KWAutomatic)
  , ("begin", KWBegin)
  , ("buf", KWBuf)
  , ("bufif0", KWBufif0)
  , ("bufif1", KWBufif1)
  , ("case", KWCase)
  , ("casex", KWCasex)
  , ("casez", KWCasez)
  , ("cell", KWCell)
  , ("cmos", KWCmos)
  , ("config", KWConfig)
  , ("deassign", KWDeassign)
  , ("default", KWDefault)
  , ("defparam", KWDefparam)
  , ("design", KWDesign)
  , ("disable", KWDisable)
  , ("else", KWElse)
  , ("end", KWEnd)
  , ("endcase", KWEndcase)
  , ("endconfig", KWEndconfig)
  , ("endfunction", KWEndfunction)
  , ("endgenerate", KWEndgenerate)
  , ("endmodule", KWEndmodule)
  , ("endprimitive", KWEndprimitive)
  , ("endspecify", KWEndspecify)
  , ("endtask", KWEndtask)
  , ("event", KWEvent)
  , ("for", KWFor)
  , ("force", KWForce)
  , ("forever", KWForever)
  , ("fork", KWFork)
  , ("function", KWFunction)
  , ("generate", KWGenerate)
  , ("genvar", KWGenvar)
  , ("highz0", KWHighz0)
  , ("highz1", KWHighz1)
  , ("if", KWIf)
  , ("ifnone", KWIfnone)
  , ("incdir", KWIncdir)
  , ("include", KWInclude)
  , ("initial", KWInitial)
  , ("inout", KWInout)
  , ("input", KWInput)
  , ("instance", KWInstance)
  , ("integer", KWInteger)
  , ("join", KWJoin)
  , ("large", KWLarge)
  , ("liblist", KWLiblist)
  , ("library", KWLibrary)
  , ("localparam", KWLocalparam)
  , ("macromodule", KWMacromodule)
  , ("medium", KWMedium)
  , ("module", KWModule)
  , ("nand", KWNand)
  , ("negedge", KWNegedge)
  , ("nmos", KWNmos)
  , ("nor", KWNor)
  , ("noshowcancelled", KWNoshowcancelled)
  , ("not", KWNot)
  , ("notif0", KWNotif0)
  , ("notif1", KWNotif1)
  , ("or", KWOr)
  , ("output", KWOutput)
  , ("parameter", KWParameter)
  , ("pmos", KWPmos)
  , ("posedge", KWPosedge)
  , ("primitive", KWPrimitive)
  , ("pull0", KWPull0)
  , ("pull1", KWPull1)
  , ("pulldown", KWPulldown)
  , ("pullup", KWPullup)
  , ("pulsestyle_onevent", KWPulsestyleonevent)
  , ("pulsestyle_ondetect", KWPulsestyleondetect)
  , ("rcmos", KWRcmos)
  , ("real", KWReal)
  , ("realtime", KWRealtime)
  , ("reg", KWReg)
  , ("release", KWRelease)
  , ("repeat", KWRepeat)
  , ("rnmos", KWRnmos)
  , ("rpmos", KWRpmos)
  , ("rtran", KWRtran)
  , ("rtranif0", KWRtranif0)
  , ("rtranif1", KWRtranif1)
  , ("scalared", KWScalared)
  , ("showcancelled", KWShowcancelled)
  , ("signed", KWSigned)
  , ("small", KWSmall)
  , ("specify", KWSpecify)
  , ("specparam", KWSpecparam)
  , ("strong0", KWStrong0)
  , ("strong1", KWStrong1)
  , ("supply0", KWSupply0)
  , ("supply1", KWSupply1)
  , ("task", KWTask)
  , ("time", KWTime)
  , ("tran", KWTran)
  , ("tranif0", KWTranif0)
  , ("tranif1", KWTranif1)
  , ("tri", KWTri)
  , ("tri0", KWTri0)
  , ("tri1", KWTri1)
  , ("triand", KWTriand)
  , ("trior", KWTrior)
  , ("trireg", KWTrireg)
  , ("unsigned", KWUnsigned)
  , ("use", KWUse)
  , ("uwire", KWUwire)
  , ("vectored", KWVectored)
  , ("wait", KWWait)
  , ("wand", KWWand)
  , ("weak0", KWWeak0)
  , ("weak1", KWWeak1)
  , ("while", KWWhile)
  , ("wire", KWWire)
  , ("wor", KWWor)
  , ("xnor", KWXnor)
  , ("xor", KWXor)
  ]

alexScanTokens :: ByteString.ByteString -> Either String [PosToken]
alexScanTokens bs = runAlex bs $ loop []
  where
    loop tks = do
      tk <- alexMonadScan
      if _PTToken tk == TokEof then return $ reverse tks else loop $ tk : tks

}
