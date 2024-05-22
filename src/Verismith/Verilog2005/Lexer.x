-- Module      : Verismith.Verilog2005.Lexer
-- Description : Partial Verilog 2005 lexer to reconstruct the AST.
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX

{
{-# OPTIONS_GHC -w #-}

module Verismith.Verilog2005.Lexer
  ( scanTokens
  , parseDecimal
  , isKW
  , makeString
  )
where

import Data.Bits
import Data.Word (Word8)
import Numeric.Natural
import GHC.Natural
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import Text.Printf (printf)
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Exception
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Internal (c2w, w2c, unpackChars, packChars)
import qualified Data.HashMap.Strict as HashMap
import Verismith.Verilog2005.Token
}

%encoding "latin1"

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
    @compilerDirective { cdcident }
  }

  <0> {
    @real   { to LitReal   }
    @string { to LitString }
    @simpleIdentifier  { toa kwident }
    @escapedIdentifier { to escSimpleIdent }
    @systemIdentifier  { to (IdSystem . SBS.tail) }
    \'[sS]?[bB] { toa (numberBase BBin) }
    \'[sS]?[oO] { toa (numberBase BOct) }
    \'[sS]?[dD] { toa (numberBase BDec) }
    \'[sS]?[hH] { toa (numberBase BHex) }
    & { tok AmAmp  }
    ! { tok UnBang }
    = { tok SymEq  }
    @ { tok SymAt  }
    \| { tok AmBar       }
    \^ { tok AmHat       }
    \~ { tok UnTilde     }
    \/ { tok BinSlash    }
    \% { tok BinPercent  }
    \< { tok BinLt       }
    \> { tok BinGt       }
    \{ { tok SymBraceL   }
    \} { tok SymBraceR   }
    \+ { tok SymPlus     }
    \? { tok SymQuestion }
    \# { tok SymPound    }
    \* { tok SymAster    }
    \. { tok SymDot      }
    \$ { tok SymDollar   }
    "^~" { tok AmTildeHat    } -- Sorry
    "~^" { tok AmTildeHat    }
    "~&" { tok UnTildeAmp    }
    "~|" { tok UnTildeBar    }
    "==" { tok BinEqEq       }
    "!=" { tok BinBangEq     }
    "&&" { tok BinAmpAmp     }
    "||" { tok BinBarBar     }
    "**" { tok BinAsterAster }
    ">=" { tok BinGtEq       }
    "<<" { tok BinLtLt       }
    ">>" { tok BinGtGt       }
    "(*" { tok SymParenAster }
    "*)" { tok SymAsterParen }
    "<=" { tok SymLtEq       }
    "+:" { tok SymPlusColon  }
    "-:" { tok SymDashColon  }
    "->" { tok SymDashGt     }
    "=>" { tok SymEqGt       }
    "*>" { tok SymAsterGt    }
    "===" { tok BinEqEqEq    }
    "!==" { tok BinBangEqEq  }
    "<<<" { tok BinLtLtLt    }
    ">>>" { tok BinGtGtGt    }
    "&&&" { tok SymAmpAmpAmp }
  }

  <ts0> @tsvalue { startcode ts1 (to tsValue) }
  <ts1> @tsunit  { startcode ts2 (to tsUnit)  }
  <ts2> \/       { \_ _ -> sc ts3 >> scan     }
  <ts3> @tsvalue { startcode ts4 (to tsValue) }
  <ts4> @tsunit  { startcode 0   (to tsUnit)  }

  <pre2>    @binValue { startcode 0 (to binary)  }
  <pre8>    @octValue { startcode 0 (to octal)   }
  <pre16>   @hexValue { startcode 0 (to hex)     }
  <pre10>   @xzValue  { startcode 0 (to xz)      }
  <0,pre10> @unsigned { startcode 0 (to decimal) }


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
    \] { startcode 0 (tok SymBrackR) }
  }

  -- Reusing token constructors
  <edge>  @edgedesc  { to edgeDesc }
  <table> {
    $tableout  { to tableOut }
    $tablein   { to tableIn  }
    $tableedge { to tableEdge }
    "endtable" { startcode 0 (tok KWEndtable) }
  }

{
-- not using any wrapper so I have to define things myself

data AlexInput = AlexInput
  { _aiPosition  :: !Position,
    _aiPrevChar  :: !Char,
    _aiInput     :: !LBS.ByteString
  }

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput (Position ln cl s) _ inp) = case LBS.uncons inp of
  Nothing -> Nothing
  Just (b, inp) -> Just (b, let c = w2c b in AlexInput
      ( case c of
          '\t' -> Position ln ((cl .|. 3) + 1) s
          '\n' -> Position (ln + 1) 1 s
          _ -> Position ln (cl + 1) s
      )
      c
      inp
    )

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = _aiPrevChar

-- above was mandatory declarations for alex, below is the interesting stuff

data AlexState = AlexState
  { _asStartCode  :: !Int,
    _asInput      :: !AlexInput,
    -- LATER: User defined compiler directives support, the value (as in key/value) type is wrong
    _asDefines    :: !(HashMap.HashMap SBS.ByteString LBS.ByteString),
    _asSavedInput :: ![(Position, LBS.ByteString)]
  }

type Alex = StateT AlexState (ExceptT String IO)

scan :: Alex (Maybe PosToken)
scan = get >>= \(AlexState sc inp d si) -> case alexScan inp sc of
  AlexEOF -> case si of
    [] -> return Nothing
    (p, i) : t -> put (AlexState sc (AlexInput p (_aiPrevChar inp) i) d t) >> scan
  AlexError (AlexInput p pc i) ->
    throwError $
      printf
        "lexical error between %s and %s"
        (show p)
        (helperShowPositions $ _aiPosition inp :| map fst si)
  AlexSkip inp' _ -> modify' (\s -> s { _asInput = inp' }) >> scan
  AlexToken inp' n action -> do
    modify' $ \s -> s { _asInput = inp' }
    action (_aiPosition inp) $ LBS.toStrict $ LBS.take (toEnum n) $ _aiInput inp

scanTokens :: String -> IO (Either String [PosToken])
scanTokens f = do
  inp <- LBS.readFile f
  runExceptT $
    evalStateT loop $
      AlexState 0 (AlexInput (Position 1 1 $ PSFile f) '\n' inp) HashMap.empty []
  where
    loop = scan >>= maybe (pure []) (\x -> (x :) <$> loop)

type AlexAction = Position -> SBS.ByteString -> Alex (Maybe PosToken)

mkPos :: Position -> Token -> Alex (Maybe PosToken)
mkPos p t = (\l -> Just $ PosToken (p : map fst l) t) <$> gets _asSavedInput

tok :: Token -> AlexAction
tok t p _ = mkPos p t

to :: (SBS.ByteString -> Token) -> AlexAction
to f p = mkPos p . f

toa :: (SBS.ByteString -> Alex Token) -> AlexAction
toa f p s = f s >>= mkPos p

sc :: Int -> Alex ()
sc n = modify' $ \s -> s { _asStartCode = n }

startcode :: Int -> AlexAction -> AlexAction
startcode n f p s = sc n >> f p s

alexError :: Position -> String -> Alex a
alexError p s = do
  si <- gets _asSavedInput
  throwError $ s ++ " at " ++ helperShowPositions (p :| map fst si)

-- Lexer actions

tsValue :: SBS.ByteString -> Token
tsValue s = CDTSInt (SBS.length s - 1)

tsUnit :: SBS.ByteString -> Token
tsUnit s =
  CDTSUnit $
    case s of "s" -> 0 ; "ms" -> -3 ; "us" -> -6 ; "ns" -> -9 ; "ps" -> -12 ; "fs" -> -15

numberBase :: Base -> SBS.ByteString -> Alex Token
numberBase ba bs = do
  sc (case ba of BBin -> pre2 ; BOct -> pre8 ; BDec -> pre10 ; BHex -> pre16)
  return $ NumberBase (SBS.length bs == 3) ba

binary :: SBS.ByteString -> Token
binary s = LitBinary $ mapMaybe (\c -> case c of
  '0' -> Just BXZ0
  '1' -> Just BXZ1
  'x' -> Just BXZX
  'X' -> Just BXZX
  'z' -> Just BXZZ
  'Z' -> Just BXZZ
  '?' -> Just BXZZ
  _ -> Nothing) $ unpackChars s

octal :: SBS.ByteString -> Token
octal s = LitOctal $ mapMaybe (\c -> case c of
  'x' -> Just OXZX
  'X' -> Just OXZX
  'z' -> Just OXZZ
  'Z' -> Just OXZZ
  '?' -> Just OXZZ
  _ | '0' <= c && c <= '7' -> Just $ toEnum $ fromEnum c - fromEnum '0'
  _ -> Nothing) $ unpackChars s

hex :: SBS.ByteString -> Token
hex s = LitHex $ mapMaybe (\c -> case c of
  'x' -> Just HXZX
  'X' -> Just HXZX
  'z' -> Just HXZZ
  'Z' -> Just HXZZ
  '?' -> Just HXZZ
  _ | '0' <= c && c <= '9' -> Just $ toEnum $ fromEnum c - fromEnum '0'
  _ | 'a' <= c && c <= 'f' -> Just $ toEnum $ fromEnum c + 10 - fromEnum 'a'
  _ | 'A' <= c && c <= 'F' -> Just $ toEnum $ fromEnum c + 10 - fromEnum 'A'
  _ -> Nothing) $ unpackChars s

xz :: SBS.ByteString -> Token
xz s = LitXZ $ let c = SBS.head s in c == c2w 'x' || c == c2w 'X'

parseDecimal :: SBS.ByteString -> Natural
parseDecimal =
  fromInteger . SBS.foldl
    (\acc d -> if c2w '0' <= d && d <= c2w '9' then 10*acc + toInteger (d - c2w '0') else acc)
    0

decimal :: SBS.ByteString -> Token
decimal = LitDecimal . parseDecimal

unbxz :: SBS.ByteString -> BXZ
unbxz s = case s of "0" -> BXZ0; "1" -> BXZ1; "x" -> BXZX; "X" -> BXZX; "z" -> BXZZ; "Z" -> BXZZ

edgeDesc :: SBS.ByteString -> Token
edgeDesc s = EdgeEdge (unbxz $ SBS.init s) (unbxz $ SBS.tail s)

tableOut :: SBS.ByteString -> Token
tableOut s = TableOut $ case s of "0" -> ZOXZ; "1" -> ZOXO; "x" -> ZOXX; "X" -> ZOXX

tableIn :: SBS.ByteString -> Token
tableIn s = TableIn $ case s of "b" -> True; "B" -> True; "?" -> False

tableEdge :: SBS.ByteString -> Token
tableEdge s =
  TableEdge $ case s of
    "*" -> AFRNPA
    "f" -> AFRNPF
    "F" -> AFRNPF
    "r" -> AFRNPR
    "R" -> AFRNPR
    "n" -> AFRNPN
    "N" -> AFRNPN
    "p" -> AFRNPP
    "P" -> AFRNPP

cdcident :: AlexAction
cdcident p s = case HashMap.lookup (SBS.tail s) cdMap of
  Just a -> a p
  Nothing -> do
    defs <- gets _asDefines
    case HashMap.lookup s defs of
      Nothing ->
        alexError p $
          printf "Compiler directive %s not declared nor supported, preprocess input file" $
            show s
      Just i ->
        alexError p $
          printf
            "User defined compiler directive replacement in not implemented, %s was encountered"
            (show s)

kwident :: SBS.ByteString -> Alex Token
kwident s = case SBS.stripPrefix "PATHPULSE$" s of
  Just ss -> return $ TknPP ss
  Nothing -> HashMap.findWithDefault (return $ IdSimple s) s kwMap

escSimpleIdent :: SBS.ByteString -> Token
escSimpleIdent s = case SBS.uncons ss of
  Just (c, t)
    | testfirst c
      && not (isKW ss)
      && SBS.all (\c -> testfirst c || (c2w '0' <= c && c <= c2w '9') || c == c2w '$') t
    -> IdSimple ss
  _ -> IdEscaped s
  where
    testfirst c = (c2w 'A' <= c && c <= c2w 'Z') || (c2w 'a' <= c && c <= c2w 'z') || c == c2w '_'
    ss = SBS.tail s

makeString :: String -> SBS.ByteString
makeString s = packChars $ '"' : w s ++ "\""
  where
    w = concatMap $ \x -> case x of '"' -> "\\\""; '\\' -> "\\\\"; '\n' -> "\\n"; x -> [x]

cdMap :: HashMap.HashMap SBS.ByteString (Position -> Alex (Maybe PosToken))
cdMap = HashMap.fromList $
  ("include", includecompdir)
  -- LATER: `define would go here and change state to store input as is in _asDefines
  : ("line", linecompdir)
  : map (\(x, y) -> (x, \p -> y >>= mkPos p))
    ( ("timescale", sc ts0 >> return CDTimescale)
      : ("resetall", modify' (\s -> s { _asDefines = HashMap.empty }) >> return CDResetall)
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
        , ("nounconnected_drive", CDNounconnecteddrive)
        -- , ("pragma", CDPragma) -- Another layer of hell
        , ("unconnected_drive", CDUnconnecteddrive)
        ]
    )

includecompdir :: Position -> Alex (Maybe PosToken)
includecompdir _ = do
  AlexState oldsc (AlexInput oldp _ _) _ _ <- get
  sc 0
  t <- scan
  case t of
    Nothing -> return Nothing
    Just (PosToken _ (LitString s)) -> do
      let f = tail $ unpackChars $ SBS.init s
      -- LATER: search for file
      i <- liftIO $ LBS.readFile f
      modify' $ \(AlexState _ (AlexInput p pc bs) d si) ->
        AlexState oldsc (AlexInput (Position 1 1 $ PSFile f) pc i) d $ (p, bs) : si
      scan
    _ -> alexError oldp "Expected a filename to include"

linecompdir :: Position -> Alex (Maybe PosToken)
linecompdir _ = do
  AlexState oldsc (AlexInput oldp _ _) _ _ <- get
  sc 0
  l <- scan
  f <- scan
  c <- scan
  case l >>= \ll -> f >>= \ff -> c >>= \cc -> pure (ll, ff, cc) of
    Nothing -> return Nothing
    Just (PosToken _ (LitDecimal l), PosToken _ (LitString s), PosToken _ (LitDecimal n))
      | n < 3 -> do
        modify' $ \(AlexState _ (AlexInput p pc bs) d si) ->
          AlexState
            oldsc
            (AlexInput (Position (naturalToWord l - 1) 1 $ PSLine (unpackChars s) (n == 1)) pc bs)
            d
            ((p, "") : if n /= 2 then si else smashline si)
        scan
    _ -> alexError
      oldp
      "Expected a number, a filename and a number between 0 and 2 to override position"
  where
    smashline l =
      case dropWhile (\(p, _) -> case _posSource p of PSLine _ b -> not b; _ -> False) l of
        (Position _ _ (PSLine _ True), _) : t -> t
        ll -> ll

isKW :: SBS.ByteString -> Bool
isKW s = HashMap.member s kwMap

kwMap :: HashMap.HashMap SBS.ByteString (Alex Token)
kwMap = HashMap.fromList
  $ ("edge", sc edge >> return KWEdge)
  : ("endtable", sc 0 >> return KWEndtable)
  : ("table", sc table >> return KWTable)
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

}
