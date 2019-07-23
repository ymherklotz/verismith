{-|
Module      : VeriFuzz.Verilog.CodeGen
Description : Code generation for Verilog AST.
Copyright   : (c) 2018-2019, Yann Herklotz
License     : BSD-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

This module generates the code from the Verilog AST defined in
"VeriFuzz.Verilog.AST".
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}

module VeriFuzz.Verilog.CodeGen
    ( -- * Code Generation
      GenVerilog(..)
    , Source(..)
    , render
    )
where

import           Data.Data                 (Data)
import           Data.List.NonEmpty        (NonEmpty (..), toList)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc
import           Numeric                   (showHex)
import           VeriFuzz.Internal         hiding (comma)
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.BitVec

-- | 'Source' class which determines that source code is able to be generated
-- from the data structure using 'genSource'. This will be stored in 'Text' and
-- can then be processed further.
class Source a where
  genSource :: a -> Text

-- | Map a 'Maybe Statement' to 'Text'. If it is 'Just statement', the generated
-- statements are returned. If it is 'Nothing', then @;\n@ is returned.
defMap :: Maybe Statement -> Doc a
defMap = maybe semi statement

-- | Convert the 'Verilog' type to 'Text' so that it can be rendered.
verilogSrc :: Verilog -> Doc a
verilogSrc (Verilog modules) = vsep . punctuate line $ moduleDecl <$> modules

-- | Generate the 'ModDecl' for a module and convert it to 'Text'.
moduleDecl :: ModDecl -> Doc a
moduleDecl (ModDecl i outP inP items ps) = vsep
    [ sep ["module" <+> identifier i, params ps, ports <> semi]
    , indent 2 modI
    , "endmodule"
    ]
  where
    ports
        | null outP && null inP = ""
        | otherwise = parens . align . sep . punctuate comma $ modPort <$> outIn
    modI  = vsep $ moduleItem <$> items
    outIn = outP ++ inP
    params []        = ""
    params (p : pps) = hcat ["#", paramList (p :| pps)]

-- | Generates a parameter list. Can only be called with a 'NonEmpty' list.
paramList :: NonEmpty Parameter -> Doc a
paramList ps = tupled . toList $ parameter <$> ps

-- | Generates a localparam list. Can only be called with a 'NonEmpty' list.
localParamList :: NonEmpty LocalParam -> Doc a
localParamList ps = tupled . toList $ localParam <$> ps

-- | Generates the assignment for a 'Parameter'.
parameter :: Parameter -> Doc a
parameter (Parameter name val) =
    hsep ["parameter", identifier name, "=", constExpr val]

-- | Generates the assignment for a 'LocalParam'.
localParam :: LocalParam -> Doc a
localParam (LocalParam name val) =
    hsep ["localparameter", identifier name, "=", constExpr val]

identifier :: Identifier -> Doc a
identifier (Identifier i) = pretty i

-- | Conversts 'Port' to 'Text' for the module list, which means it only
-- generates a list of identifiers.
modPort :: Port -> Doc a
modPort (Port _ _ _ i) = identifier i

-- | Generate the 'Port' description.
port :: Port -> Doc a
port (Port tp sgn r name) = hsep [t, sign, range r, identifier name]
  where
    t    = pType tp
    sign = signed sgn

range :: Range -> Doc a
range (Range msb lsb) = brackets $ hcat [constExpr msb, colon, constExpr lsb]

signed :: Bool -> Doc a
signed True = "signed"
signed _    = mempty

-- | Convert the 'PortDir' type to 'Text'.
portDir :: PortDir -> Doc a
portDir PortIn    = "input"
portDir PortOut   = "output"
portDir PortInOut = "inout"

-- | Generate a 'ModItem'.
moduleItem :: ModItem -> Doc a
moduleItem (ModCA ca           ) = contAssign ca
moduleItem (ModInst i name conn) = hsep
    [ identifier i
    , identifier name
    , parens . hsep $ punctuate comma (mConn <$> conn)
    , semi
    ]
moduleItem (Initial stat  ) = nest 2 $ vsep ["initial", statement stat]
moduleItem (Always  stat  ) = nest 2 $ vsep ["always", statement stat]
moduleItem (Decl dir p ini) = hsep
    [maybe mempty makePort dir, port p, maybe mempty makeIni ini, semi]
  where
    makePort = portDir
    makeIni  = ("=" <+>) . constExpr
moduleItem (ParamDecl      p) = hcat [paramList p, semi]
moduleItem (LocalParamDecl p) = hcat [localParamList p, semi]

mConn :: ModConn -> Doc a
mConn (ModConn c       ) = expr c
mConn (ModConnNamed n c) = hcat [dot, identifier n, parens $ expr c]

-- | Generate continuous assignment
contAssign :: ContAssign -> Doc a
contAssign (ContAssign val e) =
    hsep ["assign", identifier val, "=", align $ expr e, semi]

-- | Generate 'Expr' to 'Text'.
expr :: Expr -> Doc a
expr (BinOp eRhs bin eLhs) = parens $ hsep [expr eRhs, binaryOp bin, expr eLhs]
expr (Number b           ) = showNum b
expr (Id     i           ) = identifier i
expr (VecSelect   i e    ) = hcat [identifier i, brackets $ expr e]
expr (RangeSelect i r    ) = hcat [identifier i, range r]
expr (Concat c) = braces . nest 4 . sep . punctuate comma $ toList (expr <$> c)
expr (UnOp u e           ) = parens $ hcat [unaryOp u, expr e]
expr (Cond l t f) =
    parens . nest 4 $ sep [expr l <+> "?", hsep [expr t, colon, expr f]]
expr (Appl f e) = hcat [identifier f, parens $ expr e]
expr (Str t   ) = dquotes $ pretty t

showNum :: BitVec -> Doc a
showNum (BitVec s n) = parens
    $ hcat [minus, pretty $ showT s, "'h", pretty $ T.pack (showHex (abs n) "")]
  where
    minus | signum n >= 0 = mempty
          | otherwise     = "-"

constExpr :: ConstExpr -> Doc a
constExpr (ConstNum b) = showNum b
constExpr (ParamId  i) = identifier i
constExpr (ConstConcat c) =
    braces . hsep . punctuate comma $ toList (constExpr <$> c)
constExpr (ConstUnOp u e) = parens $ hcat [unaryOp u, constExpr e]
constExpr (ConstBinOp eRhs bin eLhs) =
    parens $ hsep [constExpr eRhs, binaryOp bin, constExpr eLhs]
constExpr (ConstCond l t f) =
    parens $ hsep [constExpr l, "?", constExpr t, colon, constExpr f]
constExpr (ConstStr t) = dquotes $ pretty t

-- | Convert 'BinaryOperator' to 'Text'.
binaryOp :: BinaryOperator -> Doc a
binaryOp BinPlus    = "+"
binaryOp BinMinus   = "-"
binaryOp BinTimes   = "*"
binaryOp BinDiv     = "/"
binaryOp BinMod     = "%"
binaryOp BinEq      = "=="
binaryOp BinNEq     = "!="
binaryOp BinCEq     = "==="
binaryOp BinCNEq    = "!=="
binaryOp BinLAnd    = "&&"
binaryOp BinLOr     = "||"
binaryOp BinLT      = "<"
binaryOp BinLEq     = "<="
binaryOp BinGT      = ">"
binaryOp BinGEq     = ">="
binaryOp BinAnd     = "&"
binaryOp BinOr      = "|"
binaryOp BinXor     = "^"
binaryOp BinXNor    = "^~"
binaryOp BinXNorInv = "~^"
binaryOp BinPower   = "**"
binaryOp BinLSL     = "<<"
binaryOp BinLSR     = ">>"
binaryOp BinASL     = "<<<"
binaryOp BinASR     = ">>>"

-- | Convert 'UnaryOperator' to 'Text'.
unaryOp :: UnaryOperator -> Doc a
unaryOp UnPlus    = "+"
unaryOp UnMinus   = "-"
unaryOp UnLNot    = "!"
unaryOp UnNot     = "~"
unaryOp UnAnd     = "&"
unaryOp UnNand    = "~&"
unaryOp UnOr      = "|"
unaryOp UnNor     = "~|"
unaryOp UnXor     = "^"
unaryOp UnNxor    = "~^"
unaryOp UnNxorInv = "^~"

event :: Event -> Doc a
event a = hcat ["@", parens $ eventRec a]

-- | Generate verilog code for an 'Event'.
eventRec :: Event -> Doc a
eventRec (EId   i)    = identifier i
eventRec (EExpr e)    = expr e
eventRec EAll         = "*"
eventRec (EPosEdge i) = hsep ["posedge", identifier i]
eventRec (ENegEdge i) = hsep ["negedge", identifier i]
eventRec (EOr   a b ) = hsep [eventRec a, "or", eventRec b]
eventRec (EComb a b ) = hsep $ punctuate comma [eventRec a, eventRec b]

-- | Generates verilog code for a 'Delay'.
delay :: Delay -> Doc a
delay (Delay i) = "#" <> pretty i

-- | Generate the verilog code for an 'LVal'.
lVal :: LVal -> Doc a
lVal (RegId i    ) = identifier i
lVal (RegExpr i e) = hsep [identifier i, expr e]
lVal (RegSize i r) = hsep [identifier i, range r]
lVal (RegConcat e) = braces . hsep $ punctuate comma (expr <$> e)

pType :: PortType -> Doc a
pType Wire = "wire"
pType Reg  = "reg"

genAssign :: Text -> Assign -> Doc a
genAssign op (Assign r d e) =
    hsep [lVal r, pretty op, maybe mempty delay d, expr e]

statement :: Statement -> Doc a
statement (TimeCtrl  d stat) = hsep [delay d, defMap stat]
statement (EventCtrl e stat) = hsep [event e, defMap stat]
statement (SeqBlock s) =
    vsep ["begin", indent 2 . vsep $ statement <$> s, "end"]
statement (BlockAssign    a) = hcat [genAssign "=" a, semi]
statement (NonBlockAssign a) = hcat [genAssign "<=" a, semi]
statement (TaskEnable     t) = hcat [task t, semi]
statement (SysTaskEnable  t) = hcat ["$", task t, semi]
statement (CondStmnt e t Nothing) =
    vsep [hsep ["if", parens $ expr e], indent 2 $ defMap t]
statement (CondStmnt e t f) = vsep
    [ hsep ["if", parens $ expr e]
    , indent 2 $ defMap t
    , "else"
    , indent 2 $ defMap f
    ]
statement (ForLoop a e incr stmnt) = vsep
    [ hsep
        [ "for"
        , parens . hsep $ punctuate
            semi
            [genAssign "=" a, expr e, genAssign "=" incr]
        ]
    , indent 2 $ statement stmnt
    ]

task :: Task -> Doc a
task (Task i e)
    | null e = identifier i
    | otherwise = hsep
        [identifier i, parens . hsep $ punctuate comma (expr <$> e)]

-- | Render the 'Text' to 'IO'. This is equivalent to 'putStrLn'.
render :: (Source a) => a -> IO ()
render = print . genSource

-- Instances

instance Source Identifier where
    genSource = showT . identifier

instance Source Task where
    genSource = showT . task

instance Source Statement where
    genSource = showT . statement

instance Source PortType where
    genSource = showT . pType

instance Source ConstExpr where
    genSource = showT . constExpr

instance Source LVal where
    genSource = showT . lVal

instance Source Delay where
    genSource = showT . delay

instance Source Event where
    genSource = showT . event

instance Source UnaryOperator where
    genSource = showT . unaryOp

instance Source Expr where
    genSource = showT . expr

instance Source ContAssign where
    genSource = showT . contAssign

instance Source ModItem where
    genSource = showT . moduleItem

instance Source PortDir where
    genSource = showT . portDir

instance Source Port where
    genSource = showT . port

instance Source ModDecl where
    genSource = showT . moduleDecl

instance Source Verilog where
    genSource = showT . verilogSrc

instance Source SourceInfo where
    genSource (SourceInfo _ src) = genSource src

newtype GenVerilog a = GenVerilog { unGenVerilog :: a }
                     deriving (Eq, Ord, Data)

instance (Source a) => Show (GenVerilog a) where
    show = T.unpack . genSource . unGenVerilog
