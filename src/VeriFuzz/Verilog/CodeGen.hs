{-|
Module      : VeriFuzz.Verilog.CodeGen
Description : Code generation for Verilog AST.
Copyright   : (c) 2018-2019, Yann Herklotz
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

This module generates the code from the Verilog AST defined in
"VeriFuzz.Verilog.AST".
-}

{-# LANGUAGE FlexibleInstances #-}

module VeriFuzz.Verilog.CodeGen
    ( -- * Code Generation
      GenVerilog(..)
    , genSource
    , render
    )
where

import           Control.Lens          (view, (^.))
import           Data.Foldable         (fold)
import           Data.List.NonEmpty    (NonEmpty (..), toList)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           Numeric               (showHex)
import           VeriFuzz.Internal
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Verilog.AST

-- | 'Source' class which determines that source code is able to be generated
-- from the data structure using 'genSource'. This will be stored in 'Text' and
-- can then be processed further.
class Source a where
  genSource :: a -> Text

-- | Map a 'Maybe Statement' to 'Text'. If it is 'Just statement', the generated
-- statements are returned. If it is 'Nothing', then @;\n@ is returned.
defMap :: Maybe Statement -> Text
defMap = maybe ";\n" statement

-- | Convert the 'Verilog' type to 'Text' so that it can be rendered.
verilogSrc :: Verilog -> Text
verilogSrc (Verilog modules) = fold $ moduleDecl <$> modules

-- | Generate the 'ModDecl' for a module and convert it to 'Text'.
moduleDecl :: ModDecl -> Text
moduleDecl (ModDecl i outP inP items ps) =
    "module "
        <> identifier i
        <> params ps
        <> ports
        <> ";\n"
        <> modI
        <> "endmodule\n"
  where
    ports | null outP && null inP = ""
          | otherwise             = "(" <> comma (modPort <$> outIn) <> ")"
    modI  = fold $ moduleItem <$> items
    outIn = outP ++ inP
    params []        = ""
    params (p : pps) = "\n#(\n" <> paramList (p :| pps) <> "\n)\n"

-- | Generates a parameter list. Can only be called with a 'NonEmpty' list.
paramList :: NonEmpty Parameter -> Text
paramList ps = "parameter " <> (commaNL . toList $ parameter <$> ps)

-- | Generates a localparam list. Can only be called with a 'NonEmpty' list.
localParamList :: NonEmpty LocalParam -> Text
localParamList ps = "localparam " <> (commaNL . toList $ localParam <$> ps)

-- | Generates the assignment for a 'Parameter'.
parameter :: Parameter -> Text
parameter (Parameter name val) = identifier name <> " = " <> constExpr val

-- | Generates the assignment for a 'LocalParam'.
localParam :: LocalParam -> Text
localParam (LocalParam name val) = identifier name <> " = " <> constExpr val

identifier :: Identifier -> Text
identifier (Identifier i) = i

-- | Conversts 'Port' to 'Text' for the module list, which means it only
-- generates a list of identifiers.
modPort :: Port -> Text
modPort p = p ^. portName . getIdentifier

-- | Generate the 'Port' description.
port :: Port -> Text
port p = t <> sign <> size <> name
  where
    t = flip mappend " " . pType $ p ^. portType
    size | p ^. portSize > 1 = "[" <> showT (p ^. portSize - 1) <> ":0] "
         | otherwise         = ""
    name = p ^. portName . getIdentifier
    sign = signed $ p ^. portSigned

signed :: Bool -> Text
signed True = "signed "
signed _    = ""

-- | Convert the 'PortDir' type to 'Text'.
portDir :: PortDir -> Text
portDir PortIn    = "input"
portDir PortOut   = "output"
portDir PortInOut = "inout"

-- | Generate a 'ModItem'.
moduleItem :: ModItem -> Text
moduleItem (ModCA ca) = contAssign ca
moduleItem (ModInst (Identifier i) (Identifier name) conn) =
    i <> " " <> name <> "(" <> comma (mConn <$> conn) <> ")" <> ";\n"
moduleItem (Initial stat) = "initial " <> statement stat
moduleItem (Always  stat) = "always " <> statement stat
moduleItem (Decl dir p ini ) = maybe "" makePort dir <> port p <> maybe "" makeIni ini <> ";\n"
    where makePort = (<> " ") . portDir
          makeIni = (" = " <>) . constExpr
moduleItem (ParamDecl      p) = paramList p <> ";\n"
moduleItem (LocalParamDecl p) = localParamList p <> ";\n"

mConn :: ModConn -> Text
mConn (ModConn c       ) = expr c
mConn (ModConnNamed n c) = "." <> n ^. getIdentifier <> "(" <> expr c <> ")"

-- | Generate continuous assignment
contAssign :: ContAssign -> Text
contAssign (ContAssign val e) =
    "assign " <> val ^. getIdentifier <> " = " <> expr e <> ";\n"

-- | Generate 'Function' to 'Text'
func :: Function -> Text
func SignedFunc   = "$signed"
func UnsignedFunc = "$unsigned"

-- | Generate 'Expr' to 'Text'.
expr :: Expr -> Text
expr (BinOp eRhs bin eLhs) =
    "(" <> expr eRhs <> binaryOp bin <> expr eLhs <> ")"
expr (Number s n) = showNum s n
expr (Id     i  ) = i ^. getIdentifier
expr (Concat c  ) = "{" <> comma (expr <$> c) <> "}"
expr (UnOp u e  ) = "(" <> unaryOp u <> expr e <> ")"
expr (Cond l t f) = "(" <> expr l <> " ? " <> expr t <> " : " <> expr f <> ")"
expr (Func f e  ) = func f <> "(" <> expr e <> ")"
expr (Str t     ) = "\"" <> t <> "\""

showNum :: Int -> Integer -> Text
showNum s n =
    "(" <> minus <> showT s <> "'h" <> T.pack (showHex (abs n) "") <> ")"
  where
    minus | signum n >= 0 = ""
          | otherwise     = "-"

constExpr :: ConstExpr -> Text
constExpr (ConstNum s n ) = showNum s n
constExpr (ParamId     i) = identifier i
constExpr (ConstConcat c) = "{" <> comma (constExpr <$> c) <> "}"
constExpr (ConstUnOp u e) = "(" <> unaryOp u <> constExpr e <> ")"
constExpr (ConstBinOp eRhs bin eLhs) =
    "(" <> constExpr eRhs <> binaryOp bin <> constExpr eLhs <> ")"
constExpr (ConstCond l t f) =
    "(" <> constExpr l <> " ? " <> constExpr t <> " : " <> constExpr f <> ")"
constExpr (ConstStr t) = "\"" <> t <> "\""

-- | Convert 'BinaryOperator' to 'Text'.
binaryOp :: BinaryOperator -> Text
binaryOp BinPlus    = " + "
binaryOp BinMinus   = " - "
binaryOp BinTimes   = " * "
binaryOp BinDiv     = " / "
binaryOp BinMod     = " % "
binaryOp BinEq      = " == "
binaryOp BinNEq     = " != "
binaryOp BinCEq     = " === "
binaryOp BinCNEq    = " !== "
binaryOp BinLAnd    = " && "
binaryOp BinLOr     = " || "
binaryOp BinLT      = " < "
binaryOp BinLEq     = " <= "
binaryOp BinGT      = " > "
binaryOp BinGEq     = " >= "
binaryOp BinAnd     = " & "
binaryOp BinOr      = " | "
binaryOp BinXor     = " ^ "
binaryOp BinXNor    = " ^~ "
binaryOp BinXNorInv = " ~^ "
binaryOp BinPower   = " ** "
binaryOp BinLSL     = " << "
binaryOp BinLSR     = " >> "
binaryOp BinASL     = " <<< "
binaryOp BinASR     = " >>> "

-- | Convert 'UnaryOperator' to 'Text'.
unaryOp :: UnaryOperator -> Text
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

-- | Generate verilog code for an 'Event'.
event :: Event -> Text
event (EId   i)    = "@(" <> i ^. getIdentifier <> ")"
event (EExpr e)    = "@(" <> expr e <> ")"
event EAll         = "@*"
event (EPosEdge i) = "@(posedge " <> i ^. getIdentifier <> ")"
event (ENegEdge i) = "@(negedge " <> i ^. getIdentifier <> ")"

-- | Generates verilog code for a 'Delay'.
delay :: Delay -> Text
delay (Delay i) = "#" <> showT i

-- | Generate the verilog code for an 'LVal'.
lVal :: LVal -> Text
lVal (RegId i    ) = i ^. getIdentifier
lVal (RegExpr i e) = i ^. getIdentifier <> " [" <> expr e <> "]"
lVal (RegSize i msb lsb) =
    i ^. getIdentifier <> " [" <> constExpr msb <> ":" <> constExpr lsb <> "]"
lVal (RegConcat e) = "{" <> comma (expr <$> e) <> "}"

pType :: PortType -> Text
pType Wire = "wire"
pType Reg  = "reg"

genAssign :: Text -> Assign -> Text
genAssign op (Assign r d e) = lVal r <> op <> maybe "" delay d <> expr e

statement :: Statement -> Text
statement (TimeCtrl  d stat     ) = delay d <> " " <> defMap stat
statement (EventCtrl e stat     ) = event e <> " " <> defMap stat
statement (SeqBlock s) = "begin\n" <> fold (statement <$> s) <> "end\n"
statement (BlockAssign    a     ) = genAssign " = " a <> ";\n"
statement (NonBlockAssign a     ) = genAssign " <= " a <> ";\n"
statement (TaskEnable     t     ) = task t <> ";\n"
statement (SysTaskEnable  t     ) = "$" <> task t <> ";\n"
statement (CondStmnt e t Nothing) = "if(" <> expr e <> ")\n" <> defMap t
statement (CondStmnt e t f) =
    "if(" <> expr e <> ")\n" <> defMap t <> "else\n" <> defMap f
statement (ForLoop a e incr stmnt) =
    "for("
        <> genAssign " = " a
        <> "; "
        <> expr e
        <> "; "
        <> genAssign " = " incr
        <> ")\n"
        <> statement stmnt

task :: Task -> Text
task (Task name e) | null e    = i
                   | otherwise = i <> "(" <> comma (expr <$> e) <> ")"
    where i = name ^. getIdentifier

-- | Render the 'Text' to 'IO'. This is equivalent to 'putStrLn'.
render :: (Source a) => a -> IO ()
render = T.putStrLn . genSource

-- Instances

instance Source Identifier where
    genSource = view getIdentifier

instance Source Task where
    genSource = task

instance Source Statement where
    genSource = statement

instance Source PortType where
    genSource = pType

instance Source ConstExpr where
    genSource = constExpr

instance Source LVal where
    genSource = lVal

instance Source Delay where
    genSource = delay

instance Source Event where
    genSource = event

instance Source UnaryOperator where
    genSource = unaryOp

instance Source Expr where
    genSource = expr

instance Source ContAssign where
    genSource = contAssign

instance Source ModItem where
    genSource = moduleItem

instance Source PortDir where
    genSource = portDir

instance Source Port where
    genSource = port

instance Source ModDecl where
    genSource = moduleDecl

instance Source Verilog where
    genSource = verilogSrc

instance Source SourceInfo where
    genSource (SourceInfo _ src) = genSource src

newtype GenVerilog a = GenVerilog { unGenVerilog :: a }

instance (Source a) => Show (GenVerilog a) where
    show = T.unpack . genSource . unGenVerilog
