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

import           Control.Lens               (view, (^.))
import           Data.Foldable              (fold)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Numeric                    (showHex)
import           VeriFuzz.Internal
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Verilog.Arbitrary
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
verilogSrc source = fold $ description <$> source ^. getVerilog

-- | Generate the 'Description' to 'Text'.
description :: Description -> Text
description desc = moduleDecl $ desc ^. getDescription

-- | Generate the 'ModDecl' for a module and convert it to 'Text'.
moduleDecl :: ModDecl -> Text
moduleDecl m =
    "module "
        <> m
        ^. modId
        .  getIdentifier
        <> ports
        <> ";\n"
        <> modI
        <> "endmodule\n"
  where
    ports | noIn && noOut = ""
          | otherwise     = "(" <> comma (modPort <$> outIn) <> ")"
    modI  = fold $ moduleItem <$> m ^. modItems
    noOut = null $ m ^. modOutPorts
    noIn  = null $ m ^. modInPorts
    outIn = (m ^. modOutPorts) ++ (m ^. modInPorts)

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
moduleItem (Decl dir p  ) = maybe "" makePort dir <> port p <> ";\n"
    where makePort = (<> " ") . portDir

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
func UnSignedFunc = "$unsigned"

-- | Generate 'Expr' to 'Text'.
expr :: Expr -> Text
expr (BinOp eRhs bin eLhs) =
    "(" <> expr eRhs <> binaryOp bin <> expr eLhs <> ")"
expr (Number s n) =
    "(" <> minus <> showT s <> "'h" <> T.pack (showHex (abs n) "") <> ")"
  where
    minus | signum n >= 0 = ""
          | otherwise     = "-"
expr (Id     i  ) = i ^. getIdentifier
expr (Concat c  ) = "{" <> comma (expr <$> c) <> "}"
expr (UnOp u e  ) = "(" <> unaryOp u <> expr e <> ")"
expr (Cond l t f) = "(" <> expr l <> " ? " <> expr t <> " : " <> expr f <> ")"
expr (Func f e  ) = func f <> "(" <> expr e <> ")"
expr (Str t     ) = "\"" <> t <> "\""

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

constExpr :: ConstExpr -> Text
constExpr (ConstExpr num) = showT num

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
statement (StatCA         a     ) = contAssign a
statement (TaskEnable     t     ) = task t <> ";\n"
statement (SysTaskEnable  t     ) = "$" <> task t <> ";\n"
statement (CondStmnt e t Nothing) = "if(" <> expr e <> ")\n" <> defMap t
statement (CondStmnt e t f) =
    "if(" <> expr e <> ")\n" <> defMap t <> "else\n" <> defMap f

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

instance Source Description where
    genSource = description

instance Source Verilog where
    genSource = verilogSrc

newtype GenVerilog a = GenVerilog { unGenVerilog :: a }

instance (Source a) => Show (GenVerilog a) where
    show = T.unpack . genSource . unGenVerilog

instance (Arb a) => Arb (GenVerilog a) where
    arb = GenVerilog <$> arb

instance Source SourceInfo where
    genSource (SourceInfo _ src) = genSource src
