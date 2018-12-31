{-|
Module      : Test.VeriFuzz.Verilog.CodeGen
Description : Code generation for Verilog AST.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

This module generates the code from the Verilog AST defined in
"Test.VeriFuzz.Verilog.AST".
-}

module Test.VeriFuzz.Verilog.CodeGen where

import           Control.Lens
import           Data.Maybe                    (fromMaybe, isNothing)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Test.VeriFuzz.Internal.Shared
import           Test.VeriFuzz.Verilog.AST

showT :: (Show a) => a -> Text
showT = T.pack . show

defMap :: Maybe Statement -> Text
defMap stat = fromMaybe ";\n" $ genStatement <$> stat

-- | Convert the 'VerilogSrc' type to 'Text' so that it can be rendered.
genVerilogSrc :: VerilogSrc -> Text
genVerilogSrc source =
  fromList $ genDescription <$> source ^. getVerilogSrc

-- | Generate the 'Description' to 'Text'.
genDescription :: Description -> Text
genDescription desc =
  genModuleDecl $ desc ^. getDescription

-- | Generate the 'ModDecl' for a module and convert it to 'Text'.
genModuleDecl :: ModDecl -> Text
genModuleDecl mod =
  "module " <> mod ^. moduleId . getIdentifier
  <> ports <> ";\n"
  <> modItems
  <> "endmodule\n"
  where
    ports
      | noIn && noOut = ""
      | otherwise = "(" <> (sep ", " $ genModPort <$> outIn) <> ")"
    modItems = fromList $ genModuleItem <$> mod ^. moduleItems
    noOut = null $ mod ^. modOutPorts
    noIn = null $ mod ^. modInPorts
    outIn = (mod ^. modOutPorts) ++ (mod ^. modInPorts)

genModPort :: Port -> Text
genModPort port = port ^. portName . getIdentifier

-- | Generate the 'Port' description.
genPort :: Port -> Text
genPort port =
  t <> size <> name
  where
    t = (<>" ") . genPortType $ port ^. portType
    size
      | port ^. portSize > 1 = "[" <> showT (port ^. portSize - 1) <> ":0] "
      | otherwise = ""
    name = port ^. portName . getIdentifier

-- | Convert the 'PortDir' type to 'Text'.
genPortDir :: PortDir -> Text
genPortDir PortIn    = "input"
genPortDir PortOut   = "output"
genPortDir PortInOut = "inout"

-- | Generate a 'ModItem'.
genModuleItem :: ModItem -> Text
genModuleItem (ModCA ca) = genContAssign ca
genModuleItem (ModInst (Identifier id) (Identifier name) conn) =
  id <> " " <> name <> "(" <> sep ", " (genExpr . _modConn <$> conn) <> ")" <> ";\n"
genModuleItem (Initial stat) = "initial " <> genStatement stat
genModuleItem (Always stat) = "always " <> genStatement stat
genModuleItem (Decl dir port) =
  (fromMaybe "" $ ((<>" ") . genPortDir) <$> dir) <> genPort port <> ";\n"

-- | Generate continuous assignment
genContAssign :: ContAssign -> Text
genContAssign (ContAssign val e) =
  "  assign " <> name <> " = " <> expr <> ";\n"
  where
    name = val ^. getIdentifier
    expr = genExpr $ e

-- | Generate 'Expression' to 'Text'.
genExpr :: Expression -> Text
genExpr (OpExpr exprRhs bin exprLhs) =
  "(" <> genExpr exprRhs <> genBinaryOperator bin <> genExpr exprLhs <> ")"
genExpr (PrimExpr prim) = genPrimary prim
genExpr (UnPrimExpr u e) =
  "(" <> genUnaryOperator u <> genPrimary e <> ")"
genExpr (CondExpr l t f) =
  "(" <> genExpr l <> " ? " <> genExpr t <> " : " <> genExpr f <> ")"
genExpr (ExprStr t) = "\"" <> t <> "\""

-- | Generate a 'PrimaryExpression' to 'Text'.
genPrimary :: Primary -> Text
genPrimary (PrimNum num) =
  "(" <> neg <> sh (num ^. numSize) <> "'d" <> (sh . abs) n <> ")"
  where
    sh = T.pack . show
    abs x = if x <= 0 then -x else x
    n = num ^. numVal
    neg = if n <= 0 then "-" else ""
genPrimary (PrimId ident) = ident ^. getIdentifier

-- | Convert 'BinaryOperator' to 'Text'.
genBinaryOperator :: BinaryOperator -> Text
genBinaryOperator BinPlus    = " + "
genBinaryOperator BinMinus   = " - "
genBinaryOperator BinTimes   = " * "
genBinaryOperator BinDiv     = " / "
genBinaryOperator BinMod     = " % "
genBinaryOperator BinEq      = " == "
genBinaryOperator BinNEq     = " != "
genBinaryOperator BinCEq     = " === "
genBinaryOperator BinCNEq    = " !== "
genBinaryOperator BinLAnd    = " && "
genBinaryOperator BinLOr     = " || "
genBinaryOperator BinLT      = " < "
genBinaryOperator BinLEq     = " <= "
genBinaryOperator BinGT      = " > "
genBinaryOperator BinGEq     = " >= "
genBinaryOperator BinAnd     = " & "
genBinaryOperator BinOr      = " | "
genBinaryOperator BinXor     = " ^ "
genBinaryOperator BinXNor    = " ^~ "
genBinaryOperator BinXNorInv = " ~^ "
genBinaryOperator BinPower   = " ** "
genBinaryOperator BinLSL     = " << "
genBinaryOperator BinLSR     = " >> "
genBinaryOperator BinASL     = " <<< "
genBinaryOperator BinASR     = " >>> "

genUnaryOperator :: UnaryOperator -> Text
genUnaryOperator UnPlus    = "+"
genUnaryOperator UnMinus   = "-"
genUnaryOperator UnNot     = "!"
genUnaryOperator UnAnd     = "&"
genUnaryOperator UnNand    = "~&"
genUnaryOperator UnOr      = "|"
genUnaryOperator UnNor     = "~|"
genUnaryOperator UnXor     = "^"
genUnaryOperator UnNxor    = "~^"
genUnaryOperator UnNxorInv = "^~"

genNet :: Net -> Text
genNet Wire    = "wire"
genNet Tri     = "tri"
genNet Tri1    = "tri1"
genNet Supply0 = "supply0"
genNet Wand    = "wand"
genNet TriAnd  = "triand"
genNet Tri0    = "tri0"
genNet Supply1 = "supply1"
genNet Wor     = "wor"
genNet Trior   = "trior"

genEvent :: Event -> Text
genEvent (EId id)     = "@(" <> id ^. getIdentifier <> ")"
genEvent (EExpr expr) = "@(" <> genExpr expr <> ")"
genEvent EAll         = "@*"

genDelay :: Delay -> Text
genDelay (Delay i) = "#" <> showT i

genRegLVal :: RegLVal -> Text
genRegLVal (RegId id) = id ^. getIdentifier
genRegLVal (RegExpr id expr) =
  id ^. getIdentifier <> " [" <> genExpr expr <> "]"
genRegLVal (RegSize id msb lsb) =
  id ^. getIdentifier <> " [" <> genConstExpr msb <> ":" <> genConstExpr lsb <> "]"

genConstExpr :: ConstExpr -> Text
genConstExpr (ConstExpr num) = showT num

genPortType :: PortType -> Text
genPortType (PortNet net) = genNet net
genPortType (Reg signed)
  | signed = "reg signed"
  | otherwise = "reg"

genAssign :: Text -> Assign -> Text
genAssign op (Assign r d e) =
  genRegLVal r <> op <> fromMaybe "" (genDelay <$> d) <> genExpr e

genStatement :: Statement -> Text
genStatement (TimeCtrl d stat) = genDelay d <> " " <> defMap stat
genStatement (EventCtrl e stat) = genEvent e <> " " <> defMap stat
genStatement (SeqBlock s) =
  "begin\n" <> fromList (genStatement <$> s) <> "end\n"
genStatement (BlockAssign a) = genAssign " = " a <> ";\n"
genStatement (NonBlockAssign a) = genAssign " <= " a <> ";\n"
genStatement (StatCA a) = genContAssign a
genStatement (TaskEnable task) = genTask task <> ";\n"
genStatement (SysTaskEnable task) = "$" <> genTask task <> ";\n"

genTask :: Task -> Text
genTask (Task name expr)
  | null expr = id
  | otherwise = id <> "(" <> sep ", " (genExpr <$> expr) <> ")"
  where
    id = name ^. getIdentifier

-- | Render the 'Text' to 'IO'. This is equivalent to 'putStrLn'.
render :: Text -> IO ()
render = T.putStrLn

-- Instances

instance Source Task where
  genSource = genTask

instance Source Statement where
  genSource = genStatement

instance Source PortType where
  genSource = genPortType

instance Source ConstExpr where
  genSource = genConstExpr

instance Source RegLVal where
  genSource = genRegLVal

instance Source Delay where
  genSource = genDelay

instance Source Event where
  genSource = genEvent

instance Source Net where
  genSource = genNet

instance Source UnaryOperator where
  genSource = genUnaryOperator

instance Source Primary where
  genSource = genPrimary

instance Source Expression where
  genSource = genExpr

instance Source ContAssign where
  genSource = genContAssign

instance Source ModItem where
  genSource = genModuleItem

instance Source PortDir where
  genSource = genPortDir

instance Source Port where
  genSource = genPort

instance Source ModDecl where
  genSource = genModuleDecl

instance Source Description where
  genSource = genDescription

instance Source VerilogSrc where
  genSource = genVerilogSrc

newtype SourceShowable a = SrcShow { unSrcShow :: a }

instance (Source a) => Show (SourceShowable a) where
  show s = T.unpack $ genSource (unSrcShow s)
