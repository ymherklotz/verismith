{-|
Module      : VeriFuzz.Verilog.CodeGen
Description : Code generation for Verilog AST.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

This module generates the code from the Verilog AST defined in
"VeriFuzz.Verilog.AST".
-}

{-# LANGUAGE FlexibleInstances #-}

module VeriFuzz.Verilog.CodeGen where

import           Control.Lens                   ( view
                                                , (^.)
                                                )
import           Data.Foldable                  ( fold )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Numeric                        ( showHex )
import           VeriFuzz.Verilog.AST

-- | 'Source' class which determines that source code is able to be generated
-- from the data structure using 'genSource'. This will be stored in 'Text' and
-- can then be processed further.
class Source a where
  genSource :: a -> Text

-- | Inserts commas between '[Text]' and except the last one.
comma :: [Text] -> Text
comma = T.intercalate ", "

-- | Show function for 'Text'
showT :: (Show a) => a -> Text
showT = T.pack . show

-- | Map a 'Maybe Stmnt' to 'Text'. If it is 'Just stmnt', the generated
-- statements are returned. If it is 'Nothing', then @;\n@ is returned.
defMap :: Maybe Stmnt -> Text
defMap = maybe ";\n" genStmnt

-- | Convert the 'VerilogSrc' type to 'Text' so that it can be rendered.
genVerilogSrc :: VerilogSrc -> Text
genVerilogSrc source = fold $ genDescription <$> source ^. getVerilogSrc

-- | Generate the 'Description' to 'Text'.
genDescription :: Description -> Text
genDescription desc = genModuleDecl $ desc ^. getDescription

-- | Generate the 'ModDecl' for a module and convert it to 'Text'.
genModuleDecl :: ModDecl -> Text
genModuleDecl m =
  "module "
    <> m
    ^. moduleId
    .  getIdentifier
    <> ports
    <> ";\n"
    <> modI
    <> "endmodule\n"
 where
  ports | noIn && noOut = ""
        | otherwise     = "(" <> comma (genModPort <$> outIn) <> ")"
  modI  = fold $ genModuleItem <$> m ^. modItems
  noOut = null $ m ^. modOutPorts
  noIn  = null $ m ^. modInPorts
  outIn = (m ^. modOutPorts) ++ (m ^. modInPorts)

-- | Conversts 'Port' to 'Text' for the module list, which means it only
-- generates a list of identifiers.
genModPort :: Port -> Text
genModPort port = port ^. portName . getIdentifier

-- | Generate the 'Port' description.
genPort :: Port -> Text
genPort port = t <> size <> name
 where
  t = (<> " ") . genPortType $ port ^. portType
  size | port ^. portSize > 1 = "[" <> showT (port ^. portSize - 1) <> ":0] "
       | otherwise            = ""
  name = port ^. portName . getIdentifier

-- | Convert the 'PortDir' type to 'Text'.
genPortDir :: PortDir -> Text
genPortDir PortIn    = "input"
genPortDir PortOut   = "output"
genPortDir PortInOut = "inout"

-- | Generate a 'ModItem'.
genModuleItem :: ModItem -> Text
genModuleItem (ModCA ca) = genContAssign ca
genModuleItem (ModInst (Identifier i) (Identifier name) conn) =
  i <> " " <> name <> "(" <> comma (genExpr . _modConn <$> conn) <> ")" <> ";\n"
genModuleItem (Initial stat) = "initial " <> genStmnt stat
genModuleItem (Always  stat) = "always " <> genStmnt stat
genModuleItem (Decl dir port) =
  (maybe "" makePort dir) <> genPort port <> ";\n"
  where makePort = (<> " ") . genPortDir

-- | Generate continuous assignment
genContAssign :: ContAssign -> Text
genContAssign (ContAssign val e) = "assign " <> name <> " = " <> expr <> ";\n"
 where
  name = val ^. getIdentifier
  expr = genExpr e

-- | Generate 'Expr' to 'Text'.
genExpr :: Expr -> Text
genExpr (BinOp eRhs bin eLhs) =
  "(" <> genExpr eRhs <> genBinaryOperator bin <> genExpr eLhs <> ")"
genExpr (Number s n) = showT s <> "'h" <> T.pack (showHex n "")
genExpr (Id     i  ) = i ^. getIdentifier
genExpr (Concat c  ) = "{" <> comma (genExpr <$> c) <> "}"
genExpr (UnOp u e  ) = "(" <> genUnaryOperator u <> genExpr e <> ")"
genExpr (Cond l t f) =
  "(" <> genExpr l <> " ? " <> genExpr t <> " : " <> genExpr f <> ")"
genExpr (Str t) = "\"" <> t <> "\""

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

-- | Convert 'UnaryOperator' to 'Text'.
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

-- | Generate verilog code for an 'Event'.
genEvent :: Event -> Text
genEvent (EId   i   ) = "@(" <> i ^. getIdentifier <> ")"
genEvent (EExpr expr) = "@(" <> genExpr expr <> ")"
genEvent EAll         = "@*"

-- | Generates verilog code for a 'Delay'.
genDelay :: Delay -> Text
genDelay (Delay i) = "#" <> showT i

-- | Generate the verilog code for an 'LVal'.
genLVal :: LVal -> Text
genLVal (RegId i       ) = i ^. getIdentifier
genLVal (RegExpr i expr) = i ^. getIdentifier <> " [" <> genExpr expr <> "]"
genLVal (RegSize i msb lsb) =
  i
    ^. getIdentifier
    <> " ["
    <> genConstExpr msb
    <> ":"
    <> genConstExpr lsb
    <> "]"
genLVal (RegConcat e) = "{" <> comma (genExpr <$> e) <> "}"

genConstExpr :: ConstExpr -> Text
genConstExpr (ConstExpr num) = showT num

genPortType :: PortType -> Text
genPortType Wire = "wire"
genPortType (Reg signed) | signed    = "reg signed"
                         | otherwise = "reg"

genAssign :: Text -> Assign -> Text
genAssign op (Assign r d e) =
  genLVal r <> op <> maybe "" genDelay d <> genExpr e

genStmnt :: Stmnt -> Text
genStmnt (TimeCtrl  d stat   ) = genDelay d <> " " <> defMap stat
genStmnt (EventCtrl e stat   ) = genEvent e <> " " <> defMap stat
genStmnt (SeqBlock       s   ) = "begin\n" <> fold (genStmnt <$> s) <> "end\n"
genStmnt (BlockAssign    a   ) = genAssign " = " a <> ";\n"
genStmnt (NonBlockAssign a   ) = genAssign " <= " a <> ";\n"
genStmnt (StatCA         a   ) = genContAssign a
genStmnt (TaskEnable     task) = genTask task <> ";\n"
genStmnt (SysTaskEnable  task) = "$" <> genTask task <> ";\n"

genTask :: Task -> Text
genTask (Task name expr)
  | null expr = i
  | otherwise = i <> "(" <> comma (genExpr <$> expr) <> ")"
  where i = name ^. getIdentifier

-- | Render the 'Text' to 'IO'. This is equivalent to 'putStrLn'.
render :: (Source a) => a -> IO ()
render = T.putStrLn . genSource

-- Instances

instance Source Identifier where
  genSource = view getIdentifier

instance Source Task where
  genSource = genTask

instance Source Stmnt where
  genSource = genStmnt

instance Source PortType where
  genSource = genPortType

instance Source ConstExpr where
  genSource = genConstExpr

instance Source LVal where
  genSource = genLVal

instance Source Delay where
  genSource = genDelay

instance Source Event where
  genSource = genEvent

instance Source UnaryOperator where
  genSource = genUnaryOperator

instance Source Expr where
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
