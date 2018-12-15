{-|
Module      : Test.VeriFuzz.CodeGen
Description : Code generation for Verilog AST.
Copyright   : (c) Yann Herklotz Grave 2018
License     : GPL-3
Maintainer  : ymherklotz@gmail.com
Stability   : experimental
Portability : POSIX

This module generates the code from the Verilog AST defined in
"Test.VeriFuzz.VerilogAST".
-}

{-# LANGUAGE OverloadedStrings #-}

module Test.VeriFuzz.CodeGen where

import           Control.Lens
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Test.VeriFuzz.Internal.Shared
import           Test.VeriFuzz.VerilogAST

-- | Convert the 'SourceText' type to 'Text' so that it can be rendered.
genSourceText :: SourceText -> Text
genSourceText source =
  fromList $ genDescription <$> source ^. getSourceText

-- | Generate the 'Description' to 'Text'.
genDescription :: Description -> Text
genDescription desc =
  genModuleDecl $ desc ^. getDescription

-- | Generate the 'ModuleDecl' for a module and convert it to 'Text'.
genModuleDecl :: ModuleDecl -> Text
genModuleDecl mod =
  "module " <> mod ^. moduleId . getIdentifier
  <> "(\n" <> ports <> "\n);\n"
  <> modItems
  <> "endmodule\n"
  where
    ports = sep ",\n" $ genPort <$> mod ^. modPorts
    modItems = fromList $ genModuleItem <$> mod ^. moduleItems

-- | Generate the 'Port' description.
genPort :: Port -> Text
genPort port =
  "  " <> dir <> " " <> name
  where
    dir = genPortDir $ port ^. portDir
    name = port ^. portName . getIdentifier

-- | Convert the 'PortDir' type to 'Text'.
genPortDir :: PortDir -> Text
genPortDir Input  = "input"
genPortDir Output = "output"
genPortDir InOut  = "inout"

-- | Generate a 'ModuleItem'.
genModuleItem :: ModuleItem -> Text
genModuleItem (Assign assign) = genContAssign assign

-- | Generate the 'ContinuousAssignment' to 'Text'.
genContAssign :: ContAssign -> Text
genContAssign assign =
  "  assign " <> name <> " = " <> expr <> ";\n"
  where
    name = assign ^. contAssignNetLVal . getIdentifier
    expr = genExpr $ assign ^. contAssignExpr

-- | Generate 'Expression' to 'Text'.
genExpr :: Expression -> Text
genExpr (OpExpr exprRhs bin exprLhs) =
  genExpr exprRhs <> genBinaryOperator bin <> genExpr exprLhs
genExpr (PrimExpr prim) =
  genPrimary prim
genExpr _ = "TODO"

-- | Generate a 'PrimaryExpression' to 'Text'.
genPrimary :: Primary -> Text
genPrimary (PrimNum num) =
  neg <> sh (num ^. numSize) <> "'d" <> (sh . abs) n
  where
    sh = T.pack . show
    abs x = if x <= 0 then -x else x
    n = num ^. numVal
    neg = if n <= 0 then "-" else ""
genPrimary (PrimId ident) = ident ^. getIdentifier

-- | Convert 'BinaryOperator' to 'Text'.
genBinaryOperator :: BinaryOperator -> Text
genBinaryOperator BinAnd = " & "
genBinaryOperator BinOr  = " | "
genBinaryOperator BinXor = " ^ "

-- | Render the 'Text' to 'IO'. This is equivalent to 'putStrLn'.
render :: Text -> IO ()
render = T.putStrLn
