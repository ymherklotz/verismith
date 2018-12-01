{-# LANGUAGE OverloadedStrings #-}

module Test.VeriFuzz.CodeGen where

import           Control.Lens
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Test.VeriFuzz.Internal.Shared
import           Test.VeriFuzz.VerilogAST

genSourceText :: SourceText -> Text
genSourceText source =
  fromList $ genDescription <$> source ^. getSourceText

genDescription :: Description -> Text
genDescription desc =
  genModuleDecl $ desc ^. getDescription

genModuleDecl :: ModuleDecl -> Text
genModuleDecl mod =
  "module " <> mod ^. moduleId . getIdentifier
  <> "(\n" <> ports <> "\n);\nendomodule"
  where
    ports = sep ",\n" $ genPort <$> mod ^. modPorts

genPort :: Port -> Text
genPort port =
  "  " <> dir <> " " <> name
  where
    dir = genPortDir $ port ^. portDir
    name = port ^. portName . getIdentifier

genPortDir :: PortDir -> Text
genPortDir Input  = "input"
genPortDir Output = "output"
genPortDir InOut  = "inout"

genModuleItem :: ModuleItem -> Text
genModuleItem (Assign assign) = genContAssign assign

genContAssign :: ContAssign -> Text
genContAssign assign =
  "  assign " <> name <> " = " <> expr <> ";\n"
  where
    name = assign ^. contAssignNetLVal . getIdentifier
    expr = genExpr $ assign ^. contAssignExpr

genExpr :: Expression -> Text
genExpr (OpExpr exprRhs bin exprLhs) =
  genExpr exprRhs <> genBinaryOperator bin <> genExpr exprLhs
genExpr _ = "TODO"

genBinaryOperator :: BinaryOperator -> Text
genBinaryOperator BinAnd = " & "
genBinaryOperator BinOr  = " | "
genBinaryOperator BinXor = " ^ "

render :: Text -> IO ()
render = T.putStrLn
