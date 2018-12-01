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
  <> "(\n" <> ports <> "\n);"
  <> modItems
  <> "endomodule\n"
  where
    ports = sep ",\n" $ genPort <$> mod ^. modPorts
    modItems = fromList $ genModuleItem <$> mod ^. moduleItems

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
genExpr (PrimExpr prim) =
  genPrimary prim
genExpr _ = "TODO"

genPrimary :: Primary -> Text
genPrimary (PrimNum num) =
  sh (num ^. numSize) <> "'d" <> sh (num ^. numVal)
  where
    sh = T.pack . show
genPrimary (PrimId ident) = ident ^. getIdentifier

genBinaryOperator :: BinaryOperator -> Text
genBinaryOperator BinAnd = " & "
genBinaryOperator BinOr  = " | "
genBinaryOperator BinXor = " ^ "

render :: Text -> IO ()
render = T.putStrLn
