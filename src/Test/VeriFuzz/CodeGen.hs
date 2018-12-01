{-# LANGUAGE OverloadedStrings #-}

module Test.VeriFuzz.CodeGen where

import           Control.Lens
import           Data.Text                     (Text)
import qualified Data.Text                     as T
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
