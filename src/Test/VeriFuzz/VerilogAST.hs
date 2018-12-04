{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.VeriFuzz.VerilogAST where

import           Control.Lens
import           Data.Text       as T
import           Data.Text       (Text)
import           Test.QuickCheck as QC

newtype Identifier = Identifier { _getIdentifier :: Text }
                   deriving (Show)

data Number = Number { _numSize :: Int
                     , _numVal  :: Int
                     } deriving (Show)

data BinaryOperator = BinAnd
                    | BinOr
                    | BinXor
                    deriving (Show)

data UnaryOperator = UnNot
                   | UnMinus
                   deriving (Show)

data Primary = PrimNum Number
             | PrimId Identifier
             deriving (Show)

data Expression = PrimExpr Primary
                | UnPrimExpr { _exprUnOp :: UnaryOperator
                             , _exprPrim :: Primary
                             }
                | OpExpr { _exprLhs   :: Expression
                         , _exprBinOp :: BinaryOperator
                         , _exprRhs   :: Expression
                         }
                | CondExpr { _exprCond  :: Expression
                           , _exprTrue  :: Expression
                           , _exprFalse :: Expression
                           }
                deriving (Show)

data ContAssign = ContAssign { _contAssignNetLVal :: Identifier
                             , _contAssignExpr    :: Expression
                             } deriving (Show)

data PortDir = Input
             | Output
             | InOut
             deriving (Show)

data Port = Port { _portName :: Identifier
                 , _portDir  :: PortDir
                 } deriving (Show)

newtype ModuleItem = Assign ContAssign
                   deriving (Show)

-- | 'module' module_identifier [list_of_ports] ';' { module_item } 'end_module'
data ModuleDecl = ModuleDecl { _moduleId    :: Identifier
                             , _modPorts    :: [Port]
                             , _moduleItems :: [ModuleItem]
                             } deriving (Show)

newtype Description = Description { _getDescription :: ModuleDecl }
                    deriving (Show)

newtype SourceText = SourceText { _getSourceText :: [Description] }
                   deriving (Show)

-- Generate Arbitrary instances for the AST

instance QC.Arbitrary Identifier where
  arbitrary = Identifier . T.pack <$>
    (QC.shuffle (['a'..'z'] <> ['A'..'Z']) >>= QC.sublistOf)

instance QC.Arbitrary Number where
  arbitrary = Number <$> (suchThat QC.arbitrary (>=0)) <*> QC.arbitrary

instance QC.Arbitrary BinaryOperator where
  arbitrary = QC.elements [BinAnd, BinOr, BinXor]

instance QC.Arbitrary UnaryOperator where
  arbitrary = QC.elements [UnNot, UnMinus]

instance QC.Arbitrary Primary where
  arbitrary = PrimNum <$> QC.arbitrary

instance QC.Arbitrary PortDir where
  arbitrary = QC.elements [Input, Output, InOut]

instance QC.Arbitrary Port where
  arbitrary = Port <$> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary Expression where
  arbitrary = QC.frequency [ (1, OpExpr <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary)
                           , (2, PrimExpr <$> arbitrary)
                           ]

instance QC.Arbitrary ContAssign where
  arbitrary = ContAssign <$> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary ModuleItem where
  arbitrary = Assign <$> QC.arbitrary

instance QC.Arbitrary ModuleDecl where
  arbitrary = ModuleDecl <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary Description where
  arbitrary = Description <$> QC.arbitrary

instance QC.Arbitrary SourceText where
  arbitrary = SourceText <$> QC.arbitrary

-- Create all the necessary lenses

makeLenses ''Identifier
makeLenses ''Number
makeLenses ''SourceText
makeLenses ''Description
makeLenses ''ModuleDecl
makeLenses ''ModuleItem
makeLenses ''Port
makeLenses ''PortDir
makeLenses ''BinaryOperator
makeLenses ''UnaryOperator
makeLenses ''Primary
makeLenses ''Expression
makeLenses ''ContAssign

-- Helper functions for the AST

numExpr :: Int -> Int -> Expression
numExpr = ((PrimExpr . PrimNum) .) . Number

emptyMod :: ModuleDecl
emptyMod = ModuleDecl (Identifier "") [] []

setModName :: Text -> ModuleDecl -> ModuleDecl
setModName str = moduleId .~ Identifier str

addModPort :: Port -> ModuleDecl -> ModuleDecl
addModPort port = modPorts %~ (:) port
