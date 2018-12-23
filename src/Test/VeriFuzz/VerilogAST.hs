{-|
Module      : Test.VeriFuzz.VerilogAST
Description : Definition of the Verilog AST types.
Copyright   : (c) Yann Herklotz Grave 2018
License     : GPL-3
Maintainer  : ymherklotz@gmail.com
Stability   : experimental
Portability : POSIX

Defines the types to build a Verilog AST.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.VeriFuzz.VerilogAST where

import           Control.Lens
import qualified Data.Graph.Inductive       as G
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Test.QuickCheck            as QC
import           Test.VeriFuzz.Circuit
import           Test.VeriFuzz.Graph.Random

-- | Identifier in Verilog. This is just a string of characters that can either
-- be lowercase and uppercase for now. This might change in the future though,
-- as Verilog supports many more characters in Identifiers.
newtype Identifier = Identifier { _getIdentifier :: Text }
                   deriving (Show, Eq, Ord)

-- | A number in Verilog which contains a size and a value.
data Number = Number { _numSize :: Int
                     , _numVal  :: Int
                     } deriving (Show, Eq, Ord)

-- | Binary operators that are currently supported in the verilog generation.
data BinaryOperator = BinAnd -- ^ Binary And (&).
                    | BinOr  -- ^ Binary Or (|).
                    | BinXor -- ^ Binary Xor (^).
                    deriving (Show, Eq, Ord)

-- | Unary operators that are currently supported by the generator.
data UnaryOperator = UnNot   -- ^ Not (!).
                   | UnMinus -- ^ Minus (-).
                   deriving (Show, Eq, Ord)

-- | A primary expression which can either be a number or an identifier.
data Primary = PrimNum Number    -- ^ Number in primary expression.
             | PrimId Identifier -- ^ Identifier in primary expression.
             deriving (Show, Eq, Ord)

-- | Verilog expression, which can either be a primary expression, unary
-- expression, binary operator expression or a conditional expression.
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
                deriving (Show, Eq, Ord)

-- | Continuous assignment which can be in the body of a statement.
data ContAssign = ContAssign { _contAssignNetLVal :: Identifier
                             , _contAssignExpr    :: Expression
                             } deriving (Show, Eq, Ord)

-- | Different port direction that are supported in Verilog.
data PortDir = Input  -- ^ Input direction for port (@input@).
             | Output -- ^ Output direction for port (@output@).
             | InOut  -- ^ Inout direction for port (@inout@).
             deriving (Show, Eq, Ord)

-- | Port declaration.
data Port = Port { _portDir  :: PortDir
                 , _portName :: Identifier
                 } deriving (Show, Eq, Ord)

-- | Module item which is the body of the module expression.
newtype ModuleItem = Assign ContAssign
                   deriving (Show, Eq, Ord)

-- | 'module' module_identifier [list_of_ports] ';' { module_item } 'end_module'
data ModuleDecl = ModuleDecl { _moduleId    :: Identifier
                             , _modPorts    :: [Port]
                             , _moduleItems :: [ModuleItem]
                             } deriving (Show, Eq, Ord)

-- | Description of the Verilog module.
newtype Description = Description { _getDescription :: ModuleDecl }
                    deriving (Show, Eq, Ord)

-- | The complete sourcetext for the Verilog module.
newtype SourceText = SourceText { _getSourceText :: [Description] }
                   deriving (Show, Eq, Ord)

-- Generate Arbitrary instances for the AST

instance QC.Arbitrary Identifier where
  arbitrary = Identifier . T.pack <$>
    (QC.shuffle (['a'..'z'] <> ['A'..'Z']) >>= QC.sublistOf)

instance QC.Arbitrary Number where
  arbitrary = Number <$> QC.suchThat QC.arbitrary (>0) <*> QC.arbitrary

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
                           , (2, PrimExpr <$> QC.arbitrary)
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

-- Traversal Instance

traverseExpr :: Traversal' Expression Expression
traverseExpr _ (PrimExpr e)      = pure (PrimExpr e)
traverseExpr _ (UnPrimExpr un e) = pure (UnPrimExpr un e)
traverseExpr f (OpExpr l op r)   = OpExpr <$> f l <*> pure op <*> f r
traverseExpr f (CondExpr c l r)  = CondExpr <$> f c <*> f l <*> f r

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

makePrisms ''Expression
makePrisms ''ModuleItem

-- Helper functions for the AST

-- | Create a number expression which will be stored in a primary expression.
numExpr :: Int -> Int -> Expression
numExpr = ((PrimExpr . PrimNum) .) . Number

-- | Create an empty module.
emptyMod :: ModuleDecl
emptyMod = ModuleDecl (Identifier "") [] []

-- | Set a module name for a module declaration.
setModName :: Text -> ModuleDecl -> ModuleDecl
setModName str = moduleId .~ Identifier str

-- | Add a port to the module declaration.
addModPort :: Port -> ModuleDecl -> ModuleDecl
addModPort port = modPorts %~ (:) port
