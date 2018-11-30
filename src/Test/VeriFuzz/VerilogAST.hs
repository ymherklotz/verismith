{-# LANGUAGE TemplateHaskell #-}

module Test.VeriFuzz.VerilogAST where

import           Control.Lens
import           Data.Text    (Text)

newtype NetLVal = NetLVal { _getNetLVal :: Text }
                deriving (Show)
makeLenses ''NetLVal

newtype Identifier = Identifier { _getIdentifier :: Text }
                   deriving (Show)
makeLenses ''Identifier

data Number = Number { _numSize :: Int
                     , _numVal  :: Int
                     } deriving (Show)
makeLenses ''Number

data BinaryOperator = BinAnd
                    | BinOr
                    | BinXor
                    deriving (Show)
makeLenses ''BinaryOperator

data UnaryOperator = UnNot
                   | UnMinus
                   deriving (Show)
makeLenses ''UnaryOperator

data Primary = PrimNum Number
             | PrimId Identifier
             deriving (Show)
makeLenses ''Primary

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
makeLenses ''Expression

data ContAssign = ContAssign { _contAssignNetLVal :: NetLVal
                             , _contAssignExpr    :: Expression
                             } deriving (Show)
makeLenses ''ContAssign

data PortDir = Input
             | Output
             | InOut
             deriving (Show)
makeLenses ''PortDir

data Port = Port { _portName :: Identifier
                 , _portDir  :: PortDir
                 } deriving (Show)
makeLenses ''Port

newtype ModuleItem = ModuleItem { _getModuleItem :: Text }
                   deriving (Show)
makeLenses ''ModuleItem

-- | 'module' module_identifier [list_of_ports] ';' { module_item } 'end_module'
data ModuleDecl = ModuleDecl { _moduleId   :: Identifier
                             , _modPorts   :: [Port]
                             , _moduleItem :: ModuleItem
                             } deriving (Show)
makeLenses ''ModuleDecl

newtype Description = Description { _getDescription :: ModuleDecl }
                    deriving (Show)
makeLenses ''Description

newtype SourceText = SourceText { _getSourceText :: [Description] }
                   deriving (Show
                            )
makeLenses ''SourceText
