{-# LANGUAGE TemplateHaskell #-}

module Test.VeriFuzz.VerilogAST where

import           Control.Lens
import           Data.Text    (Text)
import qualified Data.Text    as T

type NetLVal = Text

type Identifier = Text

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

data ContAssign = ContAssign { _contAssignNetLVal :: NetLVal
                             , _contAssignExpr    :: Expression
                             } deriving (Show)

data PortDir = Input
             | Output
             | InOut
             deriving (Show)

data Port = Port { _portName :: Identifier
                 , _portDir  :: PortDir
                 } deriving (Show)

type ModuleItem = Text

-- | 'module' module_identifier [list_of_ports] ';' { module_item } 'end_module'
data ModuleDecl = ModuleDecl { _moduleId   :: Identifier
                             , _modPorts   :: [Port]
                             , _moduleItem :: ModuleItem
                             } deriving (Show)

type Description = ModuleDecl

type SourceText = [Description]

makeLenses ''Number
makeLenses ''Expression
makeLenses ''ContAssign
makeLenses ''Port
makeLenses ''ModuleDecl
