{-|
Module      : Test.VeriFuzz.Verilog.AST
Description : Definition of the Verilog AST types.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Poratbility : POSIX

Defines the types to build a Verilog AST.
-}

{-# LANGUAGE TemplateHaskell #-}

module Test.VeriFuzz.Verilog.AST where

import           Control.Lens
import qualified Data.Graph.Inductive       as G
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Test.QuickCheck            as QC
import           Test.VeriFuzz.Circuit
import           Test.VeriFuzz.Graph.Random

class Source a where
  genSource :: a -> Text

-- | Identifier in Verilog. This is just a string of characters that can either
-- be lowercase and uppercase for now. This might change in the future though,
-- as Verilog supports many more characters in Identifiers.
newtype Identifier = Identifier { _getIdentifier :: Text }
                   deriving (Show, Eq, Ord)

-- | A number in Verilog which contains a size and a value.
data Number = Number { _numSize :: Int
                     , _numVal  :: Int
                     } deriving (Show, Eq, Ord)

newtype Delay = Delay { _delay :: Int }
                deriving (Show, Eq, Ord)

data Event = EId Identifier
           | EExpr Expression
           | EAll
           deriving (Show, Eq, Ord)

data Net = Wire
         | Tri
         | Tri1
         | Supply0
         | Wand
         | TriAnd
         | Tri0
         | Supply1
         | Wor
         | Trior
         deriving (Show, Eq, Ord)

data RegLVal = RegId Identifier
             | RegExpr { _regExprId :: Identifier
                       , _regExpr   :: Expression
                       }
             | RegSize { _regSizeId  :: Identifier
                       , _regSizeMSB :: ConstExpr
                       , _regSizeLSB :: ConstExpr
                       }
             deriving (Show, Eq, Ord)

-- | Binary operators that are currently supported in the verilog generation.
data BinaryOperator = BinPlus    -- ^ @+@
                    | BinMinus   -- ^ @-@
                    | BinTimes   -- ^ @*@
                    | BinDiv     -- ^ @/@
                    | BinMod     -- ^ @%@
                    | BinEq      -- ^ @==@
                    | BinNEq     -- ^ @!=@
                    | BinCEq     -- ^ @===@
                    | BinCNEq    -- ^ @!==@
                    | BinLAnd    -- ^ @&&@
                    | BinLOr     -- ^ @||@
                    | BinLT      -- ^ @<@
                    | BinLEq     -- ^ @<=@
                    | BinGT      -- ^ @>@
                    | BinGEq     -- ^ @>=@
                    | BinAnd     -- ^ @&@
                    | BinOr      -- ^ @|@
                    | BinXor     -- ^ @^@
                    | BinXNor    -- ^ @^~@
                    | BinXNorInv -- ^ @~^@
                    | BinPower   -- ^ @**@
                    | BinLSL     -- ^ @<<@
                    | BinLSR     -- ^ @>>@
                    | BinASL     -- ^ @<<<@
                    | BinASR     -- ^ @>>>@
                    deriving (Show, Eq, Ord)

-- | Unary operators that are currently supported by the generator.
data UnaryOperator = UnPlus    -- ^ @+@
                   | UnMinus   -- ^ @-@
                   | UnNot     -- ^ @!@
                   | UnAnd     -- ^ @&@
                   | UnNand    -- ^ @~&@
                   | UnOr      -- ^ @|@
                   | UnNor     -- ^ @~|@
                   | UnXor     -- ^ @^@
                   | UnNxor    -- ^ @~^@
                   | UnNxorInv -- ^ @^~@
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
                | ExprStr Text
                deriving (Show, Eq, Ord)

newtype ConstExpr = ConstExpr { _constNum :: Int }
                  deriving (Show, Eq, Ord)

-- | Different port direction that are supported in Verilog.
data PortDir = PortIn    -- ^ Input direction for port (@input@).
             | PortOut   -- ^ Output direction for port (@output@).
             | PortInOut -- ^ Inout direction for port (@inout@).
             deriving (Show, Eq, Ord)

data PortType = PortNet Net
              | Reg { _regSigned :: Bool }
              deriving (Show, Eq, Ord)

-- | Port declaration.
data Port = Port { _portType :: PortType
                 , _portName :: Identifier
                 } deriving (Show, Eq, Ord)

newtype ModConn = ModConn { _modConn :: Expression }
                deriving (Show, Eq, Ord)

data Assign = Assign { _assignReg   :: RegLVal
                     , _assignDelay :: Maybe Delay
                     , _assignExpr  :: Expression
                     } deriving (Show, Eq, Ord)

data ContAssign = ContAssign { _contAssignNetLVal :: Identifier
                             , _contAssignExpr    :: Expression
                             } deriving (Show, Eq, Ord)

-- | Statements in Verilog.
data Statement = TimeCtrl { _statDelay :: Delay
                          , _statDStat :: Maybe Statement
                          }                              -- ^ Time control (@#NUM@)
               | EventCtrl { _statEvent :: Event
                           , _statEStat :: Maybe Statement
                           }
               | SeqBlock { _statements :: [Statement] } -- ^ Sequential block (@begin ... end@)
               | BlockAssign Assign                      -- ^ blocking assignment (@=@)
               | NonBlockAssign Assign                   -- ^ Non blocking assignment (@<=@)
               | StatCA ContAssign                       -- ^ Statement continuous assignment. May not be correct.
               | TaskEnable Task
               | SysTaskEnable Task
               deriving (Show, Eq, Ord)

data Task = Task { _taskName :: Identifier
                 , _taskExpr :: [Expression]
                 } deriving (Show, Eq, Ord)

-- | Module item which is the body of the module expression.
data ModItem = ModCA ContAssign
             | ModInst { _modInstId    :: Identifier
                       , _modInstName  :: Identifier
                       , _modInstConns :: [ModConn]
                       }
             | Initial Statement
             | Always Statement
             | Decl Port
             deriving (Show, Eq, Ord)

-- | 'module' module_identifier [list_of_ports] ';' { module_item } 'end_module'
data ModDecl = ModDecl { _moduleId    :: Identifier
                       , _modOutPort  :: Maybe Port
                       , _modInPorts  :: [Port]
                       , _moduleItems :: [ModItem]
                       } deriving (Show, Eq, Ord)

-- | Description of the Verilog module.
newtype Description = Description { _getDescription :: ModDecl }
                    deriving (Show, Eq, Ord)

-- | The complete sourcetext for the Verilog module.
newtype VerilogSrc = VerilogSrc { _getVerilogSrc :: [Description] }
                   deriving (Show, Eq, Ord)

-- Generate Arbitrary instances for the AST

expr :: Int -> QC.Gen Expression
expr 0 = QC.oneof
  [ PrimExpr <$> QC.arbitrary
  , UnPrimExpr <$> QC.arbitrary <*> QC.arbitrary
  -- , ExprStr <$> QC.arbitrary
  ]
expr n
  | n > 0 = QC.oneof
    [ PrimExpr <$> QC.arbitrary
    , UnPrimExpr <$> QC.arbitrary <*> QC.arbitrary
    -- , ExprStr <$> QC.arbitrary
    , OpExpr <$> subexpr 2 <*> QC.arbitrary <*> subexpr 2
    , CondExpr <$> subexpr 3 <*> subexpr 3 <*> subexpr 3
    ]
  | otherwise = expr 0
  where
    subexpr y = expr (n `div` y)

statement :: Int -> QC.Gen Statement
statement 0 = QC.oneof
  [ BlockAssign <$> QC.arbitrary
  , NonBlockAssign <$> QC.arbitrary
  -- , StatCA <$> QC.arbitrary
  , TaskEnable <$> QC.arbitrary
  , SysTaskEnable <$> QC.arbitrary
  ]
statement n
  | n > 0 = QC.oneof
    [ TimeCtrl <$> QC.arbitrary <*> (Just <$> substat 2)
    , SeqBlock <$> QC.listOf1 (substat 4)
    , BlockAssign <$> QC.arbitrary
    , NonBlockAssign <$> QC.arbitrary
    -- , StatCA <$> QC.arbitrary
    , TaskEnable <$> QC.arbitrary
    , SysTaskEnable <$> QC.arbitrary
    ]
  | otherwise = statement 0
  where
    substat y = statement (n `div` y)

modPortGen :: QC.Gen Port
modPortGen = QC.oneof
  [ Port (PortNet Wire) <$> QC.arbitrary
  , Port <$> (Reg <$> QC.arbitrary) <*> QC.arbitrary
  ]

instance QC.Arbitrary Text where
  arbitrary = T.pack <$> QC.arbitrary

instance QC.Arbitrary Identifier where
  arbitrary = Identifier . T.pack <$>
    (QC.shuffle (['a'..'z'] <> ['A'..'Z']) >>= QC.sublistOf)

instance QC.Arbitrary Number where
  arbitrary = Number <$> QC.suchThat QC.arbitrary (>0) <*> QC.arbitrary

instance QC.Arbitrary Net where
  arbitrary = pure Wire

instance QC.Arbitrary BinaryOperator where
  arbitrary = QC.elements
    [ BinPlus
    , BinMinus
    , BinTimes
    , BinDiv
    , BinMod
    , BinEq
    , BinNEq
    , BinCEq
    , BinCNEq
    , BinLAnd
    , BinLOr
    , BinLT
    , BinLEq
    , BinGT
    , BinGEq
    , BinAnd
    , BinOr
    , BinXor
    , BinXNor
    , BinXNorInv
    , BinPower
    , BinLSL
    , BinLSR
    , BinASL
    , BinASR
    ]

instance QC.Arbitrary UnaryOperator where
  arbitrary = QC.elements
    [ UnPlus
    , UnMinus
    , UnNot
    , UnAnd
    , UnNand
    , UnOr
    , UnNor
    , UnXor
    , UnNxor
    , UnNxorInv
    ]

instance QC.Arbitrary Primary where
  arbitrary = PrimNum <$> QC.arbitrary

instance QC.Arbitrary PortDir where
  arbitrary = QC.elements [PortIn, PortOut, PortInOut]

instance QC.Arbitrary PortType where
  arbitrary = QC.oneof [PortNet <$> QC.arbitrary, Reg <$> QC.arbitrary]

instance QC.Arbitrary Port where
  arbitrary = Port <$> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary Delay where
  arbitrary = Delay <$> QC.suchThat QC.arbitrary (\x -> x > 0)

instance QC.Arbitrary Event where
  arbitrary = EId <$> QC.arbitrary

instance QC.Arbitrary ModConn where
  arbitrary = ModConn <$> QC.arbitrary

instance QC.Arbitrary ConstExpr where
  arbitrary = ConstExpr <$> QC.suchThat QC.arbitrary (\x -> x > 0)

instance QC.Arbitrary RegLVal where
  arbitrary = QC.oneof [ RegId <$> QC.arbitrary
                       , RegExpr <$> QC.arbitrary <*> QC.arbitrary
                       , RegSize <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
                       ]

instance QC.Arbitrary Assign where
  arbitrary = Assign <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary Expression where
  arbitrary = QC.sized expr

instance QC.Arbitrary Statement where
  arbitrary = QC.sized statement

instance QC.Arbitrary ContAssign where
  arbitrary = ContAssign <$> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary Task where
  arbitrary = Task <$> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary ModItem where
  arbitrary = QC.oneof [ ModCA <$> QC.arbitrary
                       , ModInst <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
                       , Initial <$> QC.arbitrary
                       , Always <$> (EventCtrl <$> QC.arbitrary <*> QC.arbitrary)
                       , Decl <$> QC.arbitrary
                       ]

instance QC.Arbitrary ModDecl where
  arbitrary = ModDecl <$> QC.arbitrary <*> QC.arbitrary
              <*> QC.listOf1 modPortGen <*> QC.arbitrary

instance QC.Arbitrary Description where
  arbitrary = Description <$> QC.arbitrary

instance QC.Arbitrary VerilogSrc where
  arbitrary = VerilogSrc <$> QC.arbitrary

-- Other Instances

instance IsString Identifier where
  fromString = Identifier . T.pack

-- Traversal Instance

traverseExpr :: Traversal' Expression Expression
traverseExpr _ (PrimExpr e)      = pure (PrimExpr e)
traverseExpr _ (UnPrimExpr un e) = pure (UnPrimExpr un e)
traverseExpr f (OpExpr l op r)   = OpExpr <$> f l <*> pure op <*> f r
traverseExpr f (CondExpr c l r)  = CondExpr <$> f c <*> f l <*> f r

-- Create all the necessary lenses

makeLenses ''Identifier
makeLenses ''Number
makeLenses ''VerilogSrc
makeLenses ''Description
makeLenses ''ModDecl
makeLenses ''ModItem
makeLenses ''Port
makeLenses ''PortDir
makeLenses ''BinaryOperator
makeLenses ''UnaryOperator
makeLenses ''Primary
makeLenses ''Expression
makeLenses ''ContAssign
makeLenses ''PortType

-- Make all the necessary prisms

makePrisms ''Expression
makePrisms ''ModItem
makePrisms ''ModConn
