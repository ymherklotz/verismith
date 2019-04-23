{-|
Module      : VeriFuzz.Config
Description : Configuration file format and parser.
Copyright   : (c) 2019, Yann Herklotz
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

TOML Configuration file format and parser.
-}

{-# LANGUAGE TemplateHaskell #-}

module VeriFuzz.Config
    ( -- * TOML Configuration
      -- $conf
      Config(..)
    , defaultConfig
    -- ** Probabilities
    , Probability(..)
    -- *** Expression
    , ProbExpr(..)
    -- *** Event List
    , ProbEventList(..)
    -- *** Module Item
    , ProbModItem(..)
    -- *** Statement
    , ProbStatement(..)
    -- ** Property
    , Property(..)
    -- ** Simulator Description
    , SimDescription(..)
    -- ** Synthesiser Description
    , SynthDescription(..)
    -- * Useful Lenses
    , configProbability
    , configProperty
    , configSimulators
    , configSynthesisers
    , probModItem
    , probStmnt
    , probExpr
    , probEventList
    , probExprNum
    , probExprId
    , probExprUnOp
    , probExprBinOp
    , probExprCond
    , probExprConcat
    , probExprStr
    , probExprSigned
    , probExprUnsigned
    , probEventListAll
    , probEventListVar
    , probEventListClk
    , probModItemAssign
    , probModItemAlways
    , probModItemInst
    , probStmntBlock
    , probStmntNonBlock
    , probStmntCond
    , probStmntFor
    , propSize
    , propSeed
    , propStmntDepth
    , propModDepth
    , propMaxModules
    , parseConfigFile
    , parseConfig
    , encodeConfig
    , encodeConfigFile
    )
where

import           Control.Applicative (Alternative, (<|>))
import           Control.Lens        hiding ((.=))
import           Data.List.NonEmpty  (NonEmpty (..))
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text.IO        as T
import           Toml                (TomlCodec, (.=))
import qualified Toml

-- $conf
--
-- VeriFuzz supports a TOML configuration file that can be passed using the @-c@
-- flag or using the 'parseConfig' and 'encodeConfig' functions. The
-- configuration can then be manipulated using the lenses that are also provided
-- in this module.
--
-- The configuration file can be used to tweak the random Verilog generation by
-- passing different probabilities to each of the syntax nodes in the AST. It
-- can also be used to specify which simulators to fuzz with which options. A
-- seed for the run can also be set, to replay a previous run using the same
-- exact generation. A default value is associated with each key in the
-- configuration file, which means that only the options that need overriding
-- can be added to the configuration. The defaults can be observed in
-- 'defaultConfig' or when running @verifuzz config@.
--
-- == Configuration Sections
--
-- There are four main configuration sections in the TOML file:
--
--   [@probability@] The @probability@ section defines the probabilities at
--   every node in the AST.
--
--   [@property@] Controls different properties of the generation, such as
--   adding a seed or the depth of the statements.
--
--   [@simulator@] This is an array of tables containing descriptions of the
--   different simulators that should be used. It currently only supports
--   <http://iverilog.icarus.com/ Icarus Verilog>.
--
--   [@synthesiser@] This is also an array of tables containing descriptions of
--   the different synthesisers that should be used. The synthesisers that are
--   currently supported are:
--
--     - <https://www.intel.com/content/www/us/en/programmable/downloads/download-center.html Quartus>
--     - <https://www.xilinx.com/products/design-tools/ise-design-suite.html ISE Design Suite>
--     - <https://www.xilinx.com/products/design-tools/ise-design-suite.html Vivado Design Suite>
--     - <http://www.clifford.at/yosys/ Yosys Open SYnthesis Suite>
--
-- === Default Configuration
--
-- >>> T.putStrLn $ encodeConfig defaultConfig
-- <BLANKLINE>
-- [probability]
--   eventlist.all = 1
--   eventlist.clk = 1
--   eventlist.var = 1
--   expr.binary = 1
--   expr.concatenation = 1
--   expr.number = 1
--   expr.signed = 1
--   expr.string = 0
--   expr.ternary = 1
--   expr.unary = 1
--   expr.unsigned = 1
--   expr.variable = 1
--   moditem.always = 1
--   moditem.assign = 5
--   moditem.instantiation = 1
--   statement.blocking = 0
--   statement.conditional = 1
--   statement.forloop = 1
--   statement.nonblocking = 15
-- <BLANKLINE>
-- [property]
--   module.depth = 2
--   module.max = 5
--   size = 20
--   statement.depth = 3
-- <BLANKLINE>
-- [[synthesiser]]
--   name = "yosys"
-- <BLANKLINE>
-- [[synthesiser]]
--   name = "vivado"
-- <BLANKLINE>

-- | Probability of different expressions nodes.
data ProbExpr = ProbExpr { _probExprNum      :: {-# UNPACK #-} !Int
                         -- ^ Probability of generation a number like
                         -- @4'ha@. This should never be set to 0, as it is used
                         -- as a fallback in case there are no viable
                         -- identifiers, such as none being in scope.
                         , _probExprId       :: {-# UNPACK #-} !Int
                         -- ^ Probability of generating an identifier that is in
                         -- scope and of the right type.
                         , _probExprUnOp     :: {-# UNPACK #-} !Int
                         -- ^ Probability of generating a unary operator.
                         , _probExprBinOp    :: {-# UNPACK #-} !Int
                         -- ^ Probability of generation a binary operator.
                         , _probExprCond     :: {-# UNPACK #-} !Int
                         -- ^ probability of generating a conditional ternary
                         -- operator.
                         , _probExprConcat   :: {-# UNPACK #-} !Int
                         -- ^ Probability of generating a concatenation.
                         , _probExprStr      :: {-# UNPACK #-} !Int
                         -- ^ Probability of generating a string. This is not
                         -- fully supported therefore currently cannot be set.
                         , _probExprSigned   :: {-# UNPACK #-} !Int
                         -- ^ Probability of generating a signed function
                         -- @$signed(...)@.
                         , _probExprUnsigned :: {-# UNPACK #-} !Int
                         -- ^ Probability of generating an unsigned function
                         -- @$unsigned(...)@.
                         }
              deriving (Eq, Show)

-- | Probability of generating different nodes inside a module declaration.
data ProbModItem = ProbModItem { _probModItemAssign :: {-# UNPACK #-} !Int
                               -- ^ Probability of generating an @assign@.
                               , _probModItemAlways :: {-# UNPACK #-} !Int
                               -- ^ Probability of generating an @always@ block.
                               , _probModItemInst   :: {-# UNPACK #-} !Int
                               -- ^ Probability of generating a module
                               -- instantiation.
                               }
                 deriving (Eq, Show)

data ProbStatement = ProbStatement { _probStmntBlock    :: {-# UNPACK #-} !Int
                                   , _probStmntNonBlock :: {-# UNPACK #-} !Int
                                   , _probStmntCond     :: {-# UNPACK #-} !Int
                                   , _probStmntFor      :: {-# UNPACK #-} !Int
                                   }
                   deriving (Eq, Show)

data ProbEventList = ProbEventList { _probEventListAll :: {-# UNPACK #-} !Int
                                   , _probEventListClk :: {-# UNPACK #-} !Int
                                   , _probEventListVar :: {-# UNPACK #-} !Int
                                   }
                   deriving (Eq, Show)

data Probability = Probability { _probModItem   :: {-# UNPACK #-} !ProbModItem
                               , _probStmnt     :: {-# UNPACK #-} !ProbStatement
                               , _probExpr      :: {-# UNPACK #-} !ProbExpr
                               , _probEventList :: {-# UNPACK #-} !ProbEventList
                               }
                 deriving (Eq, Show)

data Property = Property { _propSize       :: {-# UNPACK #-} !Int
                         , _propSeed       :: !(Maybe Int)
                         , _propStmntDepth :: {-# UNPACK #-} !Int
                         , _propModDepth   :: {-# UNPACK #-} !Int
                         , _propMaxModules :: {-# UNPACK #-} !Int
                         }
              deriving (Eq, Show)

data SimDescription = SimDescription { _simName :: {-# UNPACK #-} !Text }
                    deriving (Eq, Show)

data SynthDescription = SynthDescription { _synthName :: {-# UNPACK #-} !Text }
                      deriving (Eq, Show)

data Config = Config { _configProbability  :: {-# UNPACK #-} !Probability
                     , _configProperty     :: {-# UNPACK #-} !Property
                     , _configSimulators   :: ![SimDescription]
                     , _configSynthesisers :: ![SynthDescription]
                     }
            deriving (Eq, Show)

makeLenses ''ProbExpr
makeLenses ''ProbModItem
makeLenses ''ProbStatement
makeLenses ''ProbEventList
makeLenses ''Probability
makeLenses ''Property
makeLenses ''Config

defaultValue
    :: (Alternative r, Applicative w)
    => b
    -> Toml.Codec r w a b
    -> Toml.Codec r w a b
defaultValue x = Toml.dimap Just (fromMaybe x) . Toml.dioptional

defaultConfig :: Config
defaultConfig = Config
    (Probability defModItem defStmnt defExpr defEvent)
    (Property 20 Nothing 3 2 5)
    []
    [SynthDescription "yosys", SynthDescription "vivado"]
  where
    defModItem = ProbModItem 5 1 1
    defStmnt   = ProbStatement 0 15 1 1
    defExpr    = ProbExpr 1 1 1 1 1 1 0 1 1
    defEvent   = ProbEventList 1 1 1

twoKey :: Toml.Piece -> Toml.Piece -> Toml.Key
twoKey a b = Toml.Key (a :| [b])

int :: Toml.Piece -> Toml.Piece -> TomlCodec Int
int a = Toml.int . twoKey a

exprCodec :: TomlCodec ProbExpr
exprCodec =
    ProbExpr
        <$> defaultValue (defProb probExprNum) (intE "number")
        .=  _probExprNum
        <*> defaultValue (defProb probExprId) (intE "variable")
        .=  _probExprId
        <*> defaultValue (defProb probExprUnOp) (intE "unary")
        .=  _probExprUnOp
        <*> defaultValue (defProb probExprBinOp) (intE "binary")
        .=  _probExprBinOp
        <*> defaultValue (defProb probExprCond) (intE "ternary")
        .=  _probExprCond
        <*> defaultValue (defProb probExprConcat) (intE "concatenation")
        .=  _probExprConcat
        <*> defaultValue (defProb probExprStr) (intE "string")
        .=  _probExprStr
        <*> defaultValue (defProb probExprSigned) (intE "signed")
        .=  _probExprSigned
        <*> defaultValue (defProb probExprUnsigned) (intE "unsigned")
        .=  _probExprUnsigned
  where
    defProb i = defaultConfig ^. configProbability . probExpr . i
    intE = int "expr"

stmntCodec :: TomlCodec ProbStatement
stmntCodec =
    ProbStatement
        <$> defaultValue (defProb probStmntBlock) (intS "blocking")
        .=  _probStmntBlock
        <*> defaultValue (defProb probStmntNonBlock) (intS "nonblocking")
        .=  _probStmntNonBlock
        <*> defaultValue (defProb probStmntCond) (intS "conditional")
        .=  _probStmntCond
        <*> defaultValue (defProb probStmntFor) (intS "forloop")
        .=  _probStmntFor
  where
    defProb i = defaultConfig ^. configProbability . probStmnt . i
    intS = int "statement"

modItemCodec :: TomlCodec ProbModItem
modItemCodec =
    ProbModItem
        <$> defaultValue (defProb probModItemAssign) (intM "assign")
        .=  _probModItemAssign
        <*> defaultValue (defProb probModItemAlways) (intM "always")
        .=  _probModItemAlways
        <*> defaultValue (defProb probModItemInst) (intM "instantiation")
        .=  _probModItemInst
  where
    defProb i = defaultConfig ^. configProbability . probModItem . i
    intM = int "moditem"

eventListCodec :: TomlCodec ProbEventList
eventListCodec =
    ProbEventList
        <$> defaultValue (defProb probEventListClk) (intE "clk")
        .=  _probEventListClk
        <*> defaultValue (defProb probEventListClk) (intE "all")
        .=  _probEventListAll
        <*> defaultValue (defProb probEventListClk) (intE "var")
        .=  _probEventListClk
  where
    defProb i = defaultConfig ^. configProbability . probEventList . i
    intE = int "eventlist"

probCodec :: TomlCodec Probability
probCodec =
    Probability
        <$> defaultValue (defProb probModItem) modItemCodec
        .=  _probModItem
        <*> defaultValue (defProb probStmnt) stmntCodec
        .=  _probStmnt
        <*> defaultValue (defProb probExpr) exprCodec
        .=  _probExpr
        <*> defaultValue (defProb probEventList) eventListCodec
        .=  _probEventList
    where defProb i = defaultConfig ^. configProbability . i

propCodec :: TomlCodec Property
propCodec =
    Property
        <$> defaultValue (defProp propSize) (Toml.int "size")
        .=  _propSize
        <*> Toml.dioptional (Toml.int "seed")
        .=  _propSeed
        <*> defaultValue (defProp propStmntDepth) (int "statement" "depth")
        .=  _propStmntDepth
        <*> defaultValue (defProp propModDepth) (int "module" "depth")
        .=  _propModDepth
        <*> defaultValue (defProp propMaxModules) (int "module" "max")
        .=  _propMaxModules
    where defProp i = defaultConfig ^. configProperty . i

simulator :: TomlCodec SimDescription
simulator = Toml.textBy pprint parseIcarus "name"
  where
    parseIcarus i@"icarus" = Right $ SimDescription i
    parseIcarus s = Left $ "Could not match '" <> s <> "' with a simulator."
    pprint (SimDescription a) = a

synthesiser :: TomlCodec SynthDescription
synthesiser = Toml.textBy pprint parseIcarus "name"
  where
    parseIcarus s@"yosys"   = Right $ SynthDescription s
    parseIcarus s@"vivado"  = Right $ SynthDescription s
    parseIcarus s@"quartus" = Right $ SynthDescription s
    parseIcarus s@"xst"     = Right $ SynthDescription s
    parseIcarus s = Left $ "Could not match '" <> s <> "' with a synthesiser."
    pprint (SynthDescription a) = a

configCodec :: TomlCodec Config
configCodec =
    Config
        <$> defaultValue (defaultConfig ^. configProbability)
                         (Toml.table probCodec "probability")
        .=  _configProbability
        <*> defaultValue (defaultConfig ^. configProperty)
                         (Toml.table propCodec "property")
        .=  _configProperty
        <*> defaultValue (defaultConfig ^. configSimulators)
                         (Toml.list simulator "simulator")
        .=  _configSimulators
        <*> defaultValue (defaultConfig ^. configSynthesisers)
                         (Toml.list synthesiser "synthesiser")
        .=  _configSynthesisers

parseConfigFile :: FilePath -> IO Config
parseConfigFile = Toml.decodeFile configCodec

parseConfig :: Text -> Config
parseConfig t = case Toml.decode configCodec t of
    Right c                      -> c
    Left Toml.TrivialError -> error "Trivial error while parsing Toml config"
    Left  (Toml.KeyNotFound   k) -> error $ "Key " ++ show k ++ " not found"
    Left  (Toml.TableNotFound k) -> error $ "Table " ++ show k ++ " not found"
    Left (Toml.TypeMismatch k _ _) ->
        error $ "Type mismatch with key " ++ show k
    Left _ -> error "Config file parse error"

encodeConfig :: Config -> Text
encodeConfig = Toml.encode configCodec

encodeConfigFile :: FilePath -> Config -> IO ()
encodeConfigFile f = T.writeFile f . encodeConfig
