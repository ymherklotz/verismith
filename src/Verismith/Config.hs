{-|
Module      : Verismith.Config
Description : Configuration file format and parser.
Copyright   : (c) 2019, Yann Herklotz
License     : GPL-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

TOML Configuration file format and parser.
-}

{-# LANGUAGE TemplateHaskell #-}

module Verismith.Config
    ( -- * TOML Configuration
      -- $conf
      Config(..)
    , defaultConfig
    -- ** Probabilities
    , Probability(..)
    -- *** Expression
    , ProbExpr(..)
    -- *** Module Item
    , ProbModItem(..)
    -- *** Statement
    , ProbStatement(..)
    -- ** ConfProperty
    , ConfProperty(..)
    -- ** Simulator Description
    , SimDescription(..)
    -- ** Synthesiser Description
    , SynthDescription(..)
    -- * Useful Lenses
    , fromXST
    , fromYosys
    , fromVivado
    , fromQuartus
    , configProbability
    , configProperty
    , configSimulators
    , configSynthesisers
    , probModItem
    , probStmnt
    , probExpr
    , probExprNum
    , probExprId
    , probExprRangeSelect
    , probExprUnOp
    , probExprBinOp
    , probExprCond
    , probExprConcat
    , probExprStr
    , probExprSigned
    , probExprUnsigned
    , probModItemAssign
    , probModItemSeqAlways
    , probModItemCombAlways
    , probModItemInst
    , probStmntBlock
    , probStmntNonBlock
    , probStmntCond
    , probStmntFor
    , propSampleSize
    , propSampleMethod
    , propSize
    , propSeed
    , propStmntDepth
    , propModDepth
    , propMaxModules
    , propCombine
    , propDeterminism
    , propNonDeterminism
    , parseConfigFile
    , parseConfig
    , encodeConfig
    , encodeConfigFile
    , versionInfo
    )
where

import           Control.Applicative    (Alternative)
import           Control.Lens           hiding ((.=))
import           Data.List.NonEmpty     (NonEmpty (..))
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text, pack)
import qualified Data.Text.IO           as T
import           Data.Version           (showVersion)
import           Development.GitRev
import           Hedgehog.Internal.Seed (Seed)
import           Paths_verismith        (version)
import           Shelly                 (toTextIgnore)
import           Toml                   (TomlCodec, (.=))
import qualified Toml
import           Verismith.Sim.Quartus
import           Verismith.Sim.Vivado
import           Verismith.Sim.XST
import           Verismith.Sim.Yosys

-- $conf
--
-- Verismith supports a TOML configuration file that can be passed using the @-c@
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
-- 'defaultConfig' or when running @verismith config@.
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

-- | Probability of different expressions nodes.
data ProbExpr = ProbExpr { _probExprNum         :: {-# UNPACK #-} !Int
                         -- ^ Probability of generation a number like
                         -- @4'ha@. This should never be set to 0, as it is used
                         -- as a fallback in case there are no viable
                         -- identifiers, such as none being in scope.
                         , _probExprId          :: {-# UNPACK #-} !Int
                         -- ^ Probability of generating an identifier that is in
                         -- scope and of the right type.
                         , _probExprRangeSelect :: {-# UNPACK #-} !Int
                         -- ^ Probability of generating a range selection from a port.
                         , _probExprUnOp        :: {-# UNPACK #-} !Int
                         -- ^ Probability of generating a unary operator.
                         , _probExprBinOp       :: {-# UNPACK #-} !Int
                         -- ^ Probability of generation a binary operator.
                         , _probExprCond        :: {-# UNPACK #-} !Int
                         -- ^ probability of generating a conditional ternary
                         -- operator.
                         , _probExprConcat      :: {-# UNPACK #-} !Int
                         -- ^ Probability of generating a concatenation.
                         , _probExprStr         :: {-# UNPACK #-} !Int
                         -- ^ Probability of generating a string. This is not
                         -- fully supported therefore currently cannot be set.
                         , _probExprSigned      :: {-# UNPACK #-} !Int
                         -- ^ Probability of generating a signed function
                         -- @$signed(...)@.
                         , _probExprUnsigned    :: {-# UNPACK #-} !Int
                         -- ^ Probability of generating an unsigned function
                         -- @$unsigned(...)@.
                         }
              deriving (Eq, Show)

-- | Probability of generating different nodes inside a module declaration.
data ProbModItem = ProbModItem { _probModItemAssign     :: {-# UNPACK #-} !Int
                               -- ^ Probability of generating an @assign@.
                               , _probModItemSeqAlways  :: {-# UNPACK #-} !Int
                               -- ^ Probability of generating a sequential @always@ block.
                               , _probModItemCombAlways :: {-# UNPACK #-} !Int
                               -- ^ Probability of generating an combinational @always@ block.
                               , _probModItemInst       :: {-# UNPACK #-} !Int
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

data Probability = Probability { _probModItem :: {-# UNPACK #-} !ProbModItem
                               , _probStmnt   :: {-# UNPACK #-} !ProbStatement
                               , _probExpr    :: {-# UNPACK #-} !ProbExpr
                               }
                 deriving (Eq, Show)

data ConfProperty = ConfProperty { _propSize           :: {-# UNPACK #-} !Int
                                 -- ^ The size of the generated Verilog.
                                 , _propSeed           :: !(Maybe Seed)
                                 -- ^ A possible seed that could be used to
                                 -- generate the same Verilog.
                                 , _propStmntDepth     :: {-# UNPACK #-} !Int
                                 -- ^ The maximum statement depth that should be
                                 -- reached.
                                 , _propModDepth       :: {-# UNPACK #-} !Int
                                 -- ^ The maximium module depth that should be
                                 -- reached.
                                 , _propMaxModules     :: {-# UNPACK #-} !Int
                                 -- ^ The maximum number of modules that are
                                 -- allowed to be created at each level.
                                 , _propSampleMethod   :: !Text
                                 -- ^ The sampling method that should be used to
                                 -- generate specific distributions of random
                                 -- programs.
                                 , _propSampleSize     :: {-# UNPACK #-} !Int
                                 -- ^ The number of samples to take for the
                                 -- sampling method.
                                 , _propCombine        :: !Bool
                                 -- ^ If the output should be combined into one
                                 -- bit or not.
                                 , _propNonDeterminism :: {-# UNPACK #-} !Int
                                 -- ^ The frequency at which nondeterminism
                                 -- should be generated.
                                 , _propDeterminism    :: {-# UNPACK #-} !Int
                                 -- ^ The frequency at which determinism should
                                 -- be generated.
                                 }
                  deriving (Eq, Show)

data Info = Info { _infoCommit  :: !Text
                 , _infoVersion :: !Text
                 }
          deriving (Eq, Show)

data SimDescription = SimDescription { simName :: {-# UNPACK #-} !Text }
                    deriving (Eq, Show)

data SynthDescription = SynthDescription { synthName :: {-# UNPACK #-} !Text
                                         , synthBin  :: Maybe Text
                                         , synthDesc :: Maybe Text
                                         , synthOut  :: Maybe Text
                                         }
                      deriving (Eq, Show)

data Config = Config { _configInfo         :: Info
                     , _configProbability  :: {-# UNPACK #-} !Probability
                     , _configProperty     :: {-# UNPACK #-} !ConfProperty
                     , _configSimulators   :: [SimDescription]
                     , _configSynthesisers :: [SynthDescription]
                     }
            deriving (Eq, Show)

$(makeLenses ''ProbExpr)
$(makeLenses ''ProbModItem)
$(makeLenses ''ProbStatement)
$(makeLenses ''Probability)
$(makeLenses ''ConfProperty)
$(makeLenses ''Info)
$(makeLenses ''Config)

defaultValue
    :: (Alternative r, Applicative w)
    => b
    -> Toml.Codec r w a b
    -> Toml.Codec r w a b
defaultValue x = Toml.dimap Just (fromMaybe x) . Toml.dioptional

fromXST :: XST -> SynthDescription
fromXST (XST a b c) =
    SynthDescription "xst" (toTextIgnore <$> a) (Just b) (Just $ toTextIgnore c)

fromYosys :: Yosys -> SynthDescription
fromYosys (Yosys a b c) = SynthDescription "yosys"
                                           (toTextIgnore <$> a)
                                           (Just b)
                                           (Just $ toTextIgnore c)

fromVivado :: Vivado -> SynthDescription
fromVivado (Vivado a b c) = SynthDescription "vivado"
                                             (toTextIgnore <$> a)
                                             (Just b)
                                             (Just $ toTextIgnore c)

fromQuartus :: Quartus -> SynthDescription
fromQuartus (Quartus a b c) = SynthDescription "quartus"
                                               (toTextIgnore <$> a)
                                               (Just b)
                                               (Just $ toTextIgnore c)

defaultConfig :: Config
defaultConfig = Config
    (Info (pack $(gitHash)) (pack $ showVersion version))
    (Probability defModItem defStmnt defExpr)
    (ConfProperty 20 Nothing 3 2 5 "random" 10 False 0 1)
    []
    [fromYosys defaultYosys, fromVivado defaultVivado]
  where
    defModItem =
        ProbModItem 5 -- Assign
                      1 -- Sequential Always
                        1 -- Combinational Always
                          1 -- Instantiation
    defStmnt =
        ProbStatement 0 -- Blocking assignment
                        3 -- Non-blocking assignment
                          1 -- Conditional
                            0 -- For loop
    defExpr =
        ProbExpr 1 -- Number
                   5 -- Identifier
                     5 -- Range selection
                       5 -- Unary operator
                         5 -- Binary operator
                           5 -- Ternary conditional
                             3 -- Concatenation
                               0 -- String
                                 5 -- Signed function
                                   5 -- Unsigned funtion

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
        <*> defaultValue (defProb probExprRangeSelect) (intE "rangeselect")
        .=  _probExprRangeSelect
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
        <*> defaultValue (defProb probModItemSeqAlways) (intM "sequential")
        .=  _probModItemSeqAlways
        <*> defaultValue (defProb probModItemCombAlways) (intM "combinational")
        .=  _probModItemCombAlways
        <*> defaultValue (defProb probModItemInst) (intM "instantiation")
        .=  _probModItemInst
  where
    defProb i = defaultConfig ^. configProbability . probModItem . i
    intM = int "moditem"

probCodec :: TomlCodec Probability
probCodec =
    Probability
        <$> defaultValue (defProb probModItem) modItemCodec
        .=  _probModItem
        <*> defaultValue (defProb probStmnt) stmntCodec
        .=  _probStmnt
        <*> defaultValue (defProb probExpr) exprCodec
        .=  _probExpr
    where defProb i = defaultConfig ^. configProbability . i

propCodec :: TomlCodec ConfProperty
propCodec =
    ConfProperty
        <$> defaultValue (defProp propSize) (Toml.int "size")
        .=  _propSize
        <*> Toml.dioptional (Toml.read "seed")
        .=  _propSeed
        <*> defaultValue (defProp propStmntDepth) (int "statement" "depth")
        .=  _propStmntDepth
        <*> defaultValue (defProp propModDepth) (int "module" "depth")
        .=  _propModDepth
        <*> defaultValue (defProp propMaxModules) (int "module" "max")
        .=  _propMaxModules
        <*> defaultValue (defProp propSampleMethod)
                         (Toml.text (twoKey "sample" "method"))
        .=  _propSampleMethod
        <*> defaultValue (defProp propSampleSize) (int "sample" "size")
        .=  _propSampleSize
        <*> defaultValue (defProp propCombine)
                         (Toml.bool (twoKey "output" "combine"))
        .=  _propCombine
        <*> defaultValue (defProp propNonDeterminism) (Toml.int "nondeterminism")
        .=  _propNonDeterminism
        <*> defaultValue (defProp propDeterminism) (Toml.int "determinism")
        .=  _propDeterminism
    where defProp i = defaultConfig ^. configProperty . i

simulator :: TomlCodec SimDescription
simulator = Toml.textBy pprint parseIcarus "name"
  where
    parseIcarus i@"icarus" = Right $ SimDescription i
    parseIcarus s = Left $ "Could not match '" <> s <> "' with a simulator."
    pprint (SimDescription a) = a

synthesiser :: TomlCodec SynthDescription
synthesiser =
    SynthDescription
        <$> Toml.text "name"
        .=  synthName
        <*> Toml.dioptional (Toml.text "bin")
        .=  synthBin
        <*> Toml.dioptional (Toml.text "description")
        .=  synthDesc
        <*> Toml.dioptional (Toml.text "output")
        .=  synthOut

infoCodec :: TomlCodec Info
infoCodec =
    Info
        <$> defaultValue (defaultConfig ^. configInfo . infoCommit)
                         (Toml.text "commit")
        .=  _infoCommit
        <*> defaultValue (defaultConfig ^. configInfo . infoVersion)
                         (Toml.text "version")
        .=  _infoVersion

configCodec :: TomlCodec Config
configCodec =
    Config
        <$> defaultValue (defaultConfig ^. configInfo)
                         (Toml.table infoCodec "info")
        .=  _configInfo
        <*> defaultValue (defaultConfig ^. configProbability)
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

versionInfo :: String
versionInfo =
    "Verismith "
        <> showVersion version
        <> " ("
        <> $(gitCommitDate)
        <> " "
        <> $(gitHash)
        <> ")"
