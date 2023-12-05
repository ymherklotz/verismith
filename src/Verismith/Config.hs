{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Verismith.Config
-- Description : Configuration file format and parser.
-- Copyright   : (c) 2019, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- TOML Configuration file format and parser.
module Verismith.Config
  ( -- * TOML Configuration
    -- $conf
    Config (..),
    defaultConfig,

    -- ** Probabilities
    Probability (..),

    -- *** Expression
    ProbExpr (..),

    -- *** Module Item
    ProbModItem (..),

    -- *** Statement
    ProbStatement (..),

    -- *** Module
    ProbMod (..),

    -- ** EMI Configuration
    ConfEMI (..),

    -- ** ConfProperty
    ConfProperty (..),

    -- ** ConfGarbage
    NumberProbability (..),
    CategoricalProbability (..),
    uniformCP,
    GeneratorOpts (..),
    defGeneratorOpts,

    -- ** Simulator Description
    SimDescription (..),

    -- ** Synthesiser Description
    SynthDescription (..),

    -- * Useful Lenses
    fromXST,
    fromYosys,
    fromVivado,
    fromQuartus,
    fromQuartusLight,
    configEMI,
    configProbability,
    configGarbageGenerator,
    configProperty,
    configSimulators,
    configSynthesisers,
    confEMIGenerateProb,
    confEMINoGenerateProb,
    probModItem,
    probMod,
    probModDropOutput,
    probModKeepOutput,
    probStmnt,
    probExpr,
    probExprNum,
    probExprId,
    probExprRangeSelect,
    probExprUnOp,
    probExprBinOp,
    probExprCond,
    probExprConcat,
    probExprStr,
    probExprSigned,
    probExprUnsigned,
    probModItemAssign,
    probModItemSeqAlways,
    probModItemCombAlways,
    probModItemInst,
    probStmntBlock,
    probStmntNonBlock,
    probStmntCond,
    probStmntFor,
    propSampleSize,
    propSampleMethod,
    propSize,
    propSeed,
    propStmntDepth,
    propModDepth,
    propMaxModules,
    propCombine,
    propDeterminism,
    propNonDeterminism,
    propDefaultYosys,
    goSeed,
    parseConfigFile,
    parseConfig,
    parseConfigFileRelaxed,
    parseConfigRelaxed,
    encodeConfig,
    encodeConfigFile,
    versionInfo,
  )
where

import Control.Applicative (Alternative, liftA2, liftA3, (<|>))
import Control.Lens hiding ((.=))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed as VU
import Data.Version (showVersion)
import Data.Word (Word32)
import Development.GitRev
import Hedgehog.Internal.Seed (Seed)
import Paths_verismith (version)
import Shelly (toTextIgnore)
import Toml (TomlCodec, (.=))
import qualified Toml
import Verismith.Tool.Quartus
import Verismith.Tool.QuartusLight
import Verismith.Tool.Vivado
import Verismith.Tool.XST
import Verismith.Tool.Yosys
import Verismith.Utils (uncurry3)

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
data ProbExpr = ProbExpr
  { -- | @expr.number@: probability of generation a number like
    -- @4'ha@. This should never be set to 0, as it is used
    -- as a fallback in case there are no viable
    -- identifiers, such as none being in scope.
    _probExprNum :: {-# UNPACK #-} !Int,
    -- | @expr.variable@: probability of generating an identifier that is in
    -- scope and of the right type.
    _probExprId :: {-# UNPACK #-} !Int,
    -- | @expr.rangeselect@: probability of generating a range
    -- selection from a port (@reg1[2:0]@).
    _probExprRangeSelect :: {-# UNPACK #-} !Int,
    -- | @expr.unary@: probability of generating a unary operator.
    _probExprUnOp :: {-# UNPACK #-} !Int,
    -- | @expr.binary@: probability of generation a binary operator.
    _probExprBinOp :: {-# UNPACK #-} !Int,
    -- | @expr.ternary@: probability of generating a conditional ternary
    -- operator.
    _probExprCond :: {-# UNPACK #-} !Int,
    -- | @expr.concatenation@: probability of generating a concatenation.
    _probExprConcat :: {-# UNPACK #-} !Int,
    -- | @expr.string@: probability of generating a string. This is not
    -- fully supported therefore currently cannot be set.
    _probExprStr :: {-# UNPACK #-} !Int,
    -- | @expr.signed@: probability of generating a signed function
    -- @$signed(...)@.
    _probExprSigned :: {-# UNPACK #-} !Int,
    -- | @expr.unsigned@: probability of generating an unsigned function
    -- @$unsigned(...)@.
    _probExprUnsigned :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

-- | Probability of generating different nodes inside a module declaration.
data ProbModItem = ProbModItem
  { -- | @moditem.assign@: probability of generating an @assign@.
    _probModItemAssign :: {-# UNPACK #-} !Int,
    -- | @moditem.sequential@: probability of generating a sequential @always@ block.
    _probModItemSeqAlways :: {-# UNPACK #-} !Int,
    -- | @moditem.combinational@: probability of generating an combinational @always@
    -- block. This is currently not implemented.
    _probModItemCombAlways :: {-# UNPACK #-} !Int,
    -- | @moditem.instantiation@: probability of generating a module
    -- instantiation.
    _probModItemInst :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

-- | Probability of generating different statements.
data ProbStatement = ProbStatement
  { -- | @statement.blocking@: probability of generating blocking assignments.
    _probStmntBlock :: {-# UNPACK #-} !Int,
    -- | @statement.nonblocking@: probability of generating nonblocking assignments.
    _probStmntNonBlock :: {-# UNPACK #-} !Int,
    -- | @statement.conditional@: probability of generating conditional
    -- statements (@if@ statements).
    _probStmntCond :: {-# UNPACK #-} !Int,
    -- | @statement.forloop@: probability of generating for loops.
    _probStmntFor :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

-- | Probability of generating various properties of a module.
data ProbMod = ProbMod
  { -- | "@module.drop_output@: frequency of a wire or register being dropped from the output."
    _probModDropOutput :: {-# UNPACK #-} !Int,
    -- | "@module.keep_output@: frequency of a wire or register being kept in the output."
    _probModKeepOutput :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

-- | @[probability]@: combined probabilities.
data Probability = Probability
  { -- | Probabilities for module items.
    _probModItem :: {-# UNPACK #-} !ProbModItem,
    -- | Probabilities for statements.
    _probStmnt :: {-# UNPACK #-} !ProbStatement,
    -- | Probaiblities for expressions.
    _probExpr :: {-# UNPACK #-} !ProbExpr,
    _probMod :: {-# UNPACK #-} !ProbMod
  }
  deriving (Eq, Show)

data ConfEMI = ConfEMI
  { -- | Probability of generating a new EMI statement with a new EMI entry.
    _confEMIGenerateProb :: {-# UNPACK #-} !Int,
    _confEMINoGenerateProb :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

-- | @[property]@: properties for the generated Verilog file.
data ConfProperty = ConfProperty
  { -- | @size@: the size of the generated Verilog.
    _propSize :: {-# UNPACK #-} !Int,
    -- | @seed@: a possible seed that could be used to
    -- generate the same Verilog.
    _propSeed :: !(Maybe Seed),
    -- | @statement.depth@: the maximum statement depth that should be
    -- reached.
    _propStmntDepth :: {-# UNPACK #-} !Int,
    -- | @module.depth@: the maximium module depth that should be
    -- reached.
    _propModDepth :: {-# UNPACK #-} !Int,
    -- | @module.max@: the maximum number of modules that are
    -- allowed to be created at each level.
    _propMaxModules :: {-# UNPACK #-} !Int,
    -- | @sample.method@: the sampling method that should be used to
    -- generate specific distributions of random
    -- programs.
    _propSampleMethod :: !Text,
    -- | @sample.size@: the number of samples to take for the
    -- sampling method.
    _propSampleSize :: {-# UNPACK #-} !Int,
    -- | @output.combine@: if the output should be combined into one
    -- bit or not.
    _propCombine :: !Bool,
    -- | @nondeterminism@: the frequency at which nondeterminism
    -- should be generated (currently a work in progress).
    _propNonDeterminism :: {-# UNPACK #-} !Int,
    -- | @determinism@: the frequency at which determinism should
    -- be generated (currently modules are always deterministic).
    _propDeterminism :: {-# UNPACK #-} !Int,
    -- | @default.yosys@: Default location for Yosys, which will be used for
    -- equivalence checking.
    _propDefaultYosys :: !(Maybe Text)
  }
  deriving (Eq, Show)

data NumberProbability
  = NPUniform
      { _NPULow :: !Int,
        _NPUHigh :: !Int
      }
  | NPBinomial
      { _NPBOffset :: !Int,
        _NPBTrials :: !Int,
        _NPBSuccess :: !Double
      }
  | NPNegativeBinomial -- Geometric is negative binomial with 1 failure
      { _NPNBOffset :: !Int,
        _NPNBFailRate :: !Double,
        _NPNBFailure :: !Int
      }
  | NPPoisson
      { _NPPOffset :: !Int,
        _NPPParam :: !Double
      }
  | NPDiscrete !(NonEmpty (Double, Int)) -- Weight, outcome index
  | NPLinearComb !(NonEmpty (Double, NumberProbability))
  deriving (Eq, Show)

data CategoricalProbability
  = CPDiscrete !(NonEmpty Double)
  | CPBiasedUniform
      { _CPBUBiases :: ![(Double, Int)],
        _CPBUUniformWeight :: Double
      }
  deriving (Eq, Show)

uniformCP :: CategoricalProbability
uniformCP = CPBiasedUniform [] 1

data GeneratorOpts = GeneratorOpts -- TODO LATER: duplicate OptionalElement for each use
  { _goSeed :: !(Maybe (VU.Vector Word32)),
    _goExprCurAttenuation :: !Double,
    _goAttribCurAttenuation :: !Double,
    _goStmtCurAttenuation :: !Double,
    _goExprRecAttenuation :: !Double,
    _goAttribRecAttenuation :: !Double,
    _goStmtRecAttenuation :: !Double,
    _goOptionalElement :: !Double,
    _goConfigs :: !NumberProbability,
    _goConfigItems :: !NumberProbability,
    _goDesigns :: !NumberProbability,
    _goCell_Inst :: !Double,
    _goLiblist_Use :: !Double,
    _goPrimitives :: !NumberProbability,
    _goSequential_Combinatorial :: !Double,
    _goTableRows :: !NumberProbability,
    _goPortInitialisation :: !Double,
    _goPrimitiveInitialisation :: !CategoricalProbability,
    _goEdgeSensitiveRow :: !Double,
    _goTableInLevel :: !CategoricalProbability,
    _goTableOutLevel :: !CategoricalProbability,
    _goEdgeSimple :: !Double,
    _goEdgePos_Neg :: !Double,
    _goModules :: !NumberProbability,
    _goParameters :: !NumberProbability,
    _goLocalParameters :: !NumberProbability,
    _goParameterOverrides :: !NumberProbability,
    _goDeclarations :: !NumberProbability,
    _goOtherItems :: !NumberProbability,
    _goArguments :: !NumberProbability,
    _goModuleItems :: !NumberProbability,
    _goModuleItem :: !CategoricalProbability,
    _goPortType :: !CategoricalProbability,
    _goModuleCell :: !Double,
    _goUnconnectedDrive :: !CategoricalProbability,
    _goTimeMagnitude :: !CategoricalProbability,
    _goSpecifyItems :: !NumberProbability,
    _goSpecifyItem :: !CategoricalProbability,
    _goSpecifyParameters :: !NumberProbability,
    _goInitialisation_PathPulse :: !Double,
    _goModulePathCondition :: !CategoricalProbability,
    _goPathDelayCount :: !CategoricalProbability,
    _goPathFull_Parallel :: !Double,
    _goFullPathTerms :: !NumberProbability,
    _goPulseEvent_Detect :: !Double,
    _goShowCancelled :: !Double,
    _goPolarity :: !CategoricalProbability,
    _goEdgeSensitivity :: !CategoricalProbability,
    _goSystemTimingCheck :: !CategoricalProbability,
    _goTimingTransition :: !Double,
    _goTimingCheckNeg_Pos :: !Double,
    _goGenerateSingle_Block :: !Double,
    _goGenerateItem :: !CategoricalProbability,
    _goModGenItem :: !CategoricalProbability,
    _goModGenDeclaration :: !CategoricalProbability,
    _goDimension_Initialisation :: !Double,
    _goAutomatic :: !Double,
    _goStatement :: !CategoricalProbability,
    _goTypeAbstract_Concrete :: !Double,
    _goAbstractType :: !CategoricalProbability,
    _goBlockDeclType :: !CategoricalProbability,
    _goNetType :: !CategoricalProbability,
    _goNet_Tri :: !Double,
    _goVectoring :: !CategoricalProbability,
    _goRegister :: !Double,
    _goDirection :: !CategoricalProbability,
    _goDriveStrength :: !CategoricalProbability,
    _goChargeStrength :: !CategoricalProbability,
    _goNInpGate :: !CategoricalProbability,
    _goLoopStatement :: !CategoricalProbability,
    _goCaseStatement :: !CategoricalProbability,
    _goProcContAssign :: !CategoricalProbability,
    _goPCAVar_Net :: !Double,
    _goGate :: !CategoricalProbability,
    _goGateReverse :: !Double,
    _goGate1_0 :: !Double,
    _goDelayEvent :: !CategoricalProbability,
    _goEvents :: !NumberProbability,
    _goEvent :: !CategoricalProbability,
    _goEventPrefix :: !CategoricalProbability,
    _goNamed_Positional :: !Double,
    _goBlockPar_Seq :: !Double,
    _goAssignmentBlocking :: !Double,
    _goCaseBranches :: !NumberProbability,
    _goLValues :: !NumberProbability,
    _goAttributes :: !NumberProbability,
    _goPaths :: !NumberProbability,
    _goDelay :: !CategoricalProbability,
    _goMinTypMax :: !Double,
    _goRangeExpr :: !CategoricalProbability,
    _goRangeOffsetPos_Neg :: !Double,
    _goDimensions :: !NumberProbability,
    _goSignedness :: !Double,
    _goExpression :: !CategoricalProbability,
    _goConcatenations :: !NumberProbability,
    _goUnaryOperation :: !CategoricalProbability,
    _goBinaryOperation :: !CategoricalProbability,
    _goPrimary :: !CategoricalProbability,
    _goIntRealIdent :: !CategoricalProbability,
    _goEscaped_Simple :: !Double,
    _goSimpleLetters :: !NumberProbability,
    _goSimpleLetter :: !CategoricalProbability,
    _goEscapedLetters :: !NumberProbability,
    _goEscapedLetter :: !CategoricalProbability,
    _goSystemLetters :: !NumberProbability,
    _goLiteralWidth :: !CategoricalProbability,
    _goStringCharacters :: !NumberProbability,
    _goStringCharacter :: !CategoricalProbability,
    _goFixed_Floating :: !Double,
    _goExponentSign :: !CategoricalProbability,
    _goX_Z :: !Double,
    _goBinarySymbols :: !NumberProbability,
    _goBinarySymbol :: !CategoricalProbability,
    _goOctalSymbols :: !NumberProbability,
    _goOctalSymbol :: !CategoricalProbability,
    _goDecimalSymbols :: !NumberProbability,
    _goDecimalSymbol :: !CategoricalProbability,
    _goHexadecimalSymbols :: !NumberProbability,
    _goHexadecimalSymbol :: !CategoricalProbability
  }
  deriving (Eq, Show)

defGeneratorOpts :: GeneratorOpts
defGeneratorOpts =
  GeneratorOpts
    { _goSeed = Nothing,
      _goExprCurAttenuation = 1,
      _goAttribCurAttenuation = 1,
      _goStmtCurAttenuation = 1,
      _goExprRecAttenuation = 0.5,
      _goAttribRecAttenuation = 0.5,
      _goStmtRecAttenuation = 0.5,
      _goOptionalElement = 0.5,
      _goConfigs = NPPoisson 0 1,
      _goConfigItems = NPPoisson 0 1,
      _goDesigns = NPPoisson 0 1,
      _goCell_Inst = 0.5,
      _goLiblist_Use = 0.5,
      _goPrimitives = NPPoisson 0 2,
      _goSequential_Combinatorial = 0.5,
      _goTableRows = NPPoisson 0 4,
      _goPortInitialisation = 0.5,
      _goPrimitiveInitialisation = uniformCP,
      _goEdgeSensitiveRow = 0.5,
      _goTableInLevel = uniformCP,
      _goTableOutLevel = uniformCP,
      _goEdgeSimple = 0.5,
      _goEdgePos_Neg = 0.5,
      _goModules = NPPoisson 1 2,
      _goParameters = NPNegativeBinomial 0 0.5 1,
      _goLocalParameters = NPNegativeBinomial 0 0.5 1,
      _goParameterOverrides = NPNegativeBinomial 0 0.75 1,
      _goDeclarations = NPPoisson 0 1,
      _goOtherItems = NPPoisson 0 3,
      _goArguments = NPNegativeBinomial 0 (2.0 / 5.0) 1,
      _goModuleItems = NPPoisson 0 3,
      _goModuleItem = CPDiscrete [4, 2, 1],
      _goPortType = uniformCP,
      _goModuleCell = 0.5,
      _goUnconnectedDrive = uniformCP,
      _goTimeMagnitude = uniformCP,
      _goSpecifyItems = NPPoisson 0 1,
      _goSpecifyItem = uniformCP,
      _goSpecifyParameters = NPPoisson 0 1,
      _goInitialisation_PathPulse = 0.5,
      _goModulePathCondition = uniformCP,
      _goPathDelayCount = uniformCP,
      _goPathFull_Parallel = 0.5,
      _goFullPathTerms = NPPoisson 0 1,
      _goPulseEvent_Detect = 0.5,
      _goShowCancelled = 0.5,
      _goPolarity = uniformCP,
      _goEdgeSensitivity = uniformCP,
      _goSystemTimingCheck = uniformCP,
      _goTimingTransition = 0.25,
      _goTimingCheckNeg_Pos = 0.5,
      _goGenerateSingle_Block = 0.5,
      _goGenerateItem = uniformCP,
      _goModGenDeclaration = uniformCP,
      _goDimension_Initialisation = 0.5,
      _goAutomatic = 0.5,
      _goModGenItem = uniformCP,
      _goStatement = uniformCP,
      _goTypeAbstract_Concrete = 0.5,
      _goAbstractType = uniformCP,
      _goBlockDeclType = uniformCP,
      _goNetType = uniformCP,
      _goNet_Tri = 0.5,
      _goVectoring = uniformCP,
      _goRegister = 0.5,
      _goDirection = uniformCP,
      _goDriveStrength = uniformCP,
      _goChargeStrength = uniformCP,
      _goNInpGate = uniformCP,
      _goLoopStatement = uniformCP,
      _goCaseStatement = uniformCP,
      _goProcContAssign = uniformCP,
      _goPCAVar_Net = 0.5,
      _goGate = uniformCP,
      _goGateReverse = 0.5,
      _goGate1_0 = 0.5,
      _goDelayEvent = uniformCP,
      _goEvents = NPNegativeBinomial 0 0.5 1,
      _goEvent = uniformCP,
      _goEventPrefix = uniformCP,
      _goNamed_Positional = 0.5,
      _goBlockPar_Seq = 0.5,
      _goAssignmentBlocking = 0.5,
      _goCaseBranches = NPNegativeBinomial 0 0.25 1,
      _goLValues = NPNegativeBinomial 0 0.5 1,
      _goAttributes = NPLinearComb [(2, NPDiscrete [(1, 0)]), (1, NPNegativeBinomial 0 0.75 1)],
      _goPaths = NPNegativeBinomial 0 0.75 1,
      _goDelay = CPDiscrete [1, 1, 2, 4],
      _goMinTypMax = 0.5,
      _goRangeExpr = uniformCP,
      _goRangeOffsetPos_Neg = 0.5,
      _goDimensions = NPNegativeBinomial 0 0.5 1,
      _goSignedness = 0.5,
      _goExpression = CPDiscrete [2, 2, 2, 1],
      _goConcatenations = NPNegativeBinomial 0 (2.0 / 5.0) 1,
      _goUnaryOperation = uniformCP,
      _goBinaryOperation = uniformCP,
      _goPrimary = CPDiscrete [2, 4, 4, 4, 4, 4, 2, 4, 1, 1, 1, 1, 1],
      _goIntRealIdent = uniformCP,
      _goEscaped_Simple = 0.5,
      _goSimpleLetters = NPNegativeBinomial 0 0.125 1,
      _goSimpleLetter = uniformCP,
      _goEscapedLetters = NPNegativeBinomial 0 0.125 1,
      _goEscapedLetter = uniformCP,
      _goSystemLetters = NPNegativeBinomial 0 0.125 1,
      _goLiteralWidth =
        CPBiasedUniform
          [(1024, 1), (512, 8), (256, 16), (128, 32), (64, 64), (32, 128), (16, 256), (8, 512)]
          1,
      _goStringCharacters = NPNegativeBinomial 0 0.125 1,
      _goStringCharacter = uniformCP,
      _goFixed_Floating = 0.5,
      _goExponentSign = uniformCP,
      _goX_Z = 0.5,
      _goBinarySymbols = NPNegativeBinomial 0 0.125 1,
      _goBinarySymbol = uniformCP,
      _goOctalSymbols = NPNegativeBinomial 0 0.125 1,
      _goOctalSymbol = uniformCP,
      _goDecimalSymbols = NPNegativeBinomial 0 0.125 1,
      _goDecimalSymbol = uniformCP,
      _goHexadecimalSymbols = NPNegativeBinomial 0 0.125 1,
      _goHexadecimalSymbol = uniformCP
    }

data Info = Info
  { -- | @commit@: the hash of the commit that was compiled.
    _infoCommit :: !Text,
    -- | @version@: the version of Verismith that was compiled.
    _infoVersion :: !Text
  }
  deriving (Eq, Show)

-- | Description of the simulator
data SimDescription = SimDescription {simName :: {-# UNPACK #-} !Text}
  deriving (Eq, Show)

-- | @[[synthesiser]]@: description of the synthesis tool. There can be multiple of these sections in a config
-- file.
data SynthDescription = SynthDescription
  { -- | @name@: type of the synthesis tool. Can either be @yosys@, @quartus@,
    -- @quartuslight@, @vivado@, @xst@.
    synthName :: {-# UNPACK #-} !Text,
    -- | @bin@: location of the synthesis tool binary.
    synthBin :: Maybe Text,
    -- | @description@: description that should be used for the synthesis tool.
    synthDesc :: Maybe Text,
    -- | @output@: name of the output Verilog file.
    synthOut :: Maybe Text
  }
  deriving (Eq, Show)

data Config = Config
  { _configEMI :: {-# UNPACK #-} !ConfEMI,
    _configInfo :: {-# UNPACK #-} !Info,
    _configProbability :: {-# UNPACK #-} !Probability,
    _configProperty :: {-# UNPACK #-} !ConfProperty,
    _configGarbageGenerator :: {-# UNPACK #-} !GeneratorOpts,
    _configSimulators :: [SimDescription],
    _configSynthesisers :: [SynthDescription]
  }
  deriving (Eq, Show)

$(makeLenses ''ProbExpr)

$(makeLenses ''ProbModItem)

$(makeLenses ''ProbStatement)

$(makeLenses ''ProbMod)

$(makeLenses ''Probability)

$(makeLenses ''ConfEMI)

$(makeLenses ''ConfProperty)

$(makeLenses ''GeneratorOpts)

$(makeLenses ''Info)

$(makeLenses ''Config)

$(makePrisms ''CategoricalProbability)

$(makePrisms ''NumberProbability)

defaultValue :: a -> TomlCodec a -> TomlCodec a
defaultValue x = Toml.dimap Just (fromMaybe x) . Toml.dioptional

fromXST :: XST -> SynthDescription
fromXST (XST a b c) =
  SynthDescription "xst" (toTextIgnore <$> a) (Just b) (Just $ toTextIgnore c)

fromYosys :: Yosys -> SynthDescription
fromYosys (Yosys a b c) =
  SynthDescription
    "yosys"
    (toTextIgnore <$> a)
    (Just b)
    (Just $ toTextIgnore c)

fromVivado :: Vivado -> SynthDescription
fromVivado (Vivado a b c) =
  SynthDescription
    "vivado"
    (toTextIgnore <$> a)
    (Just b)
    (Just $ toTextIgnore c)

fromQuartus :: Quartus -> SynthDescription
fromQuartus (Quartus a b c) =
  SynthDescription
    "quartus"
    (toTextIgnore <$> a)
    (Just b)
    (Just $ toTextIgnore c)

fromQuartusLight :: QuartusLight -> SynthDescription
fromQuartusLight (QuartusLight a b c) =
  SynthDescription
    "quartuslight"
    (toTextIgnore <$> a)
    (Just b)
    (Just $ toTextIgnore c)

defaultConfig :: Config
defaultConfig =
  Config
    (ConfEMI 2 8)
    (Info (pack $(gitHash)) (pack $ showVersion version))
    (Probability defModItem defStmnt defExpr defMod)
    (ConfProperty 20 Nothing 3 2 5 "random" 10 False 0 1 Nothing)
    defGeneratorOpts
    []
    [fromYosys defaultYosys, fromVivado defaultVivado]
  where
    defMod =
      ProbMod
        0 -- Drop Output
        1 -- Keep Output
    defModItem =
      ProbModItem
        5 -- Assign
        1 -- Sequential Always
        1 -- Combinational Always
        1 -- Instantiation
    defStmnt =
      ProbStatement
        0 -- Blocking assignment
        3 -- Non-blocking assignment
        1 -- Conditional
        0 -- For loop
    defExpr =
      ProbExpr
        1 -- Number
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

catProbCodec :: TomlCodec CategoricalProbability
catProbCodec =
  Toml.dimatch (firstOf _CPDiscrete) CPDiscrete (Toml.arrayNonEmptyOf Toml._Double "Discrete")
    <|> Toml.table
      ( liftA2
          CPBiasedUniform
          ( defaultValue [] (Toml.list (Toml.pair (Toml.double "weight") (Toml.int "value")) "biases")
              .= _CPBUBiases
          )
          (Toml.double "weight" .= _CPBUUniformWeight)
      )
      "BiasedUniform"

numProbCodec :: TomlCodec NumberProbability
numProbCodec =
  Toml.dimatch
    (firstOf _NPUniform)
    (uncurry NPUniform)
    (Toml.table (Toml.pair (defaultValue 0 $ Toml.int "low") (Toml.int "high")) "Uniform")
    <|> Toml.dimatch
      (firstOf _NPBinomial)
      (uncurry3 NPBinomial)
      ( Toml.table
          ( Toml.triple
              (defaultValue 0 (Toml.int "offset"))
              (Toml.int "trials")
              (Toml.double "succesRate")
          )
          "Binomial"
      )
    <|> Toml.dimatch
      (firstOf _NPNegativeBinomial)
      (uncurry3 NPNegativeBinomial)
      ( Toml.table
          ( Toml.triple
              (defaultValue 0 (Toml.int "offset"))
              (Toml.double "failureRate")
              (Toml.int "numberOfFailures")
          )
          "NegativeBinomial"
      )
    <|> Toml.dimatch
      (firstOf _NPNegativeBinomial)
      (uncurry3 NPNegativeBinomial)
      ( Toml.table
          ( Toml.triple
              (defaultValue 0 (Toml.int "offset"))
              (Toml.double "failureRate")
              (pure 1)
          )
          "Geometric"
      )
    <|> Toml.dimatch
      (firstOf _NPPoisson)
      (uncurry NPPoisson)
      (Toml.table (Toml.pair (defaultValue 0 (Toml.int "offset")) (Toml.double "lambda")) "Poisson")
    <|> Toml.dimatch
      (firstOf _NPDiscrete)
      NPDiscrete
      (Toml.nonEmpty (Toml.pair (Toml.double "weight") (Toml.int "value")) "Discrete")
    <|> Toml.dimatch
      (firstOf _NPLinearComb)
      NPLinearComb
      (Toml.nonEmpty (Toml.pair (Toml.double "weight") numProbCodec) "LinearCombination")

exprCodec :: TomlCodec ProbExpr
exprCodec =
  ProbExpr
    <$> defaultValue (defProb probExprNum) (intE "number")
    .= _probExprNum
    <*> defaultValue (defProb probExprId) (intE "variable")
    .= _probExprId
    <*> defaultValue (defProb probExprRangeSelect) (intE "rangeselect")
    .= _probExprRangeSelect
    <*> defaultValue (defProb probExprUnOp) (intE "unary")
    .= _probExprUnOp
    <*> defaultValue (defProb probExprBinOp) (intE "binary")
    .= _probExprBinOp
    <*> defaultValue (defProb probExprCond) (intE "ternary")
    .= _probExprCond
    <*> defaultValue (defProb probExprConcat) (intE "concatenation")
    .= _probExprConcat
    <*> defaultValue (defProb probExprStr) (intE "string")
    .= _probExprStr
    <*> defaultValue (defProb probExprSigned) (intE "signed")
    .= _probExprSigned
    <*> defaultValue (defProb probExprUnsigned) (intE "unsigned")
    .= _probExprUnsigned
  where
    defProb i = defaultConfig ^. configProbability . probExpr . i
    intE = int "expr"

stmntCodec :: TomlCodec ProbStatement
stmntCodec =
  ProbStatement
    <$> defaultValue (defProb probStmntBlock) (intS "blocking")
    .= _probStmntBlock
    <*> defaultValue (defProb probStmntNonBlock) (intS "nonblocking")
    .= _probStmntNonBlock
    <*> defaultValue (defProb probStmntCond) (intS "conditional")
    .= _probStmntCond
    <*> defaultValue (defProb probStmntFor) (intS "forloop")
    .= _probStmntFor
  where
    defProb i = defaultConfig ^. configProbability . probStmnt . i
    intS = int "statement"

modItemCodec :: TomlCodec ProbModItem
modItemCodec =
  ProbModItem
    <$> defaultValue (defProb probModItemAssign) (intM "assign")
    .= _probModItemAssign
    <*> defaultValue (defProb probModItemSeqAlways) (intM "sequential")
    .= _probModItemSeqAlways
    <*> defaultValue (defProb probModItemCombAlways) (intM "combinational")
    .= _probModItemCombAlways
    <*> defaultValue (defProb probModItemInst) (intM "instantiation")
    .= _probModItemInst
  where
    defProb i = defaultConfig ^. configProbability . probModItem . i
    intM = int "moditem"

modCodec :: TomlCodec ProbMod
modCodec =
  ProbMod
    <$> defaultValue (defProb probModDropOutput) (intM "drop_output")
    .= _probModDropOutput
    <*> defaultValue (defProb probModKeepOutput) (intM "keep_output")
    .= _probModKeepOutput
  where
    defProb i = defaultConfig ^. configProbability . probMod . i
    intM = int "module"

probCodec :: TomlCodec Probability
probCodec =
  Probability
    <$> defaultValue (defProb probModItem) modItemCodec
    .= _probModItem
    <*> defaultValue (defProb probStmnt) stmntCodec
    .= _probStmnt
    <*> defaultValue (defProb probExpr) exprCodec
    .= _probExpr
    <*> defaultValue (defProb probMod) modCodec
    .= _probMod
  where
    defProb i = defaultConfig ^. configProbability . i

propCodec :: TomlCodec ConfProperty
propCodec =
  ConfProperty
    <$> defaultValue (defProp propSize) (Toml.int "size")
    .= _propSize
    <*> Toml.dioptional (Toml.read "seed")
    .= _propSeed
    <*> defaultValue (defProp propStmntDepth) (int "statement" "depth")
    .= _propStmntDepth
    <*> defaultValue (defProp propModDepth) (int "module" "depth")
    .= _propModDepth
    <*> defaultValue (defProp propMaxModules) (int "module" "max")
    .= _propMaxModules
    <*> defaultValue
      (defProp propSampleMethod)
      (Toml.text (twoKey "sample" "method"))
    .= _propSampleMethod
    <*> defaultValue (defProp propSampleSize) (int "sample" "size")
    .= _propSampleSize
    <*> defaultValue
      (defProp propCombine)
      (Toml.bool (twoKey "output" "combine"))
    .= _propCombine
    <*> defaultValue (defProp propNonDeterminism) (Toml.int "nondeterminism")
    .= _propNonDeterminism
    <*> defaultValue (defProp propDeterminism) (Toml.int "determinism")
    .= _propDeterminism
    <*> Toml.dioptional (Toml.text (twoKey "default" "yosys"))
    .= _propDefaultYosys
  where
    defProp i = defaultConfig ^. configProperty . i

garbageCodec :: TomlCodec GeneratorOpts
garbageCodec =
  -- TODO HERE: hierarchise elements (`/".* "`)
  GeneratorOpts
    <$> Toml.dioptional (Toml.read "seed") .= _goSeed
    <*> pure 1
    <*> pure 1
    <*> pure 1
    <*> dfield _goExprRecAttenuation "expr.recAttenuation"
    <*> dfield _goAttribRecAttenuation "attribute.recAttenuation"
    <*> dfield _goStmtRecAttenuation "statement.recAttenuation"
    <*> dfield _goOptionalElement "optionalElement"
    <*> tfield _goConfigs numProbCodec "config.count"
    <*> tfield _goConfigItems numProbCodec "config.items"
    <*> tfield _goDesigns numProbCodec "config.designs"
    <*> dfield _goCell_Inst "config.cell_inst"
    <*> dfield _goLiblist_Use "config.liblist_use"
    <*> tfield _goPrimitives numProbCodec "primitive.count"
    <*> dfield _goSequential_Combinatorial "primitive.seq_comb"
    <*> tfield _goTableRows numProbCodec "primitive.table.row.count"
    <*> dfield _goPortInitialisation "primitive.portInitialisation"
    <*> tfield _goPrimitiveInitialisation catProbCodec "primitive.initialisation"
    <*> dfield _goEdgeSensitiveRow "primitive.table.row.edgeSensitive"
    <*> tfield _goTableInLevel catProbCodec "primitive.table.inLevel"
    <*> tfield _goTableOutLevel catProbCodec "primitive.table.outLevel"
    <*> dfield _goEdgeSimple "primitive.table.edge.simple"
    <*> dfield _goEdgePos_Neg "primitive.table.edge.pos_neg"
    <*> tfield _goModules numProbCodec "module.count"
    <*> tfield _goParameters numProbCodec "parameters"
    <*> tfield _goLocalParameters numProbCodec "localParameters"
    <*> tfield _goParameterOverrides numProbCodec "parameterOverrides"
    <*> tfield _goDeclarations numProbCodec "declarations"
    <*> tfield _goOtherItems numProbCodec "otherItems"
    <*> tfield _goArguments numProbCodec "arguments"
    <*> tfield _goModuleItems numProbCodec "module.item.count"
    <*> tfield _goModuleItem catProbCodec "module.item.distrib"
    <*> tfield _goPortType catProbCodec "module.port"
    <*> dfield _goModuleCell "module.cell"
    <*> tfield _goUnconnectedDrive catProbCodec "module.unconnectedDrive"
    <*> tfield _goTimeMagnitude catProbCodec "module.timeMagnitude"
    <*> tfield _goSpecifyItems numProbCodec "specify.items"
    <*> tfield _goSpecifyItem catProbCodec "specify.item"
    <*> tfield _goSpecifyParameters numProbCodec "specify.parameters"
    <*> dfield _goInitialisation_PathPulse "specify.initialisation_pathPulse"
    <*> tfield _goModulePathCondition catProbCodec "specify.path.condition"
    <*> tfield _goPathDelayCount catProbCodec "specify.path.delayCount"
    <*> dfield _goPathFull_Parallel "specify.path.full_parallel"
    <*> tfield _goFullPathTerms numProbCodec "specify.path.fullTerms"
    <*> dfield _goPulseEvent_Detect "specify.pulsestyleEvent_detect"
    <*> dfield _goShowCancelled "specify.show_cancelled"
    <*> tfield _goPolarity catProbCodec "specify.path.polarity"
    <*> tfield _goEdgeSensitivity catProbCodec "specify.path.edgeSensitivity"
    <*> tfield _goSystemTimingCheck catProbCodec "specify.STC.distrib"
    <*> dfield _goTimingTransition "specify.STC.edge"
    <*> dfield _goTimingCheckNeg_Pos "specify.STC.neg_pos"
    <*> dfield _goGenerateSingle_Block "generate.single_block"
    <*> tfield _goGenerateItem catProbCodec "generate.item"
    <*> tfield _goModGenItem catProbCodec "modgen.item.distrib"
    <*> tfield _goModGenDeclaration catProbCodec "modgen.declaration.distrib"
    <*> dfield _goDimension_Initialisation "modgen.declaration.dim_init"
    <*> dfield _goAutomatic "automatic"
    <*> tfield _goStatement catProbCodec "statement.distrib"
    <*> dfield _goTypeAbstract_Concrete "type.abstract_concrete"
    <*> tfield _goAbstractType catProbCodec "type.abstract"
    <*> tfield _goBlockDeclType catProbCodec "type.block"
    <*> tfield _goNetType catProbCodec "type.net.distrib"
    <*> dfield _goNet_Tri "type.net.net_tri"
    <*> tfield _goVectoring catProbCodec "type.net.vectoring"
    <*> dfield _goRegister "type.taskfun.register"
    <*> tfield _goDirection catProbCodec "type.taskfun.direction"
    <*> tfield _goDriveStrength catProbCodec "driveStrength"
    <*> tfield _goChargeStrength catProbCodec "chargeStrength"
    <*> tfield _goNInpGate catProbCodec "modgen.item.gate.nInputGate"
    <*> tfield _goLoopStatement catProbCodec "statement.loop"
    <*> tfield _goCaseStatement catProbCodec "statement.case"
    <*> tfield _goProcContAssign catProbCodec "statement.PCA.distrib"
    <*> dfield _goPCAVar_Net "statement.PCA.var_net "
    <*> tfield _goGate catProbCodec "modgen.item.gate.distrib"
    <*> dfield _goGateReverse "modgen.item.gate.reverse"
    <*> dfield _goGate1_0 "modgen.item.gate.1_0"
    <*> tfield _goDelayEvent catProbCodec "statement.delayEvent"
    <*> tfield _goEvents numProbCodec "statement.event.count"
    <*> tfield _goEvent catProbCodec "statement.event.distrib"
    <*> tfield _goEventPrefix catProbCodec "statement.event.prefix"
    <*> dfield _goNamed_Positional "modgen.item.modinstNamed_positional"
    <*> dfield _goBlockPar_Seq "statement.block.par_seq"
    <*> dfield _goAssignmentBlocking "statement.assignment.blocking"
    <*> tfield _goCaseBranches numProbCodec "caseBranches"
    <*> tfield _goLValues numProbCodec "lvalues"
    <*> tfield _goAttributes numProbCodec "attribute.count"
    <*> tfield _goPaths numProbCodec "paths"
    <*> tfield _goDelay catProbCodec "delay.length"
    <*> dfield _goMinTypMax "expr.Mtm_single"
    <*> tfield _goRangeExpr catProbCodec "expr.range.distrib"
    <*> dfield _goRangeOffsetPos_Neg "expr.range.offsetPos_Neg "
    <*> tfield _goDimensions numProbCodec "dimensions"
    <*> dfield _goSignedness "signedness"
    <*> tfield _goExpression catProbCodec "expr.distrib"
    <*> tfield _goConcatenations numProbCodec "expr.concatLength"
    <*> tfield _goUnaryOperation catProbCodec "expr.unary"
    <*> tfield _goBinaryOperation catProbCodec "expr.binary"
    <*> tfield _goPrimary catProbCodec "expr.primary.distrib"
    <*> tfield _goIntRealIdent catProbCodec "delay.base"
    <*> dfield _goEscaped_Simple "ident.escaped_simple"
    <*> tfield _goSimpleLetters numProbCodec "ident.simple.length"
    <*> tfield _goSimpleLetter catProbCodec "ident.simple.character"
    <*> tfield _goEscapedLetters numProbCodec "ident.escaped.length"
    <*> tfield _goEscapedLetter catProbCodec "ident.escaped.character"
    <*> tfield _goSystemLetters numProbCodec "ident.system.length"
    <*> tfield _goLiteralWidth catProbCodec "expr.primary.width"
    <*> tfield _goStringCharacters numProbCodec "expr.primary.string.length"
    <*> tfield _goStringCharacter catProbCodec "expr.primary.string.character"
    <*> dfield _goFixed_Floating "expr.primary.real.fixed_floating"
    <*> tfield _goExponentSign catProbCodec "expr.primary.real.exponentSign"
    <*> dfield _goX_Z "expr.primary.X_Z"
    <*> tfield _goBinarySymbols numProbCodec "expr.primary.binary.length"
    <*> tfield _goBinarySymbol catProbCodec "expr.primary.binary.digit"
    <*> tfield _goOctalSymbols numProbCodec "expr.primary.octal.length"
    <*> tfield _goOctalSymbol catProbCodec "expr.primary.octal.digit"
    <*> tfield _goDecimalSymbols numProbCodec "expr.primary.decimal.length"
    <*> tfield _goDecimalSymbol catProbCodec "expr.primary.decimal.digit"
    <*> tfield _goHexadecimalSymbols numProbCodec "expr.primary.hexadecimal.length"
    <*> tfield _goHexadecimalSymbol catProbCodec "expr.primary.hexadecimal.digit"
  where
    tfield p c n = defaultValue (p $ _configGarbageGenerator defaultConfig) (Toml.table c n) .= p
    dfield p n = defaultValue (p $ _configGarbageGenerator defaultConfig) (Toml.double n) .= p

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
    .= synthName
    <*> Toml.dioptional (Toml.text "bin")
    .= synthBin
    <*> Toml.dioptional (Toml.text "description")
    .= synthDesc
    <*> Toml.dioptional (Toml.text "output")
    .= synthOut

emiCodec :: TomlCodec ConfEMI
emiCodec =
  ConfEMI
    <$> defaultValue
      (defaultConfig ^. configEMI . confEMIGenerateProb)
      (Toml.int "generate_prob")
    .= _confEMIGenerateProb
    <*> defaultValue
      (defaultConfig ^. configEMI . confEMINoGenerateProb)
      (Toml.int "nogenerate_prob")
    .= _confEMINoGenerateProb

infoCodec :: TomlCodec Info
infoCodec =
  Info
    <$> defaultValue
      (defaultConfig ^. configInfo . infoCommit)
      (Toml.text "commit")
    .= _infoCommit
    <*> defaultValue
      (defaultConfig ^. configInfo . infoVersion)
      (Toml.text "version")
    .= _infoVersion

configCodec :: TomlCodec Config
configCodec =
  Config
    <$> defaultValue
      (defaultConfig ^. configEMI)
      (Toml.table emiCodec "emi")
    .= _configEMI
    <*> defaultValue
      (defaultConfig ^. configInfo)
      (Toml.table infoCodec "info")
    .= _configInfo
    <*> defaultValue
      (defaultConfig ^. configProbability)
      (Toml.table probCodec "probability")
    .= _configProbability
    <*> defaultValue
      (defaultConfig ^. configProperty)
      (Toml.table propCodec "property")
    .= _configProperty
    <*> defaultValue
      (defaultConfig ^. configGarbageGenerator)
      (Toml.table garbageCodec "invalid_generator")
    .= _configGarbageGenerator
    <*> defaultValue
      (defaultConfig ^. configSimulators)
      (Toml.list simulator "simulator")
    .= _configSimulators
    <*> defaultValue
      (defaultConfig ^. configSynthesisers)
      (Toml.list synthesiser "synthesiser")
    .= _configSynthesisers

parseConfigFile :: FilePath -> IO (Either Text Config)
parseConfigFile fp = do
  decoded <- Toml.decodeFileExact configCodec fp
  case decoded of
    Right c -> return $ Right c
    Left e -> return . Left $ Toml.prettyTomlDecodeErrors e

parseConfig :: Text -> Either Text Config
parseConfig t = case Toml.decodeExact configCodec t of
  Right c -> Right c
  Left e -> Left $ Toml.prettyTomlDecodeErrors e

parseConfigFileRelaxed :: FilePath -> IO (Either Text Config)
parseConfigFileRelaxed fp = do
  decoded <- Toml.decodeFileEither configCodec fp
  case decoded of
    Right c -> return $ Right c
    Left e -> return . Left $ Toml.prettyTomlDecodeErrors e

parseConfigRelaxed :: Text -> Either Text Config
parseConfigRelaxed t = case Toml.decode configCodec t of
  Right c -> Right c
  Left e -> Left $ Toml.prettyTomlDecodeErrors e

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
