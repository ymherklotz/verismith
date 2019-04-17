{-|
Module      : VeriFuzz.Config
Description : Configuration file format and parser.
Copyright   : (c) 2019, Yann Herklotz
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Configuration file format and parser.
-}

{-# LANGUAGE TemplateHaskell #-}

module VeriFuzz.Config
    ( Config(..)
    , defaultConfig
    , configProbability
    , configProperty
    , Probability(..)
    , probModItem
    , probStmnt
    , probExpr
    , ProbExpr(..)
    , probExprNum
    , probExprId
    , probExprUnOp
    , probExprBinOp
    , probExprCond
    , probExprConcat
    , probExprStr
    , probExprSigned
    , probExprUnsigned
    , ProbModItem(..)
    , probModItemAssign
    , probModItemAlways
    , probModItemInst
    , ProbStatement(..)
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
    , configEncode
    , configToFile
    )
where

import           Control.Applicative (Alternative)
import           Control.Lens        hiding ((.=))
import           Data.List.NonEmpty  (NonEmpty (..))
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text.IO        as T (writeFile)
import           Toml                (TomlCodec, (.=))
import qualified Toml

data ProbExpr = ProbExpr { _probExprNum      :: {-# UNPACK #-} !Int
                         , _probExprId       :: {-# UNPACK #-} !Int
                         , _probExprUnOp     :: {-# UNPACK #-} !Int
                         , _probExprBinOp    :: {-# UNPACK #-} !Int
                         , _probExprCond     :: {-# UNPACK #-} !Int
                         , _probExprConcat   :: {-# UNPACK #-} !Int
                         , _probExprStr      :: {-# UNPACK #-} !Int
                         , _probExprSigned   :: {-# UNPACK #-} !Int
                         , _probExprUnsigned :: {-# UNPACK #-} !Int
                         }
              deriving (Eq, Show)

data ProbModItem = ProbModItem { _probModItemAssign :: {-# UNPACK #-} !Int
                               , _probModItemAlways :: {-# UNPACK #-} !Int
                               , _probModItemInst   :: {-# UNPACK #-} !Int
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

data Property = Property { _propSize       :: {-# UNPACK #-} !Int
                         , _propSeed       :: !(Maybe Int)
                         , _propStmntDepth :: {-# UNPACK #-} !Int
                         , _propModDepth   :: {-# UNPACK #-} !Int
                         , _propMaxModules :: {-# UNPACK #-} !Int
                         }
              deriving (Eq, Show)

data Config = Config { _configProbability :: {-# UNPACK #-} !Probability
                     , _configProperty    :: {-# UNPACK #-} !Property
                     }
            deriving (Eq, Show)

makeLenses ''ProbExpr
makeLenses ''ProbModItem
makeLenses ''ProbStatement
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
defaultConfig = Config (Probability defModItem defStmnt defExpr)
                       (Property 20 Nothing 3 2 5)
  where
    defModItem = ProbModItem 5 1 1
    defStmnt   = ProbStatement 0 15 1 1
    defExpr    = ProbExpr 1 1 1 1 1 1 0 1 1

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

configCodec :: TomlCodec Config
configCodec =
    Config
        <$> defaultValue (defaultConfig ^. configProbability)
                         (Toml.table probCodec "probability")
        .=  _configProbability
        <*> defaultValue (defaultConfig ^. configProperty)
                         (Toml.table propCodec "property")
        .=  _configProperty

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
    Left (Toml.ParseError _) -> error "Config file parse error"

configEncode :: Config -> Text
configEncode = Toml.encode configCodec

configToFile :: FilePath -> Config -> IO ()
configToFile f = T.writeFile f . configEncode
