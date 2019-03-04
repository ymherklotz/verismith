{-|
Module      : VeriFuzz.Config
Description : Configuration file format and parser.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Configuration file format and parser.
-}

{-# LANGUAGE TemplateHaskell #-}

module VeriFuzz.Config
    ( Config(..)
    , Probability(..)
    , probAssign
    , probAlways
    , configProbability
    , parseConfigFile
    )
where

import           Control.Applicative (Alternative)
import           Control.Lens        hiding ((.=))
import           Data.Maybe          (fromMaybe)
import           Toml                (TomlCodec, (.=))
import qualified Toml

data Probability = Probability { _probAssign :: {-# UNPACK #-} !Int
                               , _probAlways :: {-# UNPACK #-} !Int
                               }
                 deriving (Eq, Show)

makeLenses ''Probability

newtype Config = Config { _configProbability :: Probability }
               deriving (Eq, Show)

makeLenses ''Config

defaultValue
  :: (Alternative r, Applicative w) =>
     b -> Toml.Codec r w a b -> Toml.Codec r w a b
defaultValue x = Toml.dimap Just (fromMaybe x) . Toml.dioptional

probCodec :: TomlCodec Probability
probCodec = Probability
    <$> Toml.int "assign" .= _probAssign
    <*> defaultValue 1 (Toml.int "always") .= _probAlways

configCodec :: TomlCodec Config
configCodec = Toml.dimap _configProbability Config $ Toml.table probCodec "probability"

parseConfigFile :: FilePath -> IO Config
parseConfigFile = Toml.decodeFile configCodec
