{-|
Module      : VeriFuzz.Context
Description : Context types used in the generation.
Copyright   : (c) 2019, Yann Herklotz
License     : GPL-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

Context types used in the generation of the Verilog.
-}

{-# LANGUAGE TemplateHaskell #-}

module VeriFuzz.Context
    ( -- * Context types
      Context (..)
    , variables
    , parameters
    , modules
    , nameCounter
    , stmntDepth
    , modDepth
    , determinism
    , VarContext (..)
    , ParamContext (..)
    , ModuleContext (..)
    , PortContext (..)
    , portContPort
    , portContND
    )
where

import           Control.Lens      (makeLenses)
import           Data.HashMap.Lazy (HashMap)
import           VeriFuzz.Verilog

data PortContext = PortContext { _portContPort :: !Port
                               , _portContND   :: !Bool
                               }

$(makeLenses ''PortContext)

type VarContext = HashMap Identifier PortContext

type ParamContext = HashMap Identifier Parameter

type ModuleContext = HashMap Identifier ModDecl

data Context = Context { _variables   :: VarContext
                       , _parameters  :: ParamContext
                       , _modules     :: ModuleContext
                       , _nameCounter :: {-# UNPACK #-} !Int
                       , _stmntDepth  :: {-# UNPACK #-} !Int
                       , _modDepth    :: {-# UNPACK #-} !Int
                       , _determinism :: !Bool
                       }

$(makeLenses ''Context)
