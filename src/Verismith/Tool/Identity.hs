-- |
-- Module      : Verismith.Tool.Identity
-- Description : The identity simulator and synthesiser.
-- Copyright   : (c) 2019, Yann Herklotz Grave
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- The identity simulator and synthesiser.
module Verismith.Tool.Identity
  ( Identity (..),
    defaultIdentity,
  )
where

import Control.DeepSeq (NFData, rnf, rwhnf)
import Data.Text (Text, unpack)
import Shelly (FilePath)
import Shelly.Lifted (writefile)
import Verismith.Tool.Internal
import Verismith.Verilog.AST
import Verismith.Verilog.CodeGen
import Prelude hiding (FilePath)

data Identity = Identity
  { identityDesc :: !Text,
    identityOutput :: !FilePath
  }
  deriving (Eq)

instance Tool Identity where
  toText (Identity d _) = d

instance Show Identity where
  show t = unpack $ toText t

instance Synthesiser Identity where
  runSynth = runSynthIdentity
  synthOutput = identityOutput
  setSynthOutput (Identity a _) = Identity a

instance NFData Identity where
  rnf = rwhnf

runSynthIdentity :: (Show ann) => Identity -> (SourceInfo ann) -> ResultSh ()
runSynthIdentity (Identity _ out) = writefile out . genSource

defaultIdentity :: Identity
defaultIdentity = Identity "identity" "syn_identity.v"
