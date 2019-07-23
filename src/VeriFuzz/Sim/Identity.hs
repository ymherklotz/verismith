{-|
Module      : VeriFuzz.Sim.Identity
Description : The identity simulator and synthesiser.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

The identity simulator and synthesiser.
-}

module VeriFuzz.Sim.Identity
    ( Identity(..)
    , defaultIdentity
    )
where

import           Control.DeepSeq          (NFData, rnf, rwhnf)
import           Data.Text                (Text, unpack)
import           Prelude                  hiding (FilePath)
import           Shelly                   (FilePath)
import           Shelly.Lifted            (writefile)
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.CodeGen

data Identity = Identity { identityDesc   :: {-# UNPACK #-} !Text
                         , identityOutput :: {-# UNPACK #-} !FilePath
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

runSynthIdentity :: Identity -> SourceInfo -> ResultSh ()
runSynthIdentity (Identity _ out) = writefile out . genSource

defaultIdentity :: Identity
defaultIdentity = Identity "identity" "syn_identity.v"
