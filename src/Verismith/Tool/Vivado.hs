{-|
Module      : Verismith.Tool.Vivado
Description : Vivado Synthesiser implementation.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

Vivado Synthesiser implementation.
-}

module Verismith.Tool.Vivado
    ( Vivado(..)
    , defaultVivado
    )
where

import           Control.DeepSeq           (NFData, rnf, rwhnf)
import           Data.Text                 (Text, unpack)
import           Prelude                   hiding (FilePath)
import           Shelly
import           Shelly.Lifted             (liftSh)
import           Verismith.Tool.Internal
import           Verismith.Tool.Template
import           Verismith.Verilog.AST
import           Verismith.Verilog.CodeGen

data Vivado = Vivado { vivadoBin    :: !(Maybe FilePath)
                     , vivadoDesc   :: !Text
                     , vivadoOutput :: !FilePath
                     }
               deriving (Eq)

instance Tool Vivado where
    toText (Vivado _ t _) = t

instance Show Vivado where
    show t = unpack $ toText t

instance Synthesiser Vivado where
    runSynth = runSynthVivado
    synthOutput = vivadoOutput
    setSynthOutput (Vivado a b _) = Vivado a b

instance NFData Vivado where
    rnf = rwhnf

defaultVivado :: Vivado
defaultVivado = Vivado Nothing "vivado" "syn_vivado.v"

runSynthVivado :: Show ann => Vivado -> (SourceInfo ann) -> ResultSh ()
runSynthVivado sim (SourceInfo top src) = do
    dir <- liftSh pwd
    liftSh $ do
        writefile vivadoTcl . vivadoSynthConfig top . toTextIgnore $ synthOutput
            sim
        writefile "rtl.v" $ genSource src
        run_
            "sed"
            [ "s/^module/(* use_dsp48=\"no\" *) (* use_dsp=\"no\" *) module/;"
            , "-i"
            , "rtl.v"
            ]
    let exec_ n = execute_
            SynthFail
            dir
            "vivado"
            (maybe (fromText n) (</> fromText n) $ vivadoBin sim)
    exec_ "vivado" ["-mode", "batch", "-source", toTextIgnore vivadoTcl]
    where vivadoTcl = fromText ("vivado_" <> top) <.> "tcl"
