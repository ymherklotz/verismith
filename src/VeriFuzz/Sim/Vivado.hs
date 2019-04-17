{-|
Module      : VeriFuzz.Sim.Vivado
Description : Vivado Synthesiser implementation.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Vivado Synthesiser implementation.
-}

module VeriFuzz.Sim.Vivado
    ( Vivado(..)
    , defaultVivado
    )
where

import           Prelude                  hiding (FilePath)
import           Shelly
import           Shelly.Lifted            (liftSh)
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Sim.Template
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.CodeGen

newtype Vivado = Vivado { vivadoPath :: FilePath }
               deriving (Eq, Show)

instance Tool Vivado where
    toText _ = "vivado"

instance Synthesiser Vivado where
    runSynth = runSynthVivado

defaultVivado :: Vivado
defaultVivado = Vivado "vivado"

runSynthVivado :: Vivado -> SourceInfo -> FilePath -> ResultSh ()
runSynthVivado sim (SourceInfo top src) outf = do
    dir <- liftSh pwd
    liftSh $ do
        writefile vivadoTcl . vivadoSynthConfig top $ toTextIgnore outf
        writefile "rtl.v" $ genSource src
        run_ "sed" ["s/^module/(* use_dsp48=\"no\" *) module/;", "-i", "rtl.v"]
        echoP "Vivado: run"
    execute_ SynthFail
             dir
             "vivado"
             (vivadoPath sim)
             ["-mode", "batch", "-source", toTextIgnore vivadoTcl]
    liftSh $ echoP "Vivado: done"
    where vivadoTcl = fromText ("vivado_" <> top) <.> "tcl"
