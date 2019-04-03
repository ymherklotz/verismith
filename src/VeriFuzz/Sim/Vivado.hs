{-|
Module      : VeriFuzz.Sim.Vivado
Description : Vivado Synthesisor implementation.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Vivado Synthesisor implementation.
-}

module VeriFuzz.Sim.Vivado
    ( Vivado(..)
    , defaultVivado
    )
where

import           Prelude                  hiding (FilePath)
import           Shelly
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Sim.Template
import           VeriFuzz.Verilog.CodeGen

newtype Vivado = Vivado { vivadoPath :: FilePath }
               deriving (Eq, Show)

instance Tool Vivado where
    toText _ = "vivado"

instance Synthesisor Vivado where
    runSynth = runSynthVivado

defaultVivado :: Vivado
defaultVivado = Vivado "vivado"

runSynthVivado :: Vivado -> SourceInfo -> FilePath -> Sh ()
runSynthVivado sim (SourceInfo top src) outf = do
    dir <- pwd
    writefile vivadoTcl . vivadoSynthConfig top $ toTextIgnore outf
    writefile "rtl.v" $ genSource src
    echoP "Vivado: run"
    logger_ dir "vivado"
        $ timeout
              (vivadoPath sim)
              ["-mode", "batch", "-source", toTextIgnore vivadoTcl]
    echoP "Vivado: done"
    where vivadoTcl = "vivado_" <> fromText top <.> "tcl"
