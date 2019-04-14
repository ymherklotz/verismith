{-|
Module      : VeriFuzz.Sim.Quartus
Description : Quartus synthesiser implementation.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Quartus synthesiser implementation.
-}

module VeriFuzz.Sim.Quartus
    ( Quartus(..)
    , defaultQuartus
    )
where

import           Prelude                  hiding (FilePath)
import           Shelly
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Verilog.CodeGen

newtype Quartus = Quartus { quartusBin :: Maybe FilePath }
                deriving (Eq, Show)

instance Tool Quartus where
    toText _ = "quartus"

instance Synthesisor Quartus where
    runSynth = runSynthQuartus

defaultQuartus :: Quartus
defaultQuartus = Quartus Nothing

runSynthQuartus :: Quartus -> SourceInfo -> FilePath -> Sh ()
runSynthQuartus sim (SourceInfo top src) outf = do
    dir <- pwd
    writefile inpf $ genSource src
    echoP "Running Quartus synthesis"
    logger_ dir "quartus" $ timeout
        (exec "quartus_map")
        [top, "--source=" <> toTextIgnore inpf, "--family=Cyclone V"]
    logger_ dir "quartus"
        $ timeout (exec "quartus_fit") [top, "--part=5CGXFC7D6F27C6"]
    logger_ dir "quartus"
        $ timeout (exec "quartus_eda") [top, "--simulation", "--tool=vcs"] -- --formal_verification --tool=conformal
    cp (fromText "simulation/vcs" </> fromText top <.> "vo") outf
    echoP "Quartus synthesis done"
  where
    inpf = "rtl.v"
    exec s = maybe (fromText s) (</> fromText s) $ quartusBin sim
