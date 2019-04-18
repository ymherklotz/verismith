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
import           Shelly.Lifted            (liftSh)
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.CodeGen

data Quartus = Quartus { quartusBin    :: !(Maybe FilePath)
                       , quartusOutput :: {-# UNPACK #-} !FilePath
                       }
                deriving (Eq)

instance Show Quartus where
    show _ = "quartus"

instance Tool Quartus where
    toText _ = "quartus"

instance Synthesiser Quartus where
    runSynth = runSynthQuartus
    synthOutput = quartusOutput
    setSynthOutput (Quartus a _) f = Quartus a f

defaultQuartus :: Quartus
defaultQuartus = Quartus Nothing "quartus/syn_quartus.v"

runSynthQuartus :: Quartus -> SourceInfo -> FilePath -> ResultSh ()
runSynthQuartus sim (SourceInfo top src) outf = do
    dir <- liftSh pwd
    let ex = execute_ SynthFail dir "quartus"
    liftSh $ do
        writefile inpf $ genSource src
        echoP "Running Quartus synthesis"
    ex (exec "quartus_map")
       [top, "--source=" <> toTextIgnore inpf, "--family=Cyclone V"]
    ex (exec "quartus_fit") [top, "--part=5CGXFC7D6F31C6"]
    ex (exec "quartus_eda") [top, "--simulation", "--tool=vcs"]
    liftSh $ do
        cp (fromText "simulation/vcs" </> fromText top <.> "vo") outf
        echoP "Quartus synthesis done"
  where
    inpf = "rtl.v"
    exec s = maybe (fromText s) (</> fromText s) $ quartusBin sim
