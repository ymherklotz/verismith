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

import           Data.Text                (Text, unpack)
import           Prelude                  hiding (FilePath)
import           Shelly
import           Shelly.Lifted            (liftSh)
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.CodeGen

data Quartus = Quartus { quartusBin    :: !(Maybe FilePath)
                       , quartusDesc   :: {-# UNPACK #-} !Text
                       , quartusOutput :: {-# UNPACK #-} !FilePath
                       }
                deriving (Eq)

instance Tool Quartus where
    toText (Quartus _ t _) = t

instance Show Quartus where
    show t = unpack $ toText t

instance Synthesiser Quartus where
    runSynth = runSynthQuartus
    synthOutput = quartusOutput
    setSynthOutput (Quartus a b _) = Quartus a b

defaultQuartus :: Quartus
defaultQuartus = Quartus Nothing "quartus" "syn_quartus.v"

runSynthQuartus :: Quartus -> SourceInfo -> ResultSh ()
runSynthQuartus sim (SourceInfo top src) = do
    dir <- liftSh pwd
    let ex = execute_ SynthFail dir "quartus"
    liftSh $ do
        writefile inpf $ genSource src
        logger "Running Quartus synthesis"
    ex (exec "quartus_map")
       [top, "--source=" <> toTextIgnore inpf, "--family=Cyclone V"]
    ex (exec "quartus_fit") [top, "--part=5CGXFC7D6F31C6"]
    ex (exec "quartus_eda") [top, "--simulation", "--tool=vcs"]
    liftSh $ do
        cp (fromText "simulation/vcs" </> fromText top <.> "vo")
            $ synthOutput sim
        run_
            "sed"
            [ "-ri"
            , "s,^// DATE.*,,; s,^tri1 (.*);,wire \\1 = 1;,; /^\\/\\/ +synopsys/ d;"
            , toTextIgnore $ synthOutput sim
            ]
        logger "Quartus synthesis done"
  where
    inpf = "rtl.v"
    exec s = maybe (fromText s) (</> fromText s) $ quartusBin sim
