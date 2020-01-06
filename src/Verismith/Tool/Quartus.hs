{-|
Module      : Verismith.Tool.Quartus
Description : Quartus synthesiser implementation.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPLv3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

Quartus synthesiser implementation.
-}

module Verismith.Tool.Quartus
    ( Quartus(..)
    , defaultQuartus
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

instance NFData Quartus where
    rnf = rwhnf

defaultQuartus :: Quartus
defaultQuartus = Quartus Nothing "quartus" "syn_quartus.v"

runSynthQuartus :: Quartus -> SourceInfo -> ResultSh ()
runSynthQuartus sim (SourceInfo top src) = do
    dir <- liftSh pwd
    let ex = execute_ SynthFail dir "quartus"
    liftSh $ do
        writefile inpf $ genSource src
        noPrint $ run_ "sed" [ "-i"
                             , "s/^module/(* multstyle = \"logic\" *) module/;"
                             , toTextIgnore inpf
                             ]
        writefile quartusSdc $ "create_clock -period 5 -name clk [get_ports clock]"
        writefile quartusTcl $ quartusSynthConfig sim quartusSdc top inpf
    ex (exec "quartus_sh") ["-t", toTextIgnore quartusTcl]
    liftSh $ do
        cp (fromText "simulation/vcs" </> fromText top <.> "vo")
            $ synthOutput sim
        run_
            "sed"
            [ "-ri"
            , "s,^// DATE.*,,; s,^tri1 (.*);,wire \\1 = 1;,; /^\\/\\/ +synopsys/ d;"
            , toTextIgnore $ synthOutput sim
            ]
  where
    inpf = "rtl.v"
    exec s = maybe (fromText s) (</> fromText s) $ quartusBin sim
    quartusTcl = fromText top <.> "tcl"
    quartusSdc = fromText top <.> "sdc"
