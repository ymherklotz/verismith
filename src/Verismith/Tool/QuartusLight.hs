{-|
Module      : Verismith.Tool.QuartusLight
Description : QuartusLight synthesiser implementation.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

QuartusLight synthesiser implementation.
-}

module Verismith.Tool.QuartusLight
    ( QuartusLight(..)
    , defaultQuartusLight
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

data QuartusLight = QuartusLight { quartusLightBin    :: !(Maybe FilePath)
                                 , quartusLightDesc   :: {-# UNPACK #-} !Text
                                 , quartusLightOutput :: {-# UNPACK #-} !FilePath
                                 }
                  deriving (Eq)

instance Tool QuartusLight where
    toText (QuartusLight _ t _) = t

instance Show QuartusLight where
    show t = unpack $ toText t

instance Synthesiser QuartusLight where
    runSynth = runSynthQuartusLight
    synthOutput = quartusLightOutput
    setSynthOutput (QuartusLight a b _) = QuartusLight a b

instance NFData QuartusLight where
    rnf = rwhnf

defaultQuartusLight :: QuartusLight
defaultQuartusLight = QuartusLight Nothing "quartus" "syn_quartus.v"

runSynthQuartusLight :: QuartusLight -> SourceInfo -> ResultSh ()
runSynthQuartusLight sim (SourceInfo top src) = do
    dir <- liftSh pwd
    let ex = execute_ SynthFail dir "quartus"
    liftSh $ do
        writefile inpf $ genSource src
        noPrint $ run_ "sed" [ "-i"
                             , "s/^module/(* multstyle = \"logic\" *) module/;"
                             , toTextIgnore inpf
                             ]
        writefile quartusSdc "create_clock -period 5 -name clk [get_ports clock]"
        writefile quartusTcl $ quartusLightSynthConfig sim quartusSdc top inpf
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
    exec s = maybe (fromText s) (</> fromText s) $ quartusLightBin sim
    quartusTcl = fromText top <.> "tcl"
    quartusSdc = fromText top <.> "sdc"
