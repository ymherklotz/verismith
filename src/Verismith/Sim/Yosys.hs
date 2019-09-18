{-|
Module      : Verismith.Sim.Yosys
Description : Yosys simulator implementation.
Copyright   : (c) 2018-2019, Yann Herklotz
License     : BSD-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

Yosys simulator implementation.
-}

{-# LANGUAGE QuasiQuotes #-}

module Verismith.Sim.Yosys
    ( Yosys(..)
    , defaultYosys
    , runEquiv
    , runEquivYosys
    )
where

import           Control.DeepSeq           (NFData, rnf, rwhnf)
import           Control.Lens
import           Control.Monad             (void)
import           Data.Text                 (Text, unpack)
import           Prelude                   hiding (FilePath)
import           Shelly
import           Shelly.Lifted             (liftSh)
import           Text.Shakespeare.Text     (st)
import           Verismith.Result
import           Verismith.Sim.Internal
import           Verismith.Sim.Template
import           Verismith.Verilog.AST
import           Verismith.Verilog.CodeGen
import           Verismith.Verilog.Mutate

data Yosys = Yosys { yosysBin    :: !(Maybe FilePath)
                   , yosysDesc   :: {-# UNPACK #-} !Text
                   , yosysOutput :: {-# UNPACK #-} !FilePath
                   }
              deriving (Eq)

instance Tool Yosys where
    toText (Yosys _ t _) = t

instance Show Yosys where
    show t = unpack $ toText t

instance Synthesiser Yosys where
  runSynth = runSynthYosys
  synthOutput = yosysOutput
  setSynthOutput (Yosys a b _) = Yosys a b

instance NFData Yosys where
    rnf = rwhnf

defaultYosys :: Yosys
defaultYosys = Yosys Nothing "yosys" "syn_yosys.v"

yosysPath :: Yosys -> FilePath
yosysPath sim = maybe (fromText "yosys") (</> fromText "yosys") $ yosysBin sim

runSynthYosys :: Yosys -> SourceInfo -> ResultSh ()
runSynthYosys sim (SourceInfo _ src) = do
    dir <- liftSh $ do
        dir' <- pwd
        writefile inpf $ genSource src
        return dir'
    execute_
        SynthFail
        dir
        "yosys"
        (yosysPath sim)
        [ "-p"
        , "read_verilog " <> inp <> "; synth; write_verilog -noattr " <> out
        ]
  where
    inpf = "rtl.v"
    inp  = toTextIgnore inpf
    out  = toTextIgnore $ synthOutput sim

runEquivYosys
    :: (Synthesiser a, Synthesiser b)
    => Yosys
    -> a
    -> b
    -> SourceInfo
    -> ResultSh ()
runEquivYosys yosys sim1 sim2 srcInfo = do
    liftSh $ do
        writefile "top.v"
            .  genSource
            .  initMod
            .  makeTop 2
            $  srcInfo
            ^. mainModule
        writefile checkFile $ yosysSatConfig sim1 sim2 srcInfo
    runSynth sim1 srcInfo
    runSynth sim2 srcInfo
    liftSh $ run_ (yosysPath yosys) [toTextIgnore checkFile]
    where checkFile = fromText [st|test.#{toText sim1}.#{toText sim2}.ys|]

runEquiv
    :: (Synthesiser a, Synthesiser b) => a -> b -> SourceInfo -> ResultSh ()
runEquiv sim1 sim2 srcInfo = do
    dir <- liftSh pwd
    liftSh $ do
        writefile "top.v"
            .  genSource
            .  initMod
            .  makeTopAssert
            $  srcInfo
            ^. mainModule
        replaceMods (synthOutput sim1) "_1" srcInfo
        replaceMods (synthOutput sim2) "_2" srcInfo
        writefile "proof.sby" $ sbyConfig sim1 sim2 srcInfo
    e <- liftSh $ do
        exe dir "symbiyosys" "sby" ["-f", "proof.sby"]
        lastExitCode
    case e of
        0   -> ResultT . return $ Pass ()
        2   -> ResultT . return $ Fail EquivFail
        124 -> ResultT . return $ Fail TimeoutError
        _   -> ResultT . return $ Fail EquivError
  where
    exe dir name e = void . errExit False . logCommand dir name . timeout e
