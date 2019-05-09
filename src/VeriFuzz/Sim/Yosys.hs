{-|
Module      : VeriFuzz.Sim.Yosys
Description : Yosys simulator implementation.
Copyright   : (c) 2018-2019, Yann Herklotz
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Yosys simulator implementation.
-}

{-# LANGUAGE QuasiQuotes #-}

module VeriFuzz.Sim.Yosys
    ( Yosys(..)
    , defaultYosys
    , runEquiv
    , runEquivYosys
    )
where

import           Control.Lens
import           Data.Text                (Text, unpack)
import           Prelude                  hiding (FilePath)
import           Shelly
import           Shelly.Lifted            (liftSh)
import           Text.Shakespeare.Text    (st)
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Sim.Template
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.CodeGen
import           VeriFuzz.Verilog.Mutate

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

defaultYosys :: Yosys
defaultYosys = Yosys Nothing "yosys" "syn_yosys.v"

yosysPath :: Yosys -> FilePath
yosysPath sim = maybe (fromText "yosys") (</> fromText "yosys") $ yosysBin sim

runSynthYosys :: Yosys -> SourceInfo -> ResultSh ()
runSynthYosys sim (SourceInfo _ src) = (<?> SynthFail) . liftSh $ do
    dir <- pwd
    writefile inpf $ genSource src
    logger "Yosys: synthesis"
    logCommand_ dir "yosys" $ timeout
        (yosysPath sim)
        [ "-p"
        , "read -formal " <> inp <> "; synth; write_verilog -noattr " <> out
        ]
    logger "Yosys: synthesis done"
  where
    inpf = "rtl.v"
    inp  = toTextIgnore inpf
    out  = toTextIgnore $ synthOutput sim

runMaybeSynth :: (Synthesiser a) => Maybe a -> SourceInfo -> ResultSh ()
runMaybeSynth (Just sim) srcInfo = runSynth sim srcInfo
runMaybeSynth Nothing (SourceInfo _ src) =
    liftSh . writefile "rtl.v" $ genSource src

runEquivYosys
    :: (Synthesiser a, Synthesiser b)
    => Yosys
    -> a
    -> Maybe b
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
    runMaybeSynth sim2 srcInfo
    liftSh $ do
        logger "Yosys: equivalence check"
        run_ (yosysPath yosys) [toTextIgnore checkFile]
        logger "Yosys: equivalence done"
  where
    checkFile =
        fromText [st|test.#{toText sim1}.#{maybe "rtl" toText sim2}.ys|]

runEquiv
    :: (Synthesiser a, Synthesiser b)
    => a
    -> Maybe b
    -> SourceInfo
    -> ResultSh ()
runEquiv sim1 sim2 srcInfo = do
    dir <- liftSh pwd
    liftSh $ do
        writefile "top.v"
            .  genSource
            .  initMod
            .  makeTopAssert
            $  srcInfo
            ^. mainModule
        replaceMods (synthOutput sim1)               "_1" srcInfo
        replaceMods (maybe "rtl.v" synthOutput sim2) "_2" srcInfo
        writefile "proof.sby" $ sbyConfig sim1 sim2 srcInfo
    liftSh $ logger "Running SymbiYosys"
    execute_ EquivFail dir "symbiyosys" "sby" ["proof.sby"]
    liftSh $ logger "SymbiYosys equivalence check passed"
