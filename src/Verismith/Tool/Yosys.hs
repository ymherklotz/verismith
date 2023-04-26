{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : Verismith.Tool.Yosys
-- Description : Yosys simulator implementation.
-- Copyright   : (c) 2018-2022, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Yosys simulator implementation.
module Verismith.Tool.Yosys
  ( Yosys (..),
    defaultYosys,
    runEquiv,
    runEquivYosys,
  )
where

import Control.DeepSeq (NFData, rnf, rwhnf)
import Control.Lens
import Control.Monad (void)
import Data.Either (fromRight)
import Data.Text (Text, unpack)
import Shelly (FilePath, (</>))
import qualified Shelly as S
import Shelly.Lifted (liftSh, readfile)
import Verismith.CounterEg (parseCounterEg)
import Verismith.Result
import Verismith.Tool.Internal
import Verismith.Tool.Template
import Verismith.Verilog.AST
import Verismith.Verilog.CodeGen
import Verismith.Verilog.Mutate
import Prelude hiding (FilePath)

data Yosys = Yosys
  { yosysBin :: !(Maybe FilePath),
    yosysDesc :: !Text,
    yosysOutput :: !FilePath
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
yosysPath sim = maybe (S.fromText "yosys") (</> S.fromText "yosys") $ yosysBin sim

runSynthYosys :: (Show ann) => Yosys -> (SourceInfo ann) -> ResultSh ()
runSynthYosys sim (SourceInfo _ src) = do
  dir <- liftSh $ do
    dir' <- S.pwd
    S.writefile inpf $ genSource src
    return dir'
  execute_
    SynthFail
    dir
    "yosys"
    (yosysPath sim)
    [ "-p",
      "read_verilog " <> inp <> "; synth; write_verilog -noattr " <> out
    ]
  where
    inpf = "rtl.v"
    inp = S.toTextIgnore inpf
    out = S.toTextIgnore $ synthOutput sim

runEquivYosys ::
  (Synthesiser a, Synthesiser b, Show ann) =>
  Yosys ->
  a ->
  b ->
  (SourceInfo ann) ->
  ResultSh ()
runEquivYosys yosys sim1 sim2 srcInfo = do
  liftSh $ do
    S.writefile "top.v"
      . genSource
      . initMod
      . makeTop False 2
      $ srcInfo
        ^. mainModule
    S.writefile checkFile $ yosysSatConfig sim1 sim2 srcInfo
  runSynth sim1 srcInfo
  runSynth sim2 srcInfo
  liftSh $ S.run_ (yosysPath yosys) [S.toTextIgnore checkFile]
  where
    checkFile = S.fromText $ "test." <> toText sim1 <> "." <> toText sim2 <> ".ys"

runEquiv ::
  (Synthesiser a, Synthesiser b, Show ann) =>
  Maybe Text ->
  FilePath ->
  a ->
  b ->
  (SourceInfo ann) ->
  ResultSh ()
runEquiv mt datadir sim1 sim2 srcInfo = do
  dir <- liftSh S.pwd
  liftSh $ do
    S.writefile "top.v"
      . genSource
      . initMod
      . makeTopAssert
      $ srcInfo
        ^. mainModule
    replaceMods (synthOutput sim1) "_1" srcInfo
    replaceMods (synthOutput sim2) "_2" srcInfo
    S.writefile "proof.sby" $ sbyConfig mt datadir sim1 sim2 srcInfo
  e <- liftSh $ do
    exe dir "symbiyosys" "sby" ["-f", "proof.sby"]
    S.lastExitCode
  case e of
    0 -> ResultT . return $ Pass ()
    2 -> case mt of
      Nothing -> ResultT . return . Fail $ EquivFail Nothing
      Just _ ->
        ResultT $
          Fail
            . EquivFail
            . Just
            . fromRight mempty
            . parseCounterEg
            <$> readfile "proof/engine_0/trace.smtc"
    124 -> ResultT . return $ Fail TimeoutError
    _ -> ResultT . return $ Fail EquivError
  where
    exe dir name e = void . S.errExit False . logCommand dir name . timeout e
