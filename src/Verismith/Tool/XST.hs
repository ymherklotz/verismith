{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : Verismith.Tool.XST
-- Description : XST (ise) simulator implementation.
-- Copyright   : (c) 2018-2019, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- XST (ise) simulator implementation.
module Verismith.Tool.XST
  ( XST (..),
    defaultXST,
  )
where

import Control.DeepSeq (NFData, rnf, rwhnf)
import Data.Text (Text, unpack)
import Shelly
import Shelly.Lifted (liftSh)
import Verismith.Tool.Internal
import Verismith.Tool.Template
import Verismith.Verilog.AST
import Verismith.Verilog.CodeGen
import Prelude hiding (FilePath)

data XST = XST
  { xstBin :: !(Maybe FilePath),
    xstDesc :: !Text,
    xstOutput :: !FilePath
  }
  deriving (Eq)

instance Tool XST where
  toText (XST _ t _) = t

instance Show XST where
  show t = unpack $ toText t

instance Synthesiser XST where
  runSynth = runSynthXST
  synthOutput = xstOutput
  setSynthOutput (XST a b _) = XST a b

instance NFData XST where
  rnf = rwhnf

defaultXST :: XST
defaultXST = XST Nothing "xst" "syn_xst.v"

runSynthXST :: (Show ann) => XST -> (SourceInfo ann) -> ResultSh ()
runSynthXST sim (SourceInfo top src) = do
  dir <- liftSh pwd
  let exec n =
        execute_
          SynthFail
          dir
          "xst"
          (maybe (fromText n) (</> fromText n) $ xstBin sim)
  liftSh $ do
    writefile xstFile $ xstSynthConfig top
    writefile prjFile "verilog work \"rtl.v\""
    writefile "rtl.v" $ genSource src
  exec "xst" ["-ifn", toTextIgnore xstFile]
  exec
    "netgen"
    [ "-w",
      "-ofmt",
      "verilog",
      toTextIgnore $ modFile <.> "ngc",
      toTextIgnore $ synthOutput sim
    ]
  liftSh . noPrint $
    run_
      "sed"
      [ "-i",
        "/^`ifndef/,/^`endif/ d; s/ *Timestamp: .*//;",
        toTextIgnore $ synthOutput sim
      ]
  where
    modFile = fromText top
    xstFile = modFile <.> "xst"
    prjFile = modFile <.> "prj"
