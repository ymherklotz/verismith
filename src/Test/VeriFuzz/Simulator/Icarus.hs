{-|
Module      : Test.VeriFuzz.Simulator.Icarus
Description : Icarus verilog module.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Icarus verilog module.
-}

{-# LANGUAGE QuasiQuotes #-}

module Test.VeriFuzz.Simulator.Icarus where

import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as B
import           Data.Hashable
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Prelude                         hiding (FilePath)
import           Shelly
import           Test.VeriFuzz.Simulator.General
import           Test.VeriFuzz.Verilog
import           Text.Shakespeare.Text           (st)

data Icarus = Icarus { icarusPath :: FilePath }

instance Simulator Icarus where
  toText _ = "iverilog"

instance Simulate Icarus where
  runSim = runSimIcarus

runSimIcarus :: Icarus -> ModDecl -> [ByteString] -> Sh Int
runSimIcarus sim mod tst = do
  let tb = ModDecl "main" [] []
        [ Initial $
          (SysTaskEnable $ Task "display" [ Str "21832" ])
          <> (SysTaskEnable $ Task "finish" [])
        ]
  let newtb = instantiateMod mod tb
  let modWithTb = VerilogSrc $ Description <$> [newtb, mod]
  writefile "main.v" $ genSource modWithTb
  run_ "iverilog" ["-o", "main", "main.v"]
  hash <$> run "vvp" ["main"]
