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

module Test.VeriFuzz.Simulator.Icarus where

import           Control.Lens
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as B
import           Data.Foldable                   (fold)
import           Data.Hashable
import           Data.List                       (transpose)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Prelude                         hiding (FilePath)
import           Shelly
import           Test.VeriFuzz.Simulator.General
import           Test.VeriFuzz.Verilog
import           Text.Shakespeare.Text           (st)

newtype Icarus = Icarus { icarusPath :: FilePath }

instance Simulator Icarus where
  toText _ = "iverilog"

instance Simulate Icarus where
  runSim = runSimIcarus

addDisplay :: [Stmnt] -> [Stmnt]
addDisplay s =
  concat $ transpose [s, replicate l $ TimeCtrl 1 Nothing
                     , replicate l . SysTaskEnable $ Task "display" ["%h", Id "y"]]
  where
    l = length s

assignFunc :: [Port] -> ByteString -> Stmnt
assignFunc inp bs =
  NonBlockAssign . Assign conc Nothing . Number (B.length bs * 4) $ bsToI bs
  where
    conc = RegConcat (portToExpr <$> inp)

runSimIcarus :: Icarus -> ModDecl -> [ByteString] -> Sh Int
runSimIcarus sim mod bss = do
  let tb = ModDecl "main" [] []
        [ Initial $
          fold (addDisplay $ assignFunc (mod ^. modInPorts) <$> bss)
          <> (SysTaskEnable $ Task "finish" [])
        ]
  let newtb = instantiateMod mod tb
  let modWithTb = VerilogSrc $ Description <$> [newtb, mod]
  writefile "main.v" $ genSource modWithTb
  run_ "iverilog" ["-o", "main", "main.v"]
  hash <$> run "vvp" ["main"]
