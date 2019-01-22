{-|
Module      : VeriFuzz.Simulator.Icarus
Description : Icarus verilog module.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Icarus verilog module.
-}

module VeriFuzz.Simulator.Icarus where

import           Control.Lens
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import           Data.Foldable              (fold)
import           Data.Hashable
import           Data.List                  (transpose)
import           Prelude                    hiding (FilePath)
import           Shelly
import           VeriFuzz.Simulator.General
import           VeriFuzz.Verilog

data Icarus = Icarus { icarusPath :: FilePath
                     , vvpPath    :: FilePath
                     }

instance Simulator Icarus where
  toText _ = "iverilog"

instance Simulate Icarus where
  runSim = runSimIcarus

defaultIcarus :: Icarus
defaultIcarus = Icarus "iverilog" "vvp"

addDisplay :: [Stmnt] -> [Stmnt]
addDisplay s = concat $ transpose
  [s, replicate l $ TimeCtrl 1 Nothing, replicate l . SysTaskEnable $ Task "display" ["%h", Id "y"]]
  where l = length s

assignFunc :: [Port] -> ByteString -> Stmnt
assignFunc inp bs = NonBlockAssign . Assign conc Nothing . Number (B.length bs * 4) $ bsToI bs
  where conc = RegConcat (portToExpr <$> inp)

runSimIcarus :: Icarus -> ModDecl -> [ByteString] -> Sh Int
runSimIcarus sim m bss = do
  let tb = ModDecl
        "main"
        []
        []
        [ Initial
          $  fold (addDisplay $ assignFunc (m ^. modInPorts) <$> bss)
          <> (SysTaskEnable $ Task "finish" [])
        ]
  let newtb     = instantiateMod m tb
  let modWithTb = VerilogSrc $ Description <$> [newtb, m]
  writefile "main.v" $ genSource modWithTb
  noPrint $ run_ (icarusPath sim) ["-o", "main", "main.v"]
  hash <$> run (vvpPath sim) ["main"]
