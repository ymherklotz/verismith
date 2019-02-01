{-|
Module      : VeriFuzz.Env
Description : Environment to run the simulator and synthesisers in a matrix.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Environment to run the simulator and synthesisers in a matrix.
-}

module VeriFuzz.Env where

import           Control.Monad.Trans.Reader
import           VeriFuzz.Icarus
import           VeriFuzz.XST
import           VeriFuzz.Yosys

-- | Environment used to run the main
data SimMatrix = SimMatrix { yosys  :: Yosys
                           , xst    :: Maybe Xst
                           , icarus :: Maybe Icarus
                           }

type SimEnv = ReaderT SimMatrix IO

runAll :: SimEnv ()
runAll = do
  val <- asks xst
  shelly $ run_ "echo" ["Hello World"]
