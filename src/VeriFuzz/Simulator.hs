{-|
Module      : VeriFuzz.Simulator
Description : Simulator module.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Simulator module.
-}

module VeriFuzz.Simulator
  ( SimMatrix
  , module VeriFuzz.Simulator.General
  , module VeriFuzz.Simulator.Yosys
  , module VeriFuzz.Simulator.Xst
  , module VeriFuzz.Simulator.Icarus
  )
where

import           Control.Monad.Trans.Reader
import           Prelude                    hiding (FilePath)
import           Shelly
import           VeriFuzz.Simulator.General
import           VeriFuzz.Simulator.Icarus
import           VeriFuzz.Simulator.Xst
import           VeriFuzz.Simulator.Yosys

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
