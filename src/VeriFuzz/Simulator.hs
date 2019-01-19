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
  ( module VeriFuzz.Simulator.General
  , module VeriFuzz.Simulator.Yosys
  , module VeriFuzz.Simulator.Xst
  , module VeriFuzz.Simulator.Icarus
  )
where

import           Control.Monad.Trans.Reader (ReaderT)
import           Shelly
import           VeriFuzz.Simulator.General
import           VeriFuzz.Simulator.Icarus
import           VeriFuzz.Simulator.Xst
import           VeriFuzz.Simulator.Yosys

data SimMatrix = SimMatrix { yosys  :: Yosys
                           , xst    :: Maybe Xst
                           , icarus :: Maybe Icarus
                           }

type SimEnv = ReaderT SimMatrix Sh
