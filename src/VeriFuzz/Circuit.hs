{-|
Module      : VeriFuzz.Circuit
Description : Definition of the circuit graph.
Copyright   : (c) 2018-2019, Yann Herklotz
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Definition of the circuit graph.
-}

module VeriFuzz.Circuit
    ( -- * Circuit
      Gate(..)
    , Circuit(..)
    , CNode(..)
    , CEdge(..)
    , fromGraph
    , generateAST
    , rDups
    , rDupsCirc
    , randomDAG
    , genRandomDAG
    )
where

import           Control.Lens
import           Hedgehog                (Gen)
import qualified Hedgehog.Gen            as Hog
import           VeriFuzz.Circuit.Base
import           VeriFuzz.Circuit.Gen
import           VeriFuzz.Circuit.Random
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.Mutate

fromGraph :: Gen ModDecl
fromGraph = do
    gr <- rDupsCirc <$> Hog.resize 100 randomDAG
    return
        $   initMod
        .   head
        $   nestUpTo 5 (generateAST gr)
        ^.. getVerilog
        .   traverse
        .   getDescription
