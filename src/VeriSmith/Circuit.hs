{-|
Module      : VeriSmith.Circuit
Description : Definition of the circuit graph.
Copyright   : (c) 2018-2019, Yann Herklotz
License     : BSD-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

Definition of the circuit graph.
-}

module VeriSmith.Circuit
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
import           Hedgehog                 (Gen)
import qualified Hedgehog.Gen             as Hog
import           VeriSmith.Circuit.Base
import           VeriSmith.Circuit.Gen
import           VeriSmith.Circuit.Random
import           VeriSmith.Verilog.AST
import           VeriSmith.Verilog.Mutate

fromGraph :: Gen ModDecl
fromGraph = do
    gr <- rDupsCirc <$> Hog.resize 100 randomDAG
    return
        $   initMod
        .   head
        $   nestUpTo 5 (generateAST gr)
        ^.. _Wrapped
        .   traverse
