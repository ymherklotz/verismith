{-|
Module      : Reduce
Description : Test reduction.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Test reduction.
-}

{-# LANGUAGE QuasiQuotes #-}

module Reduce
    (reducerTests)
where

--import           Data.Either           (fromRight)
--import           Data.Text             (unpack)
import           Test.Tasty
---import           Text.Shakespeare.Text (st)
--import           VeriFuzz

reducerTests :: TestTree
reducerTests = testGroup "Reducer tests"
    [ moduleReducer ]

moduleReducer :: TestTree
moduleReducer = testGroup "Module reducer"
    [ ]

--reduceOneModule :: TestTree
--reduceOneModule = undefined
--
---- brittany-disable-next-binding
--moduleIn :: SourceInfo
--moduleIn = SourceInfo "top" . fromRight (Verilog []) . parseVerilog "" $ unpack [st|
--module m(x, y);
--input x;
--output y;
--endmodule
--
--module top(x, y);
--input x;
--output y;
--m m1(x, y)
--endmodule
-- |]
--
