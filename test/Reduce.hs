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
    (reduceUnitTests)
where

import           Data.List        ((\\))
import           Test.Tasty
import           Test.Tasty.HUnit
import           VeriFuzz
import           VeriFuzz.Reduce

reduceUnitTests :: TestTree
reduceUnitTests = testGroup "Reducer tests"
    [ moduleReducerTest
    , modItemReduceTest
    , activeWireTest
    ]

activeWireTest :: TestTree
activeWireTest = testCase "Active wires" $ do
    findActiveWires verilog1 \\ ["x", "y", "z", "w"] @?= []
    findActiveWires verilog2 \\ ["x", "y", "z"] @?= []
    where
        verilog1 = head $ getVerilog [verilog|
module top(y, x);
  input x;
  output y;
  wire z;
  wire w;
  assign z = 0;
  assign w = 2;
  assign y = w + z;
endmodule
|]
        verilog2 = head $ getVerilog [verilog|
module top(y, x);
  input x;
  output y;
  wire z;
  wire w;
  assign z = 0;
endmodule
|]

modItemReduceTest :: TestTree
modItemReduceTest = testCase "Module items" $ do
    halveModItems srcInfo1 @?= golden1
    where
        srcInfo1 = SourceInfo "top" [verilog|
module top(y, x);
  input x;
  output y;
  wire z;
  wire w;
  assign z = x;
  assign w = z;
  assign y = w;
endmodule
|]
        golden1 = Dual (SourceInfo "top" [verilog|
module top(y, x);
  input x;
  output y;
  wire z;
  wire w;
  assign z = x;
  assign y = w;
endmodule
|]) $ SourceInfo "top" [verilog|
module top(y, x);
  input x;
  output y;
  wire z;
  wire w;
  assign w = 1'b0;
  assign y = w;
endmodule
|]

moduleReducerTest :: TestTree
moduleReducerTest = testCase "Module reducer" $ do
    halveModules srcInfo1 @?= golden1
    halveModules srcInfo2 @?= golden2
    where
        srcInfo1 = SourceInfo "top" [verilog|
module top(y, x);
  output wire [4:0] y;
  input wire [4:0] x;
  m m(y, x);
endmodule

module m(y, x);
  output wire [4:0] y;
  input wire [4:0] x;
endmodule
|]
        golden1 = Single $ SourceInfo "top" [verilog|
module top(y, x);
  output wire [4:0] y;
  input wire [4:0] x;
endmodule
|]
        srcInfo2 = SourceInfo "top" [verilog|
module top(y, x);
  output wire [4:0] y;
  input wire [4:0] x;
  m m(y, x);
  m2 m2(y, x);
endmodule

module m(y, x);
  output wire [4:0] y;
  input wire [4:0] x;
endmodule

module m2(y, x);
  output wire [4:0] y;
  input wire [4:0] x;
endmodule
|]
        golden2 = Dual (SourceInfo "top" [verilog|
module top(y, x);
  output wire [4:0] y;
  input wire [4:0] x;
  m m(y, x);
endmodule

module m(y, x);
  output wire [4:0] y;
  input wire [4:0] x;
endmodule
|]) $ SourceInfo "top" [verilog|
module top(y, x);
  output wire [4:0] y;
  input wire [4:0] x;
  m2 m2(y, x);
endmodule

module m2(y, x);
  output wire [4:0] y;
  input wire [4:0] x;
endmodule
|]
