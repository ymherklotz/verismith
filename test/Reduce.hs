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
    ( reduceUnitTests
    )
where

import           Data.List        ((\\))
import           Test.Tasty
import           Test.Tasty.HUnit
import           VeriFuzz
import           VeriFuzz.Reduce

reduceUnitTests :: TestTree
reduceUnitTests = testGroup
    "Reducer tests"
    [ moduleReducerTest
    , modItemReduceTest
    , halveStatementsTest
    , activeWireTest
    , cleanTest
    ]

-- brittany-disable-next-binding
cleanTest :: TestTree
cleanTest = testCase "Clean expression" $ do
    clean ["wire1", "wire2"] srcInfo1 @?= golden1
    clean ["wire1", "wire3"] srcInfo2 @?= golden2
    where
        srcInfo1 = GenVerilog . SourceInfo "top" $ [verilog|
module top;
  wire wire1;
  wire wire2;
  wire wire3;
  assign wire1 = wire2[wire3];
endmodule
|]
        golden1 = GenVerilog . SourceInfo "top" $ [verilog|
module top;
  wire wire1;
  wire wire2;
  wire wire3;
  assign wire1 = wire2[1'b0];
endmodule
|]
        srcInfo2 = GenVerilog . SourceInfo "top" $ [verilog|
module top;
  wire wire1;
  wire wire2;
  wire wire3;
  assign wire1 = wire2[wire3:wire1];
endmodule
|]
        golden2 = GenVerilog . SourceInfo "top" $ [verilog|
module top;
  wire wire1;
  wire wire2;
  wire wire3;
  assign wire1 = 1'b0;
endmodule
|]


-- brittany-disable-next-binding
activeWireTest :: TestTree
activeWireTest = testCase "Active wires" $ do
    findActiveWires verilog1 \\ ["x", "y", "z", "w"]          @?= []
    findActiveWires verilog2 \\ ["x", "y", "z"]               @?= []
    findActiveWires verilog3 \\ ["x", "y", "clk", "r1", "r2"] @?= []
    findActiveWires verilog4 \\ ["x", "y", "w", "a", "b"]     @?= []
    where
        verilog1 = SourceInfo "top" [verilog|
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
        verilog2 = SourceInfo "top" [verilog|
module top(y, x);
  input x;
  output y;
  wire z;
  wire w;
  assign z = 0;
endmodule
|]
        verilog3 = SourceInfo "top" [verilog|
module top(clk, y, x);
  input clk;
  input x;
  output y;
  reg r1;
  reg r2;
  reg r3;
  wire w;
  always @(posedge clk) begin
    r1 <= r3;
    r2 <= r1;
  end

  always @(posedge clk) begin
    r1 <= r2;
  end
  random_inst inst(x, y, w);
  assign y = {r1, r2, r3};
endmodule
|]
        verilog4 = SourceInfo "top" [verilog|
module top(y, x);
  input x;
  output y;
  wire z;
  wire w;
  wire a;
  wire b;
  wire c;
  m1 m1(w, z);
  m2 m2(.x(c), .z(a), .y(b));
endmodule

module m1(y, x);
  input x;
  output y;
endmodule

module m2(y, z, x);
  input x;
  output y;
  output z;
endmodule
|]

-- brittany-disable-next-binding
halveStatementsTest :: TestTree
halveStatementsTest = testCase "Statements" $ do
    GenVerilog <$> halveStatements "top" srcInfo1 @?= golden1
    where
        srcInfo1 = SourceInfo "top" [verilog|
module top(clk, y, x);
  input clk;
  input x;
  output y;
  reg r1;
  reg r2;
  reg r3;
  always @(posedge clk) begin
    r1 <= r3;
    r2 <= r1;
    r3 <= r2;
  end

  always @(posedge clk) begin
    r1 <= r2;
    r2 <= r3;
    r3 <= r1;
  end
  assign y = {r1, r2, r3};
endmodule
|]
        golden1 = GenVerilog <$> Dual (SourceInfo "top" [verilog|
module top(clk, y, x);
  input clk;
  input x;
  output y;
  reg r1;
  reg r2;
  reg r3;
  always @(posedge clk) begin
    r1 <= 1'b0;
  end

  always @(posedge clk) begin
    r1 <= 1'b0;
  end
  assign y = {r1, 1'b0, 1'b0};
endmodule
|]) (SourceInfo "top" [verilog|
module top(clk, y, x);
  input clk;
  input x;
  output y;
  reg r1;
  reg r2;
  reg r3;
  always @(posedge clk) begin
    r2 <= 1'b0;
    r3 <= r2;
  end

  always @(posedge clk) begin
    r2 <= r3;
    r3 <= 1'b0;
  end
  assign y = {1'b0, r2, r3};
endmodule
|])

-- brittany-disable-next-binding
modItemReduceTest :: TestTree
modItemReduceTest = testCase "Module items" $ do
    GenVerilog <$> halveModItems "top" srcInfo1 @?= golden1
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
        golden1 = GenVerilog <$> Dual (SourceInfo "top" [verilog|
module top(y, x);
  input x;
  output y;
  wire z;
  wire w;
  assign y = 1'b0;
  assign z = x;
endmodule
|]) (SourceInfo "top" [verilog|
module top(y, x);
  input x;
  output y;
  wire z;
  wire w;
  assign y = w;
  assign w = 1'b0;
endmodule
|])

-- brittany-disable-next-binding
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
