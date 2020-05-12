{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : Reduce
-- Description : Test reduction.
-- Copyright   : (c) 2019, Yann Herklotz Grave
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Test reduction.
module Reduce
  ( reduceUnitTests,
  )
where

import Data.List ((\\))
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Verismith
import Verismith.Reduce

sourceInfo :: Text -> Verilog ReduceAnn -> SourceInfo ReduceAnn
sourceInfo = SourceInfo

reduceUnitTests :: TestTree
reduceUnitTests =
  testGroup
    "Reducer tests"
    [ moduleReducerTest,
      modItemReduceTest,
      halveStatementsTest,
      statementReducerTest,
      activeWireTest,
      cleanTest,
      cleanAllTest,
      removeDeclTest
    ]

removeConstInConcatTest :: TestTree
removeConstInConcatTest = testCase "Remove const in concat" $ do
  GenVerilog (removeDecl srcInfo1) @?= golden1
  where
    srcInfo1 =
      sourceInfo
        "top"
        [verilog|
module top;
  wire a;
  reg b;

  assign a = {1'b0, 1'b0, 1'b0, (1'b0), b, (1'b0), (1'b0)};

  always @(posedge clk) begin
    if (a)
      b <= 1 + 5 + {1'b0, 1'b1, 5'h20, b, 2'b0};
  end
endmodule
|]
    golden1 =
      GenVerilog $
        sourceInfo
          "top"
          [verilog|
module top;
  wire a;
  reg b;

  assign a = {b};

  always @(posedge clk) begin
    if (a)
      b <= 1 + 5 + {b};
  end
endmodule
|]

removeDeclTest :: TestTree
removeDeclTest = testCase "Remove declarations" $ do
  GenVerilog (removeDecl srcInfo1) @?= golden1
  where
    srcInfo1 =
      sourceInfo
        "top"
        [verilog|
module top;
  wire a;
  wire b;
  wire c;
  reg d;
  reg e;
  reg f;
  reg g;
  reg h;
  wire i;
  wire j;
  wire clk;
  initial d <= a;

  always @* begin
    f <= e;
    g <= e;
    if (1) begin
      h <= h;
    end
  end

  always @(posedge clk);

  assign b = g;
endmodule
|]
    golden1 =
      GenVerilog $
        sourceInfo
          "top"
          [verilog|
module top;
  wire a;
  wire b;
  reg d;
  reg e;
  reg f;
  reg g;
  reg h;
  wire clk;
  initial d <= a;

  always @* begin
    f <= e;
    g <= e;
    if (1) begin
      h <= h;
    end
  end

  always @(posedge clk);

  assign b = g;
endmodule
|]

cleanAllTest = testCase "Clean all" $ do
  GenVerilog (cleanSourceInfoAll srcInfo1) @?= golden1
  where
    srcInfo1 =
      sourceInfo
        "top"
        [verilog|
module top;
  wire a;
  wire b;
  wire c;
  wire d;
  assign a = b + c;
  assign b = c + d;
endmodule

module mod1;
  wire a;
  wire b;
  wire c;
  wire d;
  assign a = b + c;
  assign b = c + d;
endmodule

module mod2;
  wire a;
  wire b;
  wire c;
  wire d;
  assign a = b + c;
  assign b = c + d;
endmodule
|]
    golden1 =
      GenVerilog $
        sourceInfo
          "top"
          [verilog|
module top;
  wire a;
  wire b;
  wire c;
  wire d;
  assign a = b + 1'b0;
  assign b = 1'b0 + 1'b0;
endmodule

module mod1;
  wire a;
  wire b;
  wire c;
  wire d;
  assign a = b + 1'b0;
  assign b = 1'b0 + 1'b0;
endmodule

module mod2;
  wire a;
  wire b;
  wire c;
  wire d;
  assign a = b + 1'b0;
  assign b = 1'b0 + 1'b0;
endmodule
|]

cleanTest :: TestTree
cleanTest = testCase "Clean expression" $ do
  clean ["wire1", "wire2"] srcInfo1 @?= golden1
  clean ["wire1", "wire3"] srcInfo2 @?= golden2
  where
    srcInfo1 =
      GenVerilog . sourceInfo "top" $
        [verilog|
module top;
  wire wire1;
  wire wire2;
  wire wire3;
  assign wire1 = wire2[wire3];
endmodule
|]
    golden1 =
      GenVerilog . sourceInfo "top" $
        [verilog|
module top;
  wire wire1;
  wire wire2;
  wire wire3;
  assign wire1 = wire2[1'b0];
endmodule
|]
    srcInfo2 =
      GenVerilog . sourceInfo "top" $
        [verilog|
module top;
  wire wire1;
  wire wire2;
  wire wire3;
  assign wire1 = wire2[wire3:wire1];
endmodule
|]
    golden2 =
      GenVerilog . sourceInfo "top" $
        [verilog|
module top;
  wire wire1;
  wire wire2;
  wire wire3;
  assign wire1 = 1'b0;
endmodule
|]

activeWireTest :: TestTree
activeWireTest = testCase "Active wires" $ do
  findActiveWires "top" verilog1 \\ ["x", "y", "z", "w"] @?= []
  findActiveWires "top" verilog2 \\ ["x", "y", "z"] @?= []
  findActiveWires "top" verilog3 \\ ["x", "y", "clk", "r1", "r2"] @?= []
  findActiveWires "top" verilog4 \\ ["x", "y", "w", "a", "b"] @?= []
  where
    verilog1 =
      sourceInfo
        "top"
        [verilog|
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
    verilog2 =
      sourceInfo
        "top"
        [verilog|
module top(y, x);
  input x;
  output y;
  wire z;
  wire w;
  assign z = 0;
endmodule
|]
    verilog3 =
      sourceInfo
        "top"
        [verilog|
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
    verilog4 =
      sourceInfo
        "top"
        [verilog|
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

halveStatementsTest :: TestTree
halveStatementsTest = testCase "Statements" $ do
  GenVerilog <$> halveStatements "top" (tagAlways "top" srcInfo1) @?= golden1
  where
    srcInfo1 =
      sourceInfo
        "top"
        [verilog|
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
    golden1 =
      GenVerilog
        <$> Dual
          ( tagAlways "top" $
              sourceInfo
                "top"
                [verilog|
module top(clk, y, x);
  input clk;
  input x;
  output y;
  reg r1;
  reg r2;
  reg r3;
  always @(posedge clk) begin
    r1 <= r3;
  end

  always @(posedge clk) begin
    r1 <= r2;
    r2 <= r3;
    r3 <= r1;
  end
  assign y = {r1, r2, r3};
endmodule
|]
          )
          ( tagAlways "top" $
              sourceInfo
                "top"
                [verilog|
module top(clk, y, x);
  input clk;
  input x;
  output y;
  reg r1;
  reg r2;
  reg r3;
  always @(posedge clk) begin
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
          )

modItemReduceTest :: TestTree
modItemReduceTest = testCase "Module items" $ do
  GenVerilog <$> halveModItems "top" srcInfo1 @?= golden1
  where
    srcInfo1 =
      sourceInfo
        "top"
        [verilog|
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
    golden1 =
      GenVerilog
        <$> Dual
          ( sourceInfo
              "top"
              [verilog|
module top(y, x);
  input x;
  output y;
  wire z;
  wire w;
  assign y = 1'b0;
  assign z = x;
endmodule
|]
          )
          ( sourceInfo
              "top"
              [verilog|
module top(y, x);
  input x;
  output y;
  wire z;
  wire w;
  assign y = w;
  assign w = 1'b0;
endmodule
|]
          )

statementReducerTest :: TestTree
statementReducerTest = testCase "Statement reducer" $ do
  GenVerilog <$> halveStatements "top" srcInfo1 @?= fmap GenVerilog golden1
  GenVerilog <$> halveStatements "top" srcInfo2 @?= fmap GenVerilog golden2
  where
    srcInfo1 =
      tagAlways "top" $
        sourceInfo
          "top"
          [verilog|
module top(y, x);
  output wire [4:0] y;
  input wire [4:0] x;

  always @(posedge clk) begin
    a <= 1;
    b <= 2;
    c <= 3;
    d <= 4;
  end

  always @(posedge clk) begin
    a <= 1;
    b <= 2;
    c <= 3;
    d <= 4;
  end
endmodule
|]
    golden1 =
      Dual
        ( tagAlways "top" $
            sourceInfo
              "top"
              [verilog|
module top(y, x);
  output wire [4:0] y;
  input wire [4:0] x;

  always @(posedge clk) begin
    a <= 1;
    b <= 2;
  end

  always @(posedge clk) begin
    a <= 1;
    b <= 2;
    c <= 3;
    d <= 4;
  end
endmodule
|]
        )
        . tagAlways "top"
        $ sourceInfo
          "top"
          [verilog|
module top(y, x);
  output wire [4:0] y;
  input wire [4:0] x;

  always @(posedge clk) begin
    c <= 3;
    d <= 4;
  end

  always @(posedge clk) begin
    a <= 1;
    b <= 2;
    c <= 3;
    d <= 4;
  end
endmodule
|]
    srcInfo2 =
      tagAlways "top" $
        sourceInfo
          "top"
          [verilog|
module top(y, x);
  output wire [4:0] y;
  input wire [4:0] x;

  always @(posedge clk) begin
    if (x)
      y <= 2;
    else
      y <= 3;
  end
endmodule
|]
    golden2 =
      Dual
        ( tagAlways "top" $
            sourceInfo
              "top"
              [verilog|
module top(y, x);
  output wire [4:0] y;
  input wire [4:0] x;

  always @(posedge clk)
      y <= 2;
endmodule
|]
        )
        . tagAlways "top"
        $ sourceInfo
          "top"
          [verilog|
module top(y, x);
  output wire [4:0] y;
  input wire [4:0] x;

  always @(posedge clk)
      y <= 3;
endmodule
|]

moduleReducerTest :: TestTree
moduleReducerTest = testCase "Module reducer" $ do
  halveModules srcInfo1 @?= golden1
  halveModules srcInfo2 @?= golden2
  where
    srcInfo1 =
      sourceInfo
        "top"
        [verilog|
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
    golden1 =
      Single $
        sourceInfo
          "top"
          [verilog|
module top(y, x);
  output wire [4:0] y;
  input wire [4:0] x;
endmodule
|]
    srcInfo2 =
      sourceInfo
        "top"
        [verilog|
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
    golden2 =
      Dual
        ( sourceInfo
            "top"
            [verilog|
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
        )
        $ sourceInfo
          "top"
          [verilog|
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
