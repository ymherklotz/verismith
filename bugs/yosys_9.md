# Unexpected behaviour of for loop and if statement

[ [Issue 1243](https://github.com/YosysHQ/yosys/issues/1243) ]

## Steps to reproduce the issue

Consider the following piece of code:

```verilog
module top  (y, clk, sel);
   output wire y  ;
   input       clk; 
   input       sel;
   reg         reg_assign = (1'h0) ;
   reg [1:0]   reg_count = (1'h0) ;
   assign y = reg_assign ;
   always @(posedge clk) 
     if (sel)
       for (reg_count = 0; reg_count < 2; reg_count = reg_count + 1'h1)
         if (0);
         else reg_assign <= 1;
     else reg_assign <=  0;
endmodule
```

First of all, the for loop in the code does seem a bit dodgy, however, I would still expect `reg_assign` to be set to 1 when `sel` is high. When `sel` is low, `reg_assign` should then be reset to 0. 

However, when synthesised with 

```
Yosys 0.8+618 (git sha1 acd8bc0a, clang  -fPIC -Os)
```

using

```
yosys -p "read_verilog rtl.v; synth; write_verilog -noattr synth.v"
```

`reg_assign` is set to a constant 0 instead of to what the value of `sel` is. Removing the dead if statement in the for loop results in the correct behaviour.

I have also attached a folder containing a test bench and SymbiYosys script to compare the design to the synthesised net list.

## Expected behaviour

I would expect this to be implemented by assigning `sel` to `y`. This is actually also the output of a previous version of Yosys (`Yosys 0.8+508 (git sha1 c2ea3746, clang 8.0.0 -fPIC -Os)`)

```verilog
module top(y, clk, sel);
  input clk;
  wire reg_assign;
  wire [1:0] reg_count;
  input sel;
  output y;
  assign reg_assign = 1'h0;
  assign reg_count = 2'h0;
  assign y = sel;
endmodule
```

## Actual behaviour

However, with Yosys, `y` is set to a constant 0.

```verilog
module top(y, clk, sel);
  input clk;
  wire reg_assign;
  wire [1:0] reg_count;
  input sel;
  output y;
  assign reg_assign = 1'h0;
  assign reg_count = 2'h0;
  assign y = 1'h0;
endmodule
```

[test.zip](https://github.com/YosysHQ/yosys/files/3457004/test.zip)
