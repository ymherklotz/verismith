# Unsigned bit extension in if statement

[ [Vivado forum 981789](https://forums.xilinx.com/t5/Synthesis/Vivado-2019-1-Unsigned-bit-extension-in-if-statement/td-p/981789) ]

The code below does not seem to behave properly after synthesis with Vivado 2019.1. When the input to the module is `w1 = 2'b01`, then the output should be 0. This is because the unsigned literal `-1'b1` in the if statement is zero extended to 2 bits giving `-2'b01 = 2'b11`.

As `2'b11 != w1` (which is `2'b01`), `r1` should never be set.

However, instead of 0, after synthesis with Vivado, the output it 1. This seems to have something to do with the concatenation as well, as if that is removed, Vivado synthesis works as expected and performs the right zero extension.

Assigning `r1` directly to `{-1'b1 == w1}` also works as expected.

```
module top (y, clk, w1);
   output   y;
   input    clk;
   input signed [1:0] w1;
   reg                r1 = 1'b0;
   assign y = r1;
   
   always @(posedge clk)
     if ({-1'b1 == w1}) // when w1 = 2'b01 this should not be true
       r1 <= 1'b1;
endmodule // top
```

This happens on Vivado 2019.1 on my Arch linux machine, and also hapens on Vivado 2018.3 on CentOS 6.
