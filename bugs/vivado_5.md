# Bit selection synthesis mismatch

[ Not fixed | [Vivado forum 982419](https://forums.xilinx.com/t5/Synthesis/Vivado-2019-1-Bit-selection-synthesis-mismatch/td-p/982419) ]

## Affected versions

- Vivado 2019.1
- Vivado 2018.2
- Vivado 2017.4
- Vivado 2016.2
- Vivado 2016.1

## Description

There seems to be a mismatch between the synthesised net list and the initial design with the following Verilog code. This happens with Vivado 2019.1 on my personal arch linux machine and Vivado 2018.2 on CentoOS 6. I have attached all the necessary files to run it and hopefully reproduce it, together with a testbench that dumps a vcd file.

I have reduced the Verilog as much as possible, and changing anything else makes Vivado synthesise correctly.

```verilog
module top (y, clk, w0);
   output [1:0] y;
   input clk;
   input [1:0] w0;
   reg [2:0] r1 = 3'b0;
   reg [1:0] r0 = 2'b0;
   assign y = r1;
   always
     @(posedge clk) begin
        r0 <= 1'b1;
        if (r0) r1 <= r0 ? w0[0:0] : 1'b0;
        else r1 <= 3'b1;
     end
endmodule
```

For an input of `w2 = 2b'10` for two clock cycles, the final output should be `2'd0`, because the if statement is entered on the second clk cycle and the lsb of `w0` is assigned to `r1`, which is `1'b0`.

However, with Vivado the output seems to be `2'10` instead, which seems like Vivado does not truncate the value of the input to the lsb in `w0[0:0]`.

Expected output after 2 clock cycles with `2'b10` as input: `2'b00`

Actual output: `2'b10`
