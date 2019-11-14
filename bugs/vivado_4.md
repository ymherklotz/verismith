# Signed with shift in condition synthesis mistmatch

[ Not fixed | [Vivado forum 982518](https://forums.xilinx.com/t5/Synthesis/Vivado-2019-1-Signed-with-shift-in-condition-synthesis-mistmatch/td-p/982518) ]

## Affected versions

- Vivado 2019.1
- Vivado 2018.2

## Description

The following code seems to give a mismatch after synthesis. I am using Vivado 2019.1 on my personal Arch Linux machine and Vivado 2018.2 on CentOS 6.

```verilog
module top  (y, w0);
   output [1:0] y;
   input        w0;
   assign y = $signed(2'h1 < 2'h2) >> w0 ? 1'b1 : 1'b0;
endmodule
```

When the module is given `w0 = 0`, the output should be 1, because `$signed(1 < 2) >> 0 = 1`. However, synthesising the module gives the following net list in Verilog:

```
// Copyright 1986-2019 Xilinx, Inc. All Rights Reserved.
// --------------------------------------------------------------------------------
// Tool Version: Vivado v.2019.1 (lin64) Build 2552052 Fri May 24 14:47:09 MDT 2019
// Date        : Tue Jun 11 15:26:35 2019
// Host        : yann-arch running 64-bit unknown
// Command     : write_verilog -force syn_vivado.v
// Design      : top_vivado
// Purpose     : This is a Verilog netlist of the current design or from a specific cell of the design. The output is an
//               IEEE 1364-2001 compliant Verilog HDL file that contains netlist information obtained from the input
//               design files.
// Device      : xc7k70tfbg676-3
// --------------------------------------------------------------------------------
`timescale 1 ps / 1 ps

(* STRUCTURAL_NETLIST = "yes" *)
module top_vivado
   (y,
    w0);
  output [1:0]y;
  input w0;

  wire \<const0> ;
  wire [1:0]y;

  GND GND
       (.G(\<const0> ));
  OBUF \y_OBUF[0]_inst
       (.I(\<const0> ),
        .O(y[0]));
  OBUF \y_OBUF[1]_inst
       (.I(\<const0> ),
        .O(y[1]));
endmodule
```

Which assigns the output y to a constant 0.
