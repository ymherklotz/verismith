// Copyright 1986-2018 Xilinx, Inc. All Rights Reserved.
// --------------------------------------------------------------------------------
// Tool Version: Vivado v.2018.3 (lin64) Build 2405991 Thu Dec  6 23:36:41 MST 2018
// Date        : Thu Apr 11 19:11:05 2019
// Host        : yann-arch running 64-bit unknown
// Command     : write_verilog -force syn_vivado.v
// Design      : top
// Purpose     : This is a Verilog netlist of the current design or from a specific cell of the design. The output is an
//               IEEE 1364-2001 compliant Verilog HDL file that contains netlist information obtained from the input
//               design files.
// Device      : xc7k70tfbg676-3
// --------------------------------------------------------------------------------
`timescale 1 ps / 1 ps

(* use_dsp = "no" *) 
(* STRUCTURAL_NETLIST = "yes" *)
module top
   (y,
    wire0);
  output y;
  input wire0;

  wire \<const0> ;
  wire y;

  GND GND
       (.G(\<const0> ));
  OBUF y_OBUF_inst
       (.I(\<const0> ),
        .O(y));
endmodule
