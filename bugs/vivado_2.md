# Vivado 2018.3 synthesis crash

[ Not fixed | [Vivado forum 981136](https://forums.xilinx.com/t5/Synthesis/Vivado-2018-3-synthesis-crash/td-p/981136) ]

## Affected versions

- Vivado 2019.1
- Vivado 2018.3

## Description

Vivado 2018.3 crashes with the following Verilog code.

```verilog
module top  (y, clk, wire0);
   output y;
   input  clk;
   input [1:0] wire0;
   reg [1:0]   reg1 = 0, reg0 = 0, reg2 = 0,
               reg3 = 0, reg4 = 0;
   reg signed [1:0] reg5 = 0;
   wire [1:0]       wire1;
   assign y = reg1;
   assign wire1 = reg4;
   always @(posedge clk)
     if (reg4)
       begin
          if (wire0)
            reg3 <= 1;
          reg2 <= 1;
       end
   always @(posedge clk)
     if (reg3) begin
        reg5 <= reg2[0:0];
        if (reg0)
          reg1 <= reg5;
        if (reg2[0:0])
          reg0 <= 1;
     end
endmodule
```

It results in the error shown below.

```
****** Vivado v2018.3 (64-bit)
  **** SW Build 2405991 on Thu Dec  6 23:36:41 MST 2018
  **** IP Build 2404404 on Fri Dec  7 01:43:56 MST 2018
    ** Copyright 1986-2018 Xilinx, Inc. All Rights Reserved.

source vivado_top.tcl
# set_msg_config -id {Synth 8-5821} -new_severity {WARNING}
# read_verilog rtl.v
# synth_design -part xc7k70t -top top
Command: synth_design -part xc7k70t -top top
Starting synth_design
Attempting to get a license for feature 'Synthesis' and/or device 'xc7k70t'
INFO: [Common 17-349] Got license for feature 'Synthesis' and/or device 'xc7k70t'
INFO: Launching helper process for spawning children vivado processes
INFO: Helper process launched with PID 10415
---------------------------------------------------------------------------------
Starting Synthesize : Time (s): cpu = 00:00:02 ; elapsed = 00:00:02 . Memory (MB): peak = 1349.855 ; gain = 0.000 ; free physical = 1545 ; free virtual = 5134
---------------------------------------------------------------------------------
INFO: [Synth 8-6157] synthesizing module 'top' [/home/user/project/rtl.v:1]
Abnormal program termination (11)
Please check '/home/user/project/hs_err_pid10402.log' for details
```
