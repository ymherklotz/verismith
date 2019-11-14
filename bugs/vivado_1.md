# Verilog If statement nesting crash

[ [Vivado forum 981787](https://forums.xilinx.com/t5/Synthesis/Vivado-2019-1-Verilog-If-statement-nesting-crash/td-p/981787) ]

The following Verilog code crashes on Vivado 2019.1 and 2018.3. it has been reduced as much as possible to a minimal example from a larger design, which produces the crash. Removing any registers or removing the empty if-statements gets rid of the crash.

```verilog
module top (y, clk, wire0);
   output [1:0] y;
   input        clk;
   input [1:0]  wire0;
   reg [2:0]    reg0 = 0, reg1 = 0;
   reg [1:0]    reg2 = 0, reg3 = 0, reg4 = 0, reg5 = 0;
   assign y = reg2;
   always
     @(posedge clk) begin
        reg0 <=  1;
        reg5 <=  1;
        if (reg5)
          begin
             if (reg0);
             else
               if (wire0);
               else
                 if ($signed(reg3[0:0]))
                   begin
                      reg3 <=  reg0;
                      reg1 <=  reg3;
                   end
                 else
                   reg1 <=  reg0;
             reg2 <=  reg1[0:0];
          end
     end
endmodule
```

This crash occurs in Vivado 2018.3 on a CentOS 6 server and Vivado 2019.1 on my personal Arch Linux machine. It crashes with the following stack trace, meaning the problem is probably occuring in the HOptDfg::mergeReconvergentPartitions function.

```
/usr/lib/libc.so.6(+0x378b0) [0x7fbb3a1a88b0]
/opt/Xilinx/Vivado/2019.1/lib/lnx64.o/librdi_synth.so(+0x18a8008) [0x7fbaff38f008]
/opt/Xilinx/Vivado/2019.1/lib/lnx64.o/librdi_synth.so(HOptDfg::mergeReconvergentPartitions(DFPin*, DFGraph*, UHashSet<DFPin*, DFPin*, UEKey<DFPin*>, UHashSetNode<DFPin*>, DFPin*, UEValue<DFPin*, UHashSetNode<DFPin*> > > const&, UHashMap<DFNode*, DFPin*, DFNode*, UEKey<DFNode*> >&, UHashMap<DFGraph*, DFGraphInfo, DFGraph*, UEKey<DFGraph*> >&, DFGraph::DFGraphType, UHashMapList<DFGraph*, UHashList<DFPin*, DFPin*, UEKey<DFPin*>, UHashListNode<DFPin*>, DFPin*>, DFGraph*, UEKey<DFGraph*> >&)+0x33b) [0x7fbaff39848b]
```

The end of the log is posted below

```
---------------------------------------------------------------------------------
Finished Loading Part and Timing Information : Time (s): cpu = 00:00:04 ; elapsed = 00:00:05 . Memory (MB): peak = 1529.691 ; gain = 169.098 ; free physical = 900 ; free virtual = 4633
---------------------------------------------------------------------------------
Abnormal program termination (11)
Please check '/home/yannherklotz/Projects/verifuzz/vivado_crash/hs_err_pid28124.log' for details
```

I believe that this crash is different to https://forums.xilinx.com/t5/Synthesis/Vivado-2018-3-synthesis-crash/td-p/981136, because the function in the stack trace is different, which is why I posted this as a new post. I have attached a zip file below containing the tcl file and Verilog file, so to reproduce the crash one should be able to just run:

```
vivado -mode batch -source vivado_top.tcl
```
