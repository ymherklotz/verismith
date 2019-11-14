# Yosys aborts with exception in peephole optimizers pass

[ [Issue 993](https://github.com/YosysHQ/yosys/issues/993) ]

Yosys aborts with a `vector::_M_range_check` exception in the peephole optimizers pass (`PEEPOPT pass`). This happens with the verilog code below, which I have reduced as much as possible into an MCVE.

```verilog
module top(y, clk, wire1);
   output wire [1:0] y;
   input wire        clk;
   input wire [1:0]  wire1;
   reg [1:0]         reg1 = 0;
   reg [1:0]         reg2 = 0;
   always @(posedge clk) begin
      reg1 <= wire1 == 1;
   end
   always @(posedge clk) begin
      reg2 <= 1 >> reg1[1:1];
   end
   assign y = reg2;
endmodule
```

## Steps to reproduce the issue

Yosys version that this was tested with: `Yosys 0.8+411 (git sha1 70d0f389, clang 8.0.0 -fPIC -Os)`, which is the current master.

The following command was used to synthesise the design:

```
yosys -p 'read -formal rtl.v; synth; write_verilog rtl_yosys.v'
```

## Expected behavior

Synthesis without any exceptions.

```
=== top ===

   Number of wires:                  4
   Number of wire bits:              8
   Number of public wires:           4
   Number of public wire bits:       8
   Number of memories:               0
   Number of memory bits:            0
   Number of processes:              0
   Number of cells:                  1
     $_ORNOT_                        1

2.27. Executing CHECK pass (checking for obvious problems).
checking module top..
found and reported 0 problems.

3. Executing Verilog backend.
Dumping module `\top'.
```

## Actual behavior

Yosys aborts with a `vector::_M_range_check` exception.

```
2.7. Executing WREDUCE pass (reducing word size of cells).
Removed top 1 bits (of 2) from port B of cell top.$eq$rtl.v:7$2 ($eq).
Removed cell top.$eq$rtl.v:7$2 ($eq).
Removed top 30 bits (of 32) from port A of cell top.$shr$rtl.v:10$4 ($shr).

2.8. Executing PEEPOPT pass (run peephole optimizers).
terminate called after throwing an instance of 'std::out_of_range'
  what():  vector::_M_range_check: __n (which is 18446744073709551615) >= this->size() (which is 0)
zsh: abort (core dumped)  yosys -p 'read -formal rtl.v; synth; write_verilog rtl_yosys.v'
```
