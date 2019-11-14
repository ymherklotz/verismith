# Expression evaluates to 1'bx instead of expected 1'b0

[ Not fixed | [Issue 283](https://github.com/steveicarus/iverilog/issues/283) ]

## Affected versions

- Icarus Verilog 10.0
- Icarus Verilog 10.3

## Description

The following code outputs and assigns `y` to 1'bx instead of 1'b0. This happens in iverilog version 10.3 and also happens in version 10.0 (11/23/14) on edaplayground. However, this seems to execute fine in version 9.6 and 9.7 on edaplayground.

I have a link to the design loaded in [edaplayground](https://www.edaplayground.com/x/5SMW).

```verilog
module testbench;
   wire y;
   reg  clk;
   reg [15:0] wire0; // Reducing the size of wire0 to 15 bits seems to fix the output.
   reg        reg1 = 1'h0;
   assign y = reg1;
   initial
     begin
        clk = 1'h0;
        wire0 = 1'h0;
        #10 $finish;
     end
   always #5 clk = ~clk;
   always @(posedge clk) $strobe("%b", y);
   // Problematic line, should assign 1'b0 but assigns 1'bx.
   always @(posedge clk) reg1 <= (wire0 >> {wire0 ~^ 1'b1, wire0});
endmodule
```

After running

```
iverilog testbench.v -o im && ./im
```

the output is `x` instead of `0`.

If the always block is not used, and the expression is assigned to `y` directly, the result is the expected 1'b0. Any changes to the expression seems to fix the result as well.
