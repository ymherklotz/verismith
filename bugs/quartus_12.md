# Wrong Signed extension in binary expression

[ Not fixed | N/a ]

## Affected versions

- Quartus 19.1

## Description

Wrong signed extension in the binary xor operation. As the LHS in the binary operation is unsigned, the RHS should also be extended in an unsigned way, even though the `$signed` function is applied on the result. Therefore, the output should be `~ 2'b01 = 2'b10`, and not what Quartus does, which is `~ 2'b11 = 2'b00`.

```verilog
module top(y, wire1);
  output [1:0] y;
  input wire1;
  assign y = 2'b11 ^ $signed(wire1);
endmodule
```
