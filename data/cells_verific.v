module VERIFIC_FADD (cin, a, b, cout, o);
  input cin, a, b;
  output cout, o;
  assign {cout, o} = cin + a + b;
endmodule

module VERIFIC_DFFRS (clk, s, r, d, q);
  input clk, d, s, r;
  output reg q ;
  always @(posedge clk, posedge s, posedge r)
    if (r) q <= 0;
    else if (s) q <= 1;
    else q <= d;
endmodule
