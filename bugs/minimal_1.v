// Increasing the size of the wires makes yosys hang
module div_error(y, w1, w2);
   output [50:0] y;
   input [50:0] w1;
   input [50:0] w2;
   assign y = w1 / w2;
endmodule // div_error
