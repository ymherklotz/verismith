(* use_dsp48="no" *) module top
   (y, wire0);
   output wire y;
   input wire      wire0;
   wire [1:0]       wire4;
   assign wire4 = 1'b1;
   assign y = (wire4 <<< 1'h1) && wire0;
endmodule
