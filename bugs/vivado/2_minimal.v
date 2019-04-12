(* use_dsp48="no" *) module top
   (y, clk, wire0, wire1, wire2, wire3);
   output wire [163:0] y;
   input wire          clk;
   input wire signed [11:0] wire0;
   input wire [13:0]        wire1;
   input wire signed [7:0]  wire2;
   input wire signed [3:0]  wire3;
   wire signed [12:0]       wire19;
   wire [6:0]               wire18;
   wire [4:0]               wire17;
   wire signed [12:0]       wire16;
   wire signed [19:0]       wire15;
   wire [7:0]               wire14;
   wire                     wire13;
   wire [10:0]              wire12;
   wire signed [9:0]        wire11;
   wire signed [16:0]       wire10;
   reg [15:0]               reg9;
   reg signed [11:0]        reg8;
   wire [6:0]               wire7;
   wire signed [6:0]        wire6;
   wire [10:0]              wire5;
   wire [5:0]               wire4;
   assign wire13 = {wire0, $unsigned(((-17'hd) ? wire0 : (-18'he))), (((9'h8) ? (11'he) : reg8) <<< (~&(-8'h2))), ({reg9, wire1, (3'h12), (-5'hc)} | (&(-1'h12)))};
   assign wire17 = ((wire13 ? 1'b1 : ( 1'b1 ? (14'he) : wire0)) ? $unsigned($unsigned(((-2'h5)))) : $unsigned($unsigned((wire13 || wire2))));
   assign y = {wire17};
endmodule
