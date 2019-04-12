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
   assign wire4 = $unsigned(($unsigned((-6'h11)) ? ((wire3 ? wire3 : (8'h0)) && $signed(wire2)) : (19'he)));
   assign wire5 = wire0;
   assign wire6 = (5'h4);
   assign wire7 = (wire4 ? {wire2, wire3, wire0, (-9'h6), (20'h10), wire1, (-1'hd), (-2'h10), (6'hc), (-16'hd), wire0} : $unsigned($unsigned((16'h5))));
   always @(posedge clk) begin
      reg8 = wire6;
      reg9 = wire0;
   end
   assign wire10 = (+wire0);
   assign wire11 = $unsigned({(wire6 || (1'h2))});
   assign wire12 = (12'h7);
   assign wire13 = {wire5, $unsigned(((-17'hd) ? wire5 : (-18'he))), (((9'h8) ? (11'he) : reg8) <<< (~&(-8'h2))), ({reg9, wire1, (3'h12), (-5'hc)} | (&(-1'h12)))};
   assign wire14 = ((3'h5) >>> ($unsigned((wire6 ? reg9 : (-16'hb))) > ((wire2 ^ (-20'h1)) ? ((11'h14) ? wire12 : (-10'h13)) : {(-15'h11), wire0, (-3'he), (-12'h14), (-3'hd), (-19'h11)})));
   assign wire15 = $unsigned(wire14);
   assign wire16 = ({((-15'hc) >= (2'ha)), (wire15 || reg8), $signed(wire12), {(-10'h12), wire7, wire1, (-19'h14), (5'ha), wire0, wire4, (15'h2), (14'hc), reg9, (19'hb), wire5, (8'h8), wire11, (-11'hc), wire5, wire2, (17'h0), (13'hb), wire2}, (1'h13), (11'hc), (9'h13), (-5'h2), $signed((1'hf)), (reg9 << wire2), {(11'h6), reg8, wire15, wire5, (16'ha), (20'ha), (-5'h8), wire14, (-4'h7), wire4, (4'ha), reg9, (-1'h2)}, $unsigned(reg9), ((-12'h1) ? wire3 : (11'h0)), (|(3'h3)), reg9, {(-13'h2), (-16'h13)}, wire2, (^~wire1), $signed(reg8)} == $signed({(-12'hf), wire11, (-14'h2), (13'he), wire4, reg8, (6'h7), reg9, reg8, wire4, wire3}));
   assign wire17 = ((wire13 ? ((7'h14) ? (-14'hb) : (-9'h7)) : (wire6 ? (14'he) : wire0)) ? $unsigned($unsigned(((-2'h5) * wire7))) : $unsigned($unsigned((wire13 || wire2))));
   assign wire18 = wire3;
   assign wire19 = wire2;
   assign y = {wire19, wire18, wire17, wire16, wire15, wire14, wire13, wire12, wire11, wire10, reg9, reg8, wire7, wire6, wire5, wire4};
endmodule
