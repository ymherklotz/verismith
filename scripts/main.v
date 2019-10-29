module top
#( parameter param9 = ((~(((8'ha3) >= (8'ha4)) <= (~&(8'ha4)))) ? ((((8'h9e) ? (8'h9c) : (8'h9c)) ? {(8'hb0)} : ((8'ha2) ? (8'ha8) : (8'hae))) ? (((8'h9c) ? (8'h9d) : (8'hae)) >>> ((8'hae) ? (8'had) : (8'h9e))) : (((8'ha1) ? (8'ha8) : (8'h9c)) < ((8'h9f) <= (8'hb0)))) : (!(!(8'hb0))))
, parameter param10 = param9
, parameter param11 = (param9 >= (8'ha1))
, parameter param12 = {((param11 != (8'h9f)) ? (^(^~(8'ha9))) : (+{param11}))} )
(y, clk, wire0, wire1);
  output wire  [(32'h34):(32'h0)] y  ;
  input wire  [(1'h0):(1'h0)] clk  ;
  input wire  [(4'h9):(1'h0)] wire0  ;
  input wire  [(3'h7):(1'h0)] wire1  ;
   wire signed [(3'h4):(1'h0)] wire8  ;
   wire signed [(4'h8):(1'h0)] wire7  ;
   wire  [(3'h7):(1'h0)] wire6  ;
   wire signed [(4'ha):(1'h0)] wire5  ;
   wire  [(4'h8):(1'h0)] wire4  ;
   wire  [(4'h9):(1'h0)] wire3  ;
   wire signed [(3'h5):(1'h0)] wire2  ;
  assign wire2 = ((^wire0) >> $signed(wire1[(3'h4):(1'h0)])) ;
  assign wire3 = (~|$signed(wire0)) ;
  assign wire4 = $signed({wire0}) ;
  assign wire5 = (&($unsigned((wire4 <<< wire4)) ^ $unsigned(((8'ha3) * wire0)))) ;
  assign wire6 = $signed(({(wire5 ?
                         wire2 : wire1)} >>> (^~((8'hac) + wire3)))) ;
  assign wire7 = wire4[(2'h3):(1'h1)] ;
  assign wire8 = {{wire0}} ;
  assign y = {wire8, wire7, wire6, wire5, wire4, wire3, wire2, (1'h0)} ;
endmodule