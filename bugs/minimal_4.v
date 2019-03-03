module test_module(y, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10);
output wire [1458:0] y;
input wire [26:0] w1;
input wire [24:0] w2;
input wire [23:0] w3;
input wire [1:0] w4;
input wire [5:0] w5;
input wire [12:0] w6;
input wire [27:0] w7;
input wire [16:0] w8;
input wire [14:0] w9;
input wire [13:0] w10;
wire [23:0] w96;
assign w96 = {(-(-(10'h1))), ({(6'hb), (-(25'h3)), w7, w10, (1'h0), w2, w2, (17'h1c)} ? $signed(w6) : ((22'hd) | w10))};
assign y = {w96};
endmodule
