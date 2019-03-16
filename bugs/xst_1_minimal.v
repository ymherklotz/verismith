module test_module(y, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10);
output wire [1472:0] y;
input wire [23:0] w1;
input wire [20:0] w2;
input wire [1:0] w3;
input wire [29:0] w4;
input wire [23:0] w5;
input wire [22:0] w6;
input wire [20:0] w7;
input wire [26:0] w8;
input wire [3:0] w9;
input wire [8:0] w10;
wire [26:0] w99;
assign w99 = (&((30'h19) ? w8 : (30'h5)));
assign y = {w99};
endmodule
