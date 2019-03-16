module test_module(y, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10);
output wire [1288:0] y;
input wire [17:0] w1;
input wire [11:0] w2;
input wire [24:0] w3;
input wire [2:0] w4;
input wire [26:0] w5;
input wire [22:0] w6;
input wire [2:0] w7;
input wire [10:0] w8;
input wire [23:0] w9;
input wire [5:0] w10;
wire [8:0] w95;
assign w95 = ({(1'h0), (-(4'hf)), (1'h0), w7, (-(6'h1)), w8, (-(2'he)), (-(7'h3)), (20'h7), (16'h13), (1'h0), (1'h0), (1'h0), w8, (26'ha), (23'h9), w7, (1'h0), (-(13'h17)), w4, w5, w7, (8'h3), (33'h19)} ? $signed((~|w6)) : (1'h0));
assign y = {w95};
endmodule
