module test_module(y, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10);
output wire [1328:0] y;
input wire [19:0] w1;
input wire [5:0] w2;
input wire [29:0] w3;
input wire [20:0] w4;
input wire [7:0] w5;
input wire w6;
input wire [18:0] w7;
input wire [11:0] w8;
input wire [20:0] w9;
input wire [26:0] w10;
wire [14:0] w97;
assign w97 = (~^({w5, w7, (-(17'h18)), (30'hf), w1, (-(3'h14)), w6, (1'h0), (-(10'h12)), w2, (1'h0), (10'h3), (21'h9), w4, w6, w5, (1'h0), w7, w1, (1'h0)} ? {(-(14'h7)), (-(28'h16)), (25'h1b), (14'h7), (26'h1d), (9'h14), (-(22'hc)), (6'h1d), w10, (5'h8), w10, (5'h1b)} : $unsigned($signed(((-(29'h12)) <= (-(19'h14)))))));
assign y = {w97};
endmodule
