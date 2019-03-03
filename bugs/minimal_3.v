module test_module(y, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10);
output wire [1505:0] y;
input wire [12:0] w1;
input wire [2:0] w2;
input wire [16:0] w3;
input wire [8:0] w4;
input wire [15:0] w5;
input wire [22:0] w6;
input wire [8:0] w7;
input wire [16:0] w8;
input wire [17:0] w9;
input wire [20:0] w10;
wire [18:0] w98;
assign w98 = (~&{((22'h5) ? w4 : w3)});
assign y = {w98};
endmodule
