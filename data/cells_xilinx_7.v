
// Based on the simulation models from /opt/Xilinx/14.5/ISE_DS/ISE/verilog/src/unisims/

module IBUF(O, I);
   output O;
   input  I;
   assign O = I;
endmodule

module OBUF(O, I);
   output O;
   input  I;
   assign O = I;
endmodule

module OBUFT(O, I, T);
   output O;
   input  I, T;
   assign O = T ? 1'bz : I;
endmodule

module GND(G);
   output G;
   assign G = 0;
endmodule

module INV(O, I);
   input I;
   output O;
   assign O = !I;
endmodule

module LUT1(O, I0);
   parameter INIT = 0;
   input I0;
   wire [1:0] lutdata = INIT;
   wire [0:0] idx = { I0 };
   output     O;
   assign O = lutdata[idx];
endmodule

module LUT2(O, I0, I1);
   parameter INIT = 0;
   input I0, I1;
   wire [3:0] lutdata = INIT;
   wire [1:0] idx = { I1, I0 };
   output     O;
   assign O = lutdata[idx];
endmodule

module LUT3(O, I0, I1, I2);
   parameter INIT = 0;
   input I0, I1, I2;
   wire [7:0] lutdata = INIT;
   wire [2:0] idx = { I2, I1, I0 };
   output     O;
   assign O = lutdata[idx];
endmodule

module LUT4(O, I0, I1, I2, I3);
   parameter INIT = 0;
   input I0, I1, I2, I3;
   wire [15:0] lutdata = INIT;
   wire [3:0]  idx = { I3, I2, I1, I0 };
   output      O;
   assign O = lutdata[idx];
endmodule

module LUT5(O, I0, I1, I2, I3, I4);
   parameter INIT = 0;
   input I0, I1, I2, I3, I4;
   wire [31:0] lutdata = INIT;
   wire [4:0]  idx = { I4, I3, I2, I1, I0 };
   output      O;
   assign O = lutdata[idx];
endmodule

module LUT6(O, I0, I1, I2, I3, I4, I5);
   parameter INIT = 0;
   input I0, I1, I2, I3, I4, I5;
   wire [63:0] lutdata = INIT;
   wire [5:0]  idx = { I5, I4, I3, I2, I1, I0 };
   output      O;
   assign O = lutdata[idx];
endmodule

module MUXCY(O, CI, DI, S);
   input CI, DI, S;
   output O;
   assign O = S ? CI : DI;
endmodule

module MUXF7(O, I0, I1, S);
   input I0, I1, S;
   output O;
   assign O = S ? I1 : I0;
endmodule

module MUXF8(O, I0, I1, S);
   input I0, I1, S;
   output O;
   assign O = S ? I1 : I0;
endmodule

module VCC(P);
   output P;
   assign P = 1;
endmodule

module XORCY(O, CI, LI);
   input CI, LI;
   output O;
   assign O = CI ^ LI;
endmodule

module CARRY4(CO, O, CI, CYINIT, DI, S);
   output [3:0] CO, O;
   input        CI, CYINIT;
   input [3:0]  DI, S;
   wire         ci_or_cyinit;
   assign O = S ^ {CO[2:0], ci_or_cyinit};
   assign CO[0] = S[0] ? ci_or_cyinit : DI[0];
   assign CO[1] = S[1] ? CO[0] : DI[1];
   assign CO[2] = S[2] ? CO[1] : DI[2];
   assign CO[3] = S[3] ? CO[2] : DI[3];
   assign ci_or_cyinit = CI | CYINIT;
endmodule

module LDCE (Q, CLR, D, G, GE);
   parameter [0:0] INIT = 1'b0;
   parameter [0:0] IS_CLR_INVERTED = 1'b0;
   parameter [0:0] IS_G_INVERTED = 1'b0;

   output Q;
   reg    Q = INIT;

   input  CLR, D, G, GE;
   wire   CLR_in, G_in;

   assign CLR_in = IS_CLR_INVERTED ^ CLR;
   assign G_in = IS_G_INVERTED ^ G;

   always @( CLR_in or D or G_in or GE)
     if (CLR_in)
       Q <= 0;
     else if (G_in && GE)
       Q <= D;
endmodule

module BUFG (O, I);
   output O;
   input  I;
   buf B1 (O, I);
endmodule

module FDRE (Q, C, CE, D, R);
   parameter [0:0] INIT = 1'b0;
   parameter [0:0] IS_C_INVERTED = 1'b0;
   parameter [0:0] IS_D_INVERTED = 1'b0;
   parameter [0:0] IS_R_INVERTED = 1'b0;

   output Q;
   reg    Q = INIT;

   input  C, CE, D, R;
   wire   C_in, D_in, R_in;

   assign C_in = C ^ IS_C_INVERTED;
   assign D_in = D ^ IS_D_INVERTED;
   assign R_in = R ^ IS_R_INVERTED;

   always @(posedge C_in)
     if (R_in)
       Q <= 0;
     else if (CE)
       Q <= D_in;
endmodule

module FDSE (Q, C, CE, D, S);
   parameter INIT = 1'b1;
   parameter [0:0] IS_C_INVERTED = 1'b0;
   parameter [0:0] IS_S_INVERTED = 1'b0;
   parameter [0:0] IS_D_INVERTED = 1'b0;

   output Q;

   input  C, CE, D, S;

   wire   Q;
   wire   C_in;
   wire   S_in;
   wire   D_in;
   wire   rst_int = 0;
   wire   set_int = S;
   reg    q_out;

   initial q_out = INIT;

   assign Q = q_out;
   assign C_in = IS_C_INVERTED ^ C;
   assign S_in = IS_S_INVERTED ^ S;
   assign D_in = IS_D_INVERTED ^ D;

   always @(posedge C_in)
	 if (S_in)
	   q_out <=  1;
	 else if (CE)
	   q_out <=  D_in;
endmodule

module LD (Q, D, G);
   parameter INIT = 1'b0;
   output Q;
   wire   Q;
   input  D, G;
   reg    q_out;

   initial q_out = INIT;

   assign Q = q_out;

   always @(D or G)
     if (G)
       q_out <= D;
endmodule

module FD (Q, C, D);
   parameter INIT = 1'b0;
   output Q;
   input  C, D;

   wire   Q;
   reg    q_out;

   initial q_out = INIT;

   always @(posedge C)
     q_out <=  D;

   assign Q = q_out;
endmodule

module LDPE (Q, D, G, GE, PRE);
   parameter [0:0] INIT = 1'b1;
   parameter [0:0] IS_G_INVERTED = 1'b0;
   parameter [0:0] IS_PRE_INVERTED = 1'b0;

   output Q;
   reg    Q = INIT;

   input  D, G, GE, PRE;
   wire   G_in, PRE_in;

   assign G_in = IS_G_INVERTED ^ G;
   assign PRE_in = IS_PRE_INVERTED ^ PRE;

   always @( PRE_in or D or G_in or GE)
     if (PRE_in)
       Q <= 1;
     else if (G_in && GE)
       Q <= D;
endmodule
