
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

module FD_1 (Q, C, D);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, D;

	always @(negedge C)
	        Q <= D;

endmodule // FD_1

module FDC_1 (Q, C, CLR, D);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, CLR, D;

	always @(posedge CLR or negedge C)
	    if (CLR)
		Q <= 0;
	    else
    	        Q <= D;

endmodule // FDC_1

module FDCE_1 (Q, C, CE, CLR, D);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, CE, CLR, D;

	always @(posedge CLR or negedge C)
	    if (CLR)
		 Q <= 0;
	    else if (CE)
		Q <= D;

endmodule // FDCE_1

module FDCE (Q, C, CE, CLR, D);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, CE, CLR, D;

	always @(posedge CLR or posedge C)
	    if (CLR)
		Q <= 0;
	    else if (CE)
		Q <= D;

endmodule // FDCE

module FDCP_1 (Q, C, CLR, D, PRE);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, CLR, D, PRE;

	always @(posedge CLR or posedge PRE or negedge C)
	    if (CLR)
		Q <= 0;
	    else if (PRE)
		Q <= 1;
	    else
	        Q <= D;

endmodule // FDCP_1

module FDCPE_1 (Q, C, CE, CLR, D, PRE);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, CE, CLR, D, PRE;

	always @(posedge CLR or posedge PRE or negedge C)
	    if (CLR)
		Q <= 0;
	    else if (PRE)
		Q <= 1;
	    else if (CE)
		Q <= D;

endmodule // FDCPE_1

module FDCPE (Q, C, CE, CLR, D, PRE);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, CE, CLR, D, PRE;

	always @(posedge CLR or posedge PRE or posedge C)
	    if (CLR)
		Q <= 0;
	    else if (PRE)
		Q <= 1;
	    else if (CE)
		Q <= D;

endmodule // FDCPE

module FDCP (Q, C, CLR, D, PRE);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, CLR, D, PRE;

	always @(posedge CLR or posedge PRE or posedge C)
	    if (CLR)
		Q <= 0;
	    else if (PRE)
		Q <= 1;
	    else
	    	Q <= D;

endmodule // FDCP

module FDC (Q, C, CLR, D);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, CLR, D;

	always @(posedge CLR or posedge C)
	    if (CLR)
		Q <= 0;
	    else
	        Q <= D;

endmodule // FDC

module FDE_1 (Q, C, CE, D);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, CE, D;

	always @(negedge C)
	    if (CE)
		Q <= D;

endmodule // FDE_1

module FDE (Q, C, CE, D);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, CE, D;

	always @(posedge C)
	    if (CE)
		Q <= D;

endmodule // FDE

module FDP_1 (Q, C, D, PRE);

    parameter INIT = 1'b1;

    output Q;
    reg    Q;

    input  C, D, PRE;

	always @(posedge PRE or negedge C)
	    if (PRE)
		Q <= 1;
	    else
	        Q <= D;

endmodule // FDP_1

module FDPE_1 (Q, C, CE, D, PRE);

    parameter INIT = 1'b1;

    output Q;
    reg    Q;

    input  C, CE, D, PRE;

	always @(posedge PRE or negedge C)
	    if (PRE)
		Q <= 1;
	    else if (CE)
		Q <= D;

endmodule // FDPE_1

module FDPE (Q, C, CE, D, PRE);

    parameter INIT = 1'b1;

    output Q;
    reg    Q;

    input  C, CE, D, PRE;

	always @(posedge PRE or posedge C)
	    if (PRE)
		Q <= 1;
	    else if (CE)
		Q <= D;

endmodule // FDPE

module FDP (Q, C, D, PRE);

    parameter INIT = 1'b1;

    output Q;
    reg    Q;

    input  C, D, PRE;

	always @(posedge PRE or posedge C)
	    if (PRE)
		Q <= 1;
	    else
		Q <= D;

endmodule // FDP

module FDR_1 (Q, C, D, R);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, D, R;

	always @(negedge C)
	    if (R)
		Q <= 0;
	    else
		Q <= D;

endmodule // FDR_1

module FDRE_1 (Q, C, CE, D, R);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, CE, D, R;

	always @(negedge C)
	    if (R)
		Q <= 0;
	    else if (CE)
		Q <= D;

endmodule // FDRE_1

module FDRE (Q, C, CE, D, R);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, CE, D, R;

	always @(posedge C)
	    if (R)
		Q <= 0;
	    else if (CE)
		Q <= D;

endmodule // FDRE

module FDRS_1 (Q, C, D, R, S);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, D, R, S;

	always @(negedge C)
	    if (R)
		Q <= 0;
	    else if (S)
		Q <= 1;
	    else
		Q <= D;

endmodule // FDRS_1

module FDRSE_1 (Q, C, CE, D, R, S);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, CE, D, R, S;

	always @(negedge C)
	    if (R)
		Q <= 0;
	    else if (S)
		Q <= 1;
	    else if (CE)
		Q <= D;

endmodule // FDRSE_1

module FDRSE (Q, C, CE, D, R, S);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, CE, D, R, S;

	always @(posedge C)
	    if (R)
		Q <= 0;
	    else if (S)
		Q <= 1;
	    else if (CE)
		Q <= D;

endmodule // FDRSE

module FDRS (Q, C, D, R, S);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, D, R, S;

	always @(posedge C)
	    if (R)
		Q <= 0;
	    else if (S)
		Q <= 1;
	    else
		Q <= D;

endmodule // FDRS

module FDR (Q, C, D, R);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, D, R;

	always @(posedge C)
	    if (R)
		Q <= 0;
	    else
		Q <= D;

endmodule // FDR

module FDS_1 (Q, C, D, S);

    parameter INIT = 1'b1;

    output Q;
    reg    Q;

    input  C, D, S;

	always @(negedge C)
	    if (S)
		Q <= 1;
	    else
		Q <= D;

endmodule // FDS_1

module FDSE_1 (Q, C, CE, D, S);

    parameter INIT = 1'b1;

    output Q;
    reg    Q;

    input  C, CE, D, S;

	always @(negedge C)
	    if (S)
		Q <= 1;
	    else if (CE)
		Q <= D;

endmodule // FDSE_1

module FDSE (Q, C, CE, D, S);

    parameter INIT = 1'b1;

    output Q;
    reg    Q;

    input  C, CE, D, S;

	always @(posedge C)
	    if (S)
		Q <= 1;
	    else if (CE)
		Q <= D;

endmodule // FDSE

module FDS (Q, C, D, S);

    parameter INIT = 1'b1;

    output Q;
    reg    Q;

    input  C, D, S;

	always @(posedge C)
	    if (S)
		Q <= 1;
	    else
		Q <= D;

endmodule // FDS

module FD (Q, C, D);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  C, D;

	always @(posedge C)
		Q <= D;

endmodule // FD

module LD_1 (Q, D, G);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  D, G;

	always @( D or G)
	    if (!G)
		Q <= D;

endmodule // LD_1

module LDC_1 (Q, CLR, D, G);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  CLR, D, G;

	always @( CLR or D or G)
	    if (CLR)
		Q <= 0;
	    else if (!G)
		Q <= D;

endmodule // LDC_1

module LDCE_1 (Q, CLR, D, G, GE);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  CLR, D, G, GE;

	always @( CLR or D or G or GE)
	    if (CLR)
		Q <= 0;
	    else if (!G && GE)
		Q <= D;

endmodule // LDCE_1

module LDCE (Q, CLR, D, G, GE);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  CLR, D, G, GE;

	always @( CLR or D or G or GE)
	    if (CLR)
		Q <= 0;
	    else if (G && GE)
		Q <= D;

endmodule // LDCE

module LDCP_1 (Q, CLR, D, G, PRE);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  CLR, D, G, PRE;

	always @( CLR or PRE or D or G)
	    if (CLR)
		Q <= 0;
	    else if (PRE)
		Q <= 1;
	    else if (!G)
		Q <= D;

endmodule // LDCP_1

module LDCPE_1 (Q, CLR, D, G, GE, PRE);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  CLR, D, G, GE, PRE;

	always @( CLR or PRE or D or G or GE)
	    if (CLR)
		Q <= 0;
	    else if (PRE)
		Q <= 1;
	    else if (!G && GE)
		Q <= D;

endmodule // LDCPE_1

module LDCPE (Q, CLR, D, G, GE, PRE);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  CLR, D, G, GE, PRE;

	always @( CLR or PRE or D or G or GE)
	    if (CLR)
		Q <= 0;
	    else if (PRE)
		Q <= 1;
	    else if (G && GE)
		Q <= D;

endmodule // LDCPE

module LDCP (Q, CLR, D, G, PRE);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  CLR, D, G, PRE;

	always @( CLR or PRE or D or G)
	    if (CLR)
		Q <= 0;
	    else if (PRE)
		Q <= 1;
	    else if (G)
		Q <= D;

endmodule // LDCP

module LDC (Q, CLR, D, G);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  CLR, D, G;

	always @( CLR or D or G)
	    if (CLR)
		Q <= 0;
	    else if (G)
		Q <= D;

endmodule // LDC

module LDE_1 (Q, D, G, GE);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  D, G, GE;

	always @( D or G or GE)
	    if (!G && GE)
		Q <= D;

endmodule // LDE_1

module LDE (Q, D, G, GE);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  D, G, GE;

	always @( D or G or GE)
	    if (G && GE)
		Q <= D;

endmodule // LDE

module LDP_1 (Q, D, G, PRE);

    parameter INIT = 1'b1;

    output Q;
    reg    Q;

    input  D, G, PRE;

	always @( PRE or D or G)
	    if (PRE)
		Q <= 1;
	    else if (!G)
		Q <= D;

endmodule // LDP_1

module LDPE_1 (Q, D, G, GE, PRE);

    parameter INIT = 1'b1;

    output Q;
    reg    Q;

    input  D, G, GE, PRE;

	always @( PRE or D or G or GE)
	    if (PRE)
		Q <= 1;
	    else if (!G && GE)
		Q <= D;

endmodule // LDPE_1

module LDPE (Q, D, G, GE, PRE);

    parameter INIT = 1'b1;

    output Q;
    reg    Q;

    input  D, G, GE, PRE;

	always @( PRE or D or G or GE)
	    if (PRE)
		Q <= 1;
	    else if (G && GE)
		Q <= D;

endmodule // LDPE

module LDP (Q, D, G, PRE);

    parameter INIT = 1'b1;

    output Q;
    reg    Q;

    input  D, G, PRE;

	always @( PRE or D or G)
	    if (PRE)
		Q <= 1;
	    else if (G)
		Q <= D;

endmodule // LDP

module LD (Q, D, G);

    parameter INIT = 1'b0;

    output Q;
    reg    Q;

    input  D, G;

	always @( D or G)
	    if (G)
		Q <= D;

endmodule // LD
