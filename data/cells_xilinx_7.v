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
   input  C, D;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(negedge C  )
	 q_out <= D;
endmodule // FD_1

module FDC_1 (Q, C, CLR, D);
   parameter INIT = 1'b0;
   output Q;
   input  C, CLR, D;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(CLR)
     if (CLR)
       q_out <= 0;
   always @(negedge C)
	 q_out <=  D;
endmodule // FDC_1

module FDCE_1 (Q, C, CE, CLR, D);
   parameter INIT = 1'b0;
   output Q;
   input  C, CE, CLR, D;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(CLR)
     if (CLR)
       q_out <= 0;
   always @(negedge C)
     if (CE)
       q_out <=  D;
endmodule // FDCE_1

module FDCE (Q, C, CE, CLR, D);
   parameter INIT = 1'b0;
   output Q;
   input  C, CE, CLR, D;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(CLR)
     if (CLR)
       q_out <= 0;
   always @(posedge C)
     if (CE)
	   q_out <= D;
endmodule // FDCE

module FDCP_1 (Q, C, CLR, D, PRE);
   parameter INIT = 1'b0;
   output Q;
   input  C, CLR, D, PRE;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(CLR or PRE)
     if (CLR)
       q_out <= 0;
     else if (PRE)
       q_out <= 1;
   always @(negedge C )
     q_out <=  D;
endmodule // FDCP_1

module FDCPE_1 (Q, C, CE, CLR, D, PRE);
   parameter INIT = 1'b0;
   output Q;
   input  C, CE, CLR, D, PRE;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(CLR or PRE)
     if (CLR)
       q_out <= 0;
     else if (PRE)
       q_out <= 1;
   always @(negedge C )
     if ( CE ==1)
       q_out <=  D;
endmodule // FDCPE_1

module FDCPE (Q, C, CE, CLR, D, PRE);
   parameter INIT = 1'b0;
   output Q;
   input  C, CE, CLR, D, PRE;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(CLR or PRE)
     if (CLR)
       q_out <= 0;
     else if (PRE)
       q_out <= 1;
   always @(posedge C )
     if (CE)
	   q_out <= D;
endmodule // FDCPE

module FDCP (Q, C, CLR, D, PRE);
   parameter INIT = 1'b0;
   output Q;
   input  C, CLR, D, PRE;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(CLR or PRE)
     if (CLR)
       q_out <= 0;
     else if (PRE)
       q_out <= 1;
   always @(posedge C )
	 q_out <=  D;
endmodule // FDCP

module FDC (Q, C, CLR, D);
   parameter INIT = 1'b0;
   output Q;
   input  C, CLR, D;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   always @(CLR)
     if (CLR)
       q_out <= 0;
   always @(posedge C)
	 q_out <=  D;
   assign Q = q_out;
endmodule // FDC

module FDDRCPE (Q, C0, C1, CE, CLR, D0, D1, PRE);
   parameter INIT = 1'b0;
   output Q;
   input  C0, C1, CE, CLR, D0, D1, PRE;
   wire   Q;
   reg    q_out;
   reg    q0_out, q1_out;
   reg    C0_tmp, C1_tmp;
   initial begin
      q_out = INIT;
      q0_out = INIT;
      q1_out = INIT;
      C0_tmp = 0;
      C1_tmp = 0;
   end
   assign Q = q_out;
   always @(CLR or PRE)
     if (CLR) begin
        q_out  <= 0;
        q0_out <= 0;
        q1_out <= 0;
        C0_tmp <= 0;
        C1_tmp <= 0;
     end
     else if (PRE) begin
        q_out  <= 1;
        q0_out <= 1;
        q1_out <= 1;
        C0_tmp <= 0;
        C1_tmp <= 0;
     end
   always @(posedge C0) 
     if ( CE) begin
        C0_tmp <=  1;
        q0_out <=  D0;
     end
   always @(posedge C1)
     if ( CE ) begin
        C1_tmp <=  1;
        q1_out <=  D1;
     end
   always @(posedge C0_tmp or posedge C1_tmp )
     if (C1_tmp)
       q_out <=  q1_out;
     else 
       q_out <=  q0_out;
endmodule // FDDRCPE

module FDDRRSE (Q, C0, C1, CE, D0, D1, R, S);
   parameter INIT = 1'b0;
   output Q;
   input  C0, C1, CE, D0, D1, R, S;
   wire   Q;
   reg    q_out;
   reg    q0_out, q1_out;
   reg    C0_tmp, C1_tmp;
   initial begin
      q_out = INIT;
      q0_out = INIT;
      q1_out = INIT;
      C0_tmp = 0;
      C1_tmp = 0;
   end
   assign Q = q_out;
   always @(posedge C0) 
     if (CE == 1 || R == 1 || S == 1) begin
        C0_tmp <=  1;
        //      C0_tmp <= #100 0;
     end
   always @(posedge C1) 
     if (CE == 1 || R == 1 || S == 1) begin
        C1_tmp <=  1;
        //      C1_tmp <= #100 0;
     end
   always @(posedge C0) 
     if (R)
       q0_out <=  0;
     else if (S)
       q0_out <=  1;
     else if (CE)
       q0_out <= D0;
   always @(posedge C1)
     if (R)
       q1_out <=  0;
     else if (S)
       q1_out <=  1;
     else if (CE)
       q1_out <=  D1;
   always @(posedge C0_tmp or posedge C1_tmp )
     if (C1_tmp)
       q_out <=  q1_out;
     else 
       q_out <=  q0_out;
endmodule // FDDRRSE

module FDE_1 (Q, C, CE, D);
   parameter INIT = 1'b0;
   output Q;
   input  C, CE, D;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(negedge C )
     if (CE)
	   q_out <=  D;
endmodule // FDE_1

module FDE (Q, C, CE, D);
   parameter INIT = 1'b0;
   output Q;
   input  C, CE, D;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(posedge C)
	 if (CE)
	   q_out <=  D;
endmodule // FDE

module FDP_1 (Q, C, D, PRE);
   parameter INIT = 1'b1;
   output Q;
   input  C, D, PRE;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(PRE)
     if (PRE)
       q_out <= 1;
   always @(negedge C)
	 q_out <=  D;
endmodule // FDP_1

module FDPE_1 (Q, C, CE, D, PRE);
   parameter INIT = 1'b1;
   output Q;
   input  C, CE, D, PRE;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(PRE)
     if (PRE)
       q_out <= 1;
   always @(negedge C )
     if (CE)
	   q_out <=  D;
endmodule // FDPE_1

module FDPE (Q, C, CE, D, PRE);
   parameter INIT = 1'b1;
   output Q;
   input  C, CE, D, PRE;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(PRE)
     if (PRE)
       q_out <= 1;
   always @(posedge C )
     if (CE)
	   q_out <=  D;
endmodule // FDPE

module FDP (Q, C, D, PRE);
   parameter INIT = 1'b1;
   output Q;
   input  C, D, PRE;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(PRE)
     if (PRE) 
       q_out <= 1;
   always @(posedge C )
	 q_out <= D;
endmodule // FDP

module FDR_1 (Q, C, D, R);
   parameter INIT = 1'b0;
   output Q;
   input  C, D, R;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(negedge C )
     if (R)
	   q_out <=  0;
     else
	   q_out <=  D;
endmodule // FDR_1

module FDRE_1 (Q, C, CE, D, R);
   parameter INIT = 1'b0;
   output Q;
   input  C, CE, D, R;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(negedge C )
	 if (R)
	   q_out <=  0;
	 else if (CE)
	   q_out <=  D;
endmodule // FDRE_1

module FDRE (Q, C, CE, D, R);
   parameter INIT = 1'b0;
   output Q;
   input  C, CE, D, R;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(posedge C )
     if (R)
	   q_out <=  0;
     else if (CE)
	   q_out <=  D;
endmodule // FDRE

module FDRS_1 (Q, C, D, R, S);
   parameter INIT = 1'b0;
   output Q;
   input  C, D, R, S;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(negedge C)
	 if (R)
	   q_out <=  0;
	 else if (S)
	   q_out <=  1;
	 else
	   q_out <=  D;
endmodule // FDRS_1

module FDRSE_1 (Q, C, CE, D, R, S);
   parameter INIT = 1'b0;
   output Q;
   input  C, CE, D, R, S;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(negedge C )
     if (R)
	   q_out <=  0;
     else if (S)
	   q_out <=  1;
     else if (CE)
	   q_out <=  D;
endmodule // FDRSE_1

module FDRSE (Q, C, CE, D, R, S);
   parameter INIT = 1'b0;
   output Q;
   input  C, CE, D, R, S;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(posedge C )
     if (R)
	   q_out <=  0;
     else if (S)
	   q_out <=  1;
     else if (CE)
	   q_out <=  D;
endmodule // FDRSE

module FDRS (Q, C, D, R, S);
   parameter INIT = 1'b0;
   output Q;
   input  C, D, R, S;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(posedge C )
	 if (R)
	   q_out <=  0;
	 else if (S)
	   q_out <=  1;
	 else
	   q_out <=  D;
endmodule // FDRS

module FDR (Q, C, D, R);
   parameter INIT = 1'b0;
   output Q;
   input  C, D, R;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(posedge C )
     if (R)
	   q_out <=  0;
     else
	   q_out <=  D;
endmodule // FDR

module FDS_1 (Q, C, D, S);
   parameter INIT = 1'b1;
   output Q;
   input  C, D, S;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(negedge C )
     if (S)
	   q_out <=  1;
     else
	   q_out <=  D;
endmodule // FDS_1

module FDSE_1 (Q, C, CE, D, S);
   parameter INIT = 1'b1;
   output Q;
   input  C, CE, D, S;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(negedge C )
	 if (S)
	   q_out <=  1;
	 else if (CE)
	   q_out <=  D;
endmodule // FDSE_1

module FDSE (Q, C, CE, D, S);
   parameter INIT = 1'b1;
   output Q;
   input  C, CE, D, S;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(posedge C )
	 if (S)
	   q_out <=  1;
	 else if (CE)
	   q_out <=  D;
endmodule // FDSE

module FDS (Q, C, D, S);
   parameter INIT = 1'b1;
   output Q;
   input  C, D, S;
   wire   Q;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(posedge C )
     if (S)
	   q_out <=  1;
     else
	   q_out <=  D;
endmodule // FDS

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
endmodule // FD


module LD_1 (Q, D, G);
   parameter INIT = 1'b0;
   output Q;
   wire   Q;
   input  D, G;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(D or G)
	 if (!G)
	   q_out <= D;
endmodule // LD_1

module LDC_1 (Q, CLR, D, G);
   parameter INIT = 1'b0;
   output Q;
   wire   Q;
   input  CLR, D, G;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(CLR or D or G)
	 if (CLR)
	   q_out <= 0;
	 else if (!G)
	   q_out <= D;
endmodule // LDC_1

module LDCE_1 (Q, CLR, D, G, GE);
   parameter INIT = 1'b0;
   output Q;
   wire   Q;
   input  CLR, D, G, GE;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(CLR or D or G or GE)
	 if (CLR)
	   q_out <= 0;
	 else if (!G && GE)
	   q_out <= D;
endmodule // LDCE_1

module LDCE (Q, CLR, D, G, GE);
   parameter INIT = 1'b0;
   output Q;
   wire   Q;
   input  CLR, D, G, GE;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(CLR or D or G or GE)
	 if (CLR)
	   q_out <= 0;
	 else if (G && GE)
	   q_out <= D;
endmodule // LDCE

module LDCP_1 (Q, CLR, D, G, PRE);
   parameter INIT = 1'b0;
   output Q;
   wire   Q;
   input  CLR, D, G, PRE;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(CLR or PRE or D or G)
	 if (CLR)
	   q_out <= 0;
	 else if (PRE)
	   q_out <= 1;
	 else if (!G)
	   q_out <= D;
endmodule // LDCP_1

module LDCPE_1 (Q, CLR, D, G, GE, PRE);
   parameter INIT = 1'b0;
   output Q;
   wire   Q;
   input  CLR, D, G, GE, PRE;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(CLR or PRE or D or G or GE)
	 if (CLR)
	   q_out <= 0;
	 else if (PRE)
	   q_out <= 1;
	 else if (!G && GE)
	   q_out <= D;
endmodule // LDCPE_1

module LDCPE (Q, CLR, D, G, GE, PRE);
   parameter INIT = 1'b0;
   output Q;
   wire   Q;
   input  CLR, D, G, GE, PRE;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(CLR or PRE or D or G or GE)
	 if (CLR)
	   q_out <= 0;
	 else if (PRE)
	   q_out <= 1;
	 else if (G && GE)
	   q_out <= D;
endmodule // LDCPE

module LDCP (Q, CLR, D, G, PRE);
   parameter INIT = 1'b0;
   output Q;
   wire   Q;
   input  CLR, D, G, PRE;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(CLR or PRE or D or G)
	 if (CLR)
	   q_out <= 0;
	 else if (PRE)
	   q_out <= 1;
	 else if (G)
	   q_out <= D;
endmodule // LDCP

module LDC (Q, CLR, D, G);
   parameter INIT = 1'b0;
   output Q;
   wire   Q;
   input  CLR, D, G;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(CLR or D or G)
	 if (CLR)
	   q_out <= 0;
	 else if (G)
	   q_out <= D;
endmodule // LDC

module LDE_1 (Q, D, G, GE);
   parameter INIT = 1'b0;
   output Q;
   wire   Q;
   input  D, G, GE;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(D or G or GE)
	 if (!G && GE)
	   q_out <= D;
endmodule // LDE_1

module LDE (Q, D, G, GE);
   parameter INIT = 1'b0;
   output Q;
   wire   Q;
   input  D, G, GE;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(D or G or GE)
	 if (G && GE)
	   q_out <= D;
endmodule // LDE

module LDP_1 (Q, D, G, PRE);
   parameter INIT = 1'b1;
   output Q;
   wire   Q;
   input  D, G, PRE;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(PRE or D or G)
	 if (PRE)
	   q_out <= 1;
	 else if (!G)
	   q_out <= D;
endmodule // LDP_1

module LDPE_1 (Q, D, G, GE, PRE);
   parameter INIT = 1'b1;
   output Q;
   wire   Q;
   input  D, G, GE, PRE;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(PRE or D or G or GE)
	 if (PRE)
	   q_out <= 1;
	 else if (!G && GE)
	   q_out <= D;
endmodule // LDPE_1

module LDPE (Q, D, G, GE, PRE);
   parameter INIT = 1'b1;
   output Q;
   wire   Q;
   input  D, G, GE, PRE;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(PRE or D or G or GE)
	 if (PRE)
	   q_out <= 1;
	 else if (G && GE)
	   q_out <= D;
endmodule // LDPE

module LDP (Q, D, G, PRE);
   parameter INIT = 1'b1;
   output Q;
   wire   Q;
   input  D, G, PRE;
   reg    q_out;
   initial q_out = INIT;
   assign Q = q_out;
   always @(PRE or D or G)
	 if (PRE)
	   q_out <= 1;
	 else if (G)
	   q_out <= D;
endmodule // LDP

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
endmodule // LD


module BUFCF (O, I);
   output O;
   input  I;
   buf B1 (O, I);
endmodule // BUFCF

module BUFE (O, E, I);
   output O;
   input  E, I;
   bufif1 B1 (O, I, E);
endmodule // BUFE

module BUFGCE_1 (O, CE, I);
   output O;
   input  CE, I;
   wire   NCE;
   BUFGMUX_1 B1 (.I0(I),
	             .I1(1'b1),
	             .O(O),
	             .S(NCE));
   INV I1 (.I(CE),
	       .O(NCE));
endmodule // BUFGCE_1

module BUFGCE (O, CE, I);
   output O;
   input  CE, I;
   wire   NCE;
   BUFGMUX B1 (.I0(I),
	           .I1(1'b0),
	           .O(O),
	           .S(NCE));
   INV I1 (.I(CE),
	       .O(NCE));
endmodule // BUFGCE

module BUFGCTRL (O, CE0, CE1, I0, I1, IGNORE0, IGNORE1, S0, S1);
   output O;
   input  CE0;
   input  CE1;
   input  I0;
   input  I1;
   input  IGNORE0;
   input  IGNORE1;
   input  S0;
   input  S1;
   parameter integer INIT_OUT = 0;
   parameter PRESELECT_I0 = "FALSE";
   parameter PRESELECT_I1 = "FALSE";
   reg               O;
   reg               q0, q1;
   reg               q0_enable, q1_enable;
   reg               preselect_i0, preselect_i1;
   reg               task_input_ce0, task_input_ce1, task_input_i0;
   reg               task_input_i1, task_input_ignore0, task_input_ignore1;
   reg               task_input_s0, task_input_s1;
   wire              I0t, I1t;
   // *** parameter checking
   // *** Start here
   assign I0t = INIT_OUT ? ~I0 : I0;
   assign I1t = INIT_OUT ? ~I1 : I1;
   always @(q0 or q1 or I0t or I1t) begin 
	  case ({q1, q0})
        2'b01: O = I0;
        2'b10: O = I1; 
        2'b00: O = INIT_OUT;
	    2'b11: begin
		   q0 = 1'bx;
		   q1 = 1'bx;
		   q0_enable = 1'bx;
		   q1_enable = 1'bx;
		   O = 1'bx;
		end
      endcase
   end
endmodule // BUFGCTRL

module BUFG_LB (
                CLKOUT,
                CLKIN
                );
   output CLKOUT;
   input  CLKIN;
   buf B_CLKOUT (CLKOUT, CLKIN);
endmodule // BUFG_LB

module BUFGMUX_1 (O, I0, I1, S);
   parameter CLK_SEL_TYPE = "SYNC";
   output O;
   input  I0, I1, S;
   reg    q0, q1;
   reg    q0_enable, q1_enable;
   wire   q0_t, q1_t;
   reg    clk_sel_in;
   bufif1 B0 (O, I0, q0_t);
   bufif1 B1 (O, I1, q1_t);
   pullup P1 (O);
   initial
     clk_sel_in = (CLK_SEL_TYPE == "ASYNC") ? 1 : 0;
   assign q0_t = (clk_sel_in) ? ~S : q0;
   assign q1_t = (clk_sel_in) ? S : q1;
   always @(I0 or S or q0_enable)
 	 if (I0)
	   q0 <= !S && q0_enable;
   always @(I1 or S or q1_enable)
 	 if (I1)
	   q1 <= S && q1_enable;
   always @(q1 or I0)
	 if (q1)
	   q0_enable <= 0;
	 else if (!I0)
	   q0_enable <= !q1;
   always @(q0 or I1)
	 if (q0)
	   q1_enable <= 0;
	 else if (!I1)
	   q1_enable <= !q0;
endmodule // BUFGMUX_1

module BUFGMUX_CTRL (O, I0, I1, S);
   output O;
   input  I0;
   input  I1;
   input  S;
   BUFGCTRL bufgctrl_inst (.O(O), .CE0(1'b1), .CE1(1'b1), .I0(I0), .I1(I1), .IGNORE0(1'b0), .IGNORE1(1'b0), .S0(~S), .S1(S));
   defparam bufgctrl_inst.INIT_OUT = 1'b0;
   defparam bufgctrl_inst.PRESELECT_I0 = "TRUE";
   defparam bufgctrl_inst.PRESELECT_I1 = "FALSE";
endmodule // BUFGMUX_CTRL

module BUFGMUX (O, I0, I1, S);
   parameter CLK_SEL_TYPE = "SYNC";
   output O;
   input  I0, I1, S;
   reg    q0, q1;
   reg    q0_enable, q1_enable;
   wire   q0_t, q1_t;
   reg    clk_sel_in;
   bufif1 B0 (O, I0, q0_t);
   bufif1 B1 (O, I1, q1_t);
   pulldown P1 (O);
   initial
     clk_sel_in = (CLK_SEL_TYPE == "ASYNC") ? 1 : 0;
   assign q0_t = (clk_sel_in) ? ~S : q0;
   assign q1_t = (clk_sel_in) ? S : q1;
   always @(I0 or S or q0_enable)
 	 if (!I0)
	   q0 <= !S && q0_enable;
   always @(I1 or S or q1_enable)
 	 if (!I1)
	   q1 <= S && q1_enable;
   always @(q1 or I0)
	 if (q1)
	   q0_enable <= 0;
	 else if (I0)
	   q0_enable <= !q1;
   always @(q0 or I1)
	 if (q0)
	   q1_enable <= 0;
	 else if (I1)
	   q1_enable <= !q0;
endmodule // BUFGMUX

module BUFGMUX_VIRTEX4 (O, I0, I1, S);
   output O;
   input  I0;
   input  I1;
   input  S;
   BUFGCTRL bufgctrl_inst (.O(O), .CE0(1'b1), .CE1(1'b1), .I0(I0), .I1(I1), .IGNORE0(1'b0), .IGNORE1(1'b0), .S0(~S), .S1(S));
   defparam bufgctrl_inst.INIT_OUT = 1'b0;
   defparam bufgctrl_inst.PRESELECT_I0 = "TRUE";
   defparam bufgctrl_inst.PRESELECT_I1 = "FALSE";
endmodule // BUFGMUX_VIRTEX4

module BUFGP (O, I);
   output O;
   input  I;
   buf B1 (O, I);
endmodule // BUFGP

module BUFG (O, I);
   output O;
   input  I;
   buf B1 (O, I);
endmodule // BUFG

module BUFHCE (O, CE, I);
   parameter CE_TYPE = "SYNC";
   parameter integer INIT_OUT = 0;
   output            O;
   input             CE;
   input             I;
   wire              NCE, o_bufg_o, o_bufg1_o;
   reg               CE_TYPE_BINARY;
   reg               INIT_OUT_BINARY;
   BUFGMUX #(.CLK_SEL_TYPE(CE_TYPE)) 
   B1 (.I0(I),
       .I1(1'b0),
       .O(o_bufg_o),
       .S(NCE));
   INV I1 (.I(CE),
           .O(NCE));
   BUFGMUX_1 #(.CLK_SEL_TYPE(CE_TYPE)) 
   B2 (.I0(I),
       .I1(1'b1),
       .O(o_bufg1_o),
       .S(NCE));
   assign O = (INIT_OUT == 1) ? o_bufg1_o : o_bufg_o;
endmodule // BUFHCE

module BUFH (O, I);
   output O;
   input  I;
   buf B1 (O, I);
endmodule // BUFH

// BUFIO2_2CLK
module BUFIO2FB (O, I);
   parameter DIVIDE_BYPASS = "TRUE";      // TRUE, FALSE
   output O; 
   input  I; 
   reg    divclk_bypass_attr;
   // Other signals
   reg    attr_err_flag = 0;
   //----------------------------------------------------------------------
   //------------------------  Output Ports  ------------------------------
   //----------------------------------------------------------------------
   buf buf_o(O, I);
endmodule // BUFIO2FB

// BUFIO2FB
// BUFIO2
module BUFIODQS (O, DQSMASK, I);
   parameter DQSMASK_ENABLE = "FALSE";      // TRUE, FALSE
   output O; 
   input  DQSMASK; 
   input  I; 
   reg    delay_bypass_attr;
   reg    dqsmask_enable_attr;
   wire   o_out;
   // Other signals
   reg    attr_err_flag = 0;
   //----------------------------------------------------------------------
   //------------------------  Output Ports  ------------------------------
   //----------------------------------------------------------------------
   buf buf_o(O, o_out);
   reg    q1, q2;
   wire   clk, dglitch_en;
   assign clk = (dglitch_en == 1'b1) ? I : 1'b0;
   always @(DQSMASK or clk) begin
      if (DQSMASK == 1'b1) q1 = 0;
      else #(300) if (clk == 1) q1 = 1;
   end
   always @(DQSMASK or clk) begin
      if (DQSMASK == 1'b1) q2 = 0;
      else #(400) if (clk == 0) q2 = q1;
   end
   assign dglitch_en = (~q2 | DQSMASK);
   assign o_out = (DQSMASK_ENABLE == "TRUE") ?  clk : I;
endmodule // BUFIODQS

// BUFIODQS
module BUFIO (O, I);
   output O;
   input  I;
   buf B1 (O, I);
endmodule // BUFIO

module BUFMRCE (
                O,
                CE,
                I
                );
   parameter CE_TYPE = "SYNC";
   parameter integer INIT_OUT = 0;
   output            O;
   input             CE;
   input             I;
   wire              NCE, o_bufg_o, o_bufg1_o;
   reg               CE_TYPE_BINARY;
   reg               INIT_OUT_BINARY;
   wire              O_OUT;
   wire              CE_IN;
   wire              I_IN;
   wire              CE_INDELAY;
   wire              I_INDELAY;
   BUFGMUX #(.CLK_SEL_TYPE(CE_TYPE))
   B1 (.I0(I),
       .I1(1'b0),
       .O(o_bufg_o),
       .S(NCE));
   INV I1 (.I(CE),
           .O(NCE));
   BUFGMUX_1 #(.CLK_SEL_TYPE(CE_TYPE))
   B2 (.I0(I),
       .I1(1'b1),
       .O(o_bufg1_o),
       .S(NCE));
   assign O = (INIT_OUT == 1) ? o_bufg1_o : o_bufg_o;
endmodule // BUFMRCE

module BUFMR (
              O,
              I
              );
   output O;
   input  I;
   buf B1 (O, I);
endmodule // BUFMR

module BUFT (O, I, T);
   output O;
   input  I, T;
   bufif0 T1 (O, I, T);
endmodule // BUFT

module BUF (O, I);
   output O;
   input  I;
   buf B1 (O, I);
endmodule // BUF

