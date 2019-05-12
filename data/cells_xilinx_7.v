
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
    input CE0;
    input CE1;
    input I0;
    input I1;
    input IGNORE0;
    input IGNORE1;
    input S0;
    input S1;
    parameter INIT_OUT = 0;
    parameter PRESELECT_I0 = "FALSE";
    parameter PRESELECT_I1 = "FALSE";
// *** parameter checking
// *** input enable for i0

endmodule // BUFGCTRL

module BUFG_LB (
  CLKOUT,

  CLKIN
);


  output CLKOUT;

  input CLKIN;


  wire CLKOUT_OUT;

  wire CLKIN_IN;

  wire CLKIN_INDELAY;

  buf B_CLKOUT (CLKOUT, CLKIN);

endmodule // BUFG_LB

module BUFGMUX_1 (O, I0, I1, S);

    output O;

    input  I0, I1, S;

    reg    O;
    

        always @(I0 or I1 or S) begin

            if (S)
                O <= I1;

            else
                O <= I0;

        end

endmodule // BUFGMUX_1

module BUFGMUX_CTRL (O, I0, I1, S);

    output O;
    input  I0;
    input  I1;
    input  S;

endmodule // BUFGMUX_CTRL

module BUFGMUX (O, I0, I1, S);

    output O;

    input  I0, I1, S;

    reg    O;
    
        always @(I0 or I1 or S) begin

            if (S)
                O <= I1;

            else
                O <= I0;

        end

endmodule // BUFGMUX

module BUFGMUX_VIRTEX4 (O, I0, I1, S);

    output O;
    input  I0;
    input  I1;
    input  S;

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

    output O;

    input  CE;
    input  I;

    wire   NCE, o_bufg_o, o_bufg1_o;
                                                                                  
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

module BUFIO2_2CLK (DIVCLK, IOCLK, SERDESSTROBE, I, IB);



  parameter integer DIVIDE = 2;        // {2..8}


    output DIVCLK; 
    output IOCLK; 
    output SERDESSTROBE; 

    input I; 
    input IB; 

// Output signals 
    reg  divclk_out=0, ioclk_out=0, serdesstrobe_out=0;

// Counters and Flags
    reg [2:0] ce_count = 0;
    reg [2:0] edge_count = 0;
    reg [2:0] RisingEdgeCount = 0;
    reg [2:0] FallingEdgeCount = 0;
    reg TriggerOnRise = 0; // FP

    reg allEqual=0, RisingEdgeMatch=0, FallingEdgeMatch=0,  match=0, nmatch=0;

    reg divclk_int=0;
    reg i1_int=0, i2_int=0;

// Attribute settings

// Other signals
    reg attr_err_flag = 0;

//-----------------------------------------------------------------------------------

// =====================
// clock doubler
// =====================
    always @(posedge I) begin
        i1_int = 1;
        #100 i1_int = 0;
    end

    always @(posedge IB) begin
        i2_int = 1;
        #100  i2_int = 0;
    end

    assign doubled_clk_int = i1_int | i2_int;


// =====================
// Count the rising edges of the clk
// =====================
// CR 512001  -- for various DIVIDE widths, the count is set differently to match the hw startup      
   generate
      case(DIVIDE)
         2,4,6,8 : begin

                    always @(posedge doubled_clk_int) begin
                       if($time < 1 )
                            edge_count <= DIVIDE-1;   //001  for 5 and 7
                       else if (allEqual)
                            edge_count <= 3'b000;
                       else
                            edge_count <= edge_count + 1;
                    end
         end
         3,5,7 : begin
               //for 1, 3, 5 and 7 below

                    always @(posedge doubled_clk_int) begin
                       if($time < 1 )
                            edge_count <= 3'b001;   //001  for 5 and 7
                       else if (allEqual)
                            edge_count <= 3'b000;
                       else
                            edge_count <= edge_count + 1;
                    end
        end
      endcase
    endgenerate
 
//  Generate synchronous reset after DIVIDE number of counts
    always @(edge_count) 
        if (edge_count == ce_count) 
           allEqual = 1;
        else
          allEqual = 0;

// =====================
// Generate IOCE
// =====================
    always @(posedge doubled_clk_int)
        serdesstrobe_out <= allEqual;
 
// =====================
// Generate IOCLK
// =====================
    always @(I)
        ioclk_out <= I;
 
// =====================
// Generate Divided Clock
// =====================
    always @(edge_count)
       if (edge_count == RisingEdgeCount)
           RisingEdgeMatch = 1;
       else
           RisingEdgeMatch = 0;

    always @(edge_count)
       if (edge_count == FallingEdgeCount)
           FallingEdgeMatch = 1;
       else
           FallingEdgeMatch = 0;

    always @(posedge doubled_clk_int)
          match <= RisingEdgeMatch | (match & ~FallingEdgeMatch);

    always @(negedge doubled_clk_int)
         if(~TriggerOnRise) 
            nmatch <= match; 
         else 
            nmatch <= 0;   

    always@(match or nmatch) divclk_int = match | nmatch;

// IR 497760 fix
    always @(divclk_int or doubled_clk_int)
         divclk_out = (DIVIDE == 1)? ioclk_out : divclk_int;




    assign DIVCLK  = divclk_out;
    assign IOCLK   = ioclk_out;
    assign SERDESSTROBE = serdesstrobe_out;

endmodule
 // BUFIO2_2CLK
module BUFIO2FB (O, I);

    parameter DIVIDE_BYPASS = "TRUE";      // TRUE, FALSE

    output O; 
    input I; 

    reg divclk_bypass_attr;

// Other signals
    reg attr_err_flag = 0;

//----------------------------------------------------------------------
//------------------------  Output Ports  ------------------------------
//----------------------------------------------------------------------
    buf buf_o(O, I);

    initial begin
//-------------------------------------------------
//----- DIVIDE_BYPASS  Check
//-------------------------------------------------
        case (DIVIDE_BYPASS)
            "TRUE" : divclk_bypass_attr <= 1'b1;
            "FALSE" :divclk_bypass_attr <= 1'b0;
            default : begin
                      $display("Attribute Syntax Error : The attribute DIVIDE_BYPASS on BUFIO2FB instance %m is set to %s.  Legal values for this attribute are TRUE or FALSE", DIVIDE_BYPASS);
                      attr_err_flag = 1;
                      end
        endcase // (DIVIDE_BYPASS)

    end  // initial begin

endmodule
 // BUFIO2FB
module BUFIO2 (DIVCLK, IOCLK, SERDESSTROBE, I);



  parameter DIVIDE_BYPASS = "TRUE";    // TRUE, FALSE
  parameter integer DIVIDE = 1;        // {1..8}
  parameter I_INVERT = "FALSE";        // TRUE, FALSE
  parameter USE_DOUBLER = "FALSE";     // TRUE, FALSE


    output DIVCLK; 
    output IOCLK; 
    output SERDESSTROBE; 

    input I; 

// Output signals 
    reg  divclk_out=0, ioclk_out=0, serdesstrobe_out=0;

// Counters and Flags
    reg [2:0] ce_count = 0;
    reg [2:0] edge_count = 0;
    reg [2:0] RisingEdgeCount = 0;
    reg [2:0] FallingEdgeCount = 0;
    reg TriggerOnRise = 0; // FP

    reg allEqual=0, RisingEdgeMatch=0, FallingEdgeMatch=0,  match=0, nmatch=0;

    reg divclk_int=0;

    reg I_int;
    reg i1_int,  i2_int;
    wire doubled_clk_int;

    wire div1_clk; 

// Attribute settings

// Other signals
    reg attr_err_flag = 0;
    


// Optional inverter for I 
    generate
      case (I_INVERT)
         "FALSE" : always @(I)  I_int <= I;
         "TRUE"  : always @(I)  I_int <= ~I;
      endcase
    endgenerate

     localparam divclk_bypass_attr = (DIVIDE_BYPASS == "TRUE") ? 1'b1 : 1'b0;
     localparam Ivert_attr = (I_INVERT == "TRUE") ? 1'b1 : 1'b0;
     localparam use_doubler_attr = (USE_DOUBLER == "TRUE") ? 1'b1 : 1'b0;


    generate if  (USE_DOUBLER == "TRUE")
       begin
      // =====================
      // clock doubler
      // =====================
          always @(posedge I_int) begin
              i1_int = 1;
              #100 i1_int = 0;
          end

          always @(negedge I_int) begin
              i2_int = 1;
              #100  i2_int = 0;
          end

          assign doubled_clk_int = i1_int | i2_int;
       end
     else 
       assign doubled_clk_int = I_int; 
     endgenerate

// CR 561858  -- for various DIVIDE widths, the count is set differently to match the BUFIO2_2CLK's CR 512001  
// =====================
// Count the rising edges of the clk
// =====================
//    always @(posedge doubled_clk_int) begin
//       if(allEqual || $time < 1) 
//           edge_count <= 3'b000;
//        else
//           edge_count <= edge_count + 1; 
//    end 
   generate
      case(DIVIDE)
         2,4,6,8 : begin

                    always @(posedge doubled_clk_int) begin
                       if($time < 1 )
                            edge_count <= DIVIDE-1;   //001  for 5 and 7
                       else if (allEqual)
                            edge_count <= 3'b000;
                       else
                            edge_count <= edge_count + 1;
                    end
         end
         3,5,7 : begin
               //for 1, 3, 5 and 7 below

                    always @(posedge doubled_clk_int) begin
                       if($time < 1 )
                            edge_count <= 3'b001;   //001  for 5 and 7
                       else if (allEqual)
                            edge_count <= 3'b000;
                       else
                            edge_count <= edge_count + 1;
                    end
        end
      endcase
    endgenerate

//  Generate synchronous reset after DIVIDE number of counts
    always @(edge_count) 
        if (edge_count == ce_count) 
           allEqual = 1;
        else
          allEqual = 0;

// =====================
// Generate IOCE
// =====================
    always @(posedge doubled_clk_int)
        serdesstrobe_out <= allEqual;
 
// =====================
// Generate IOCLK
// =====================
    always @(I_int)
        ioclk_out <= I_int;
 
// =====================
// Generate Divided Clock
// =====================
    always @(edge_count)
       if (edge_count == RisingEdgeCount)
           RisingEdgeMatch = 1;
       else
           RisingEdgeMatch = 0;

    always @(edge_count)
       if (edge_count == FallingEdgeCount)
           FallingEdgeMatch = 1;
       else
           FallingEdgeMatch = 0;

    always @(posedge doubled_clk_int)
          match <= RisingEdgeMatch | (match & ~FallingEdgeMatch);

    always @(negedge doubled_clk_int)
         if(~TriggerOnRise) 
            nmatch <= match; 
         else 
            nmatch <= 0;   

    always@(match or nmatch) divclk_int = match | nmatch;

    always @(divclk_int or I_int)
         divclk_out = (divclk_bypass_attr | (DIVIDE == 1))? I_int : divclk_int;



    assign DIVCLK  = divclk_out;
    assign IOCLK   = ioclk_out;
    assign SERDESSTROBE = serdesstrobe_out;

endmodule
 // BUFIO2
module BUFIODQS (O, DQSMASK, I);

    parameter DQSMASK_ENABLE = "FALSE";      // TRUE, FALSE

    output O; 
    input DQSMASK; 
    input I; 

endmodule
 // BUFIODQS
module BUFIO (O, I);

    output O;
    input  I;

endmodule // BUFIO

module BUFMRCE (
  O,

  CE,
  I
);

  parameter CE_TYPE = "SYNC";
  parameter integer INIT_OUT = 0;


  output O;

  input CE;
  input I;

  wire   NCE, o_bufg_o, o_bufg1_o;

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

  input I;


  buf B1 (O, I);

endmodule // BUFMR

module BUFPLL_MCB (
  IOCLK0,
  IOCLK1,
  LOCK,
  SERDESSTROBE0,
  SERDESSTROBE1,

  GCLK,
  LOCKED,
  PLLIN0,
  PLLIN1
);



    parameter integer DIVIDE = 2;        // {1..8}
    parameter LOCK_SRC = "LOCK_TO_0";


    output IOCLK0;
    output IOCLK1;
    output LOCK;
    output SERDESSTROBE0;
    output SERDESSTROBE1;

    input GCLK;
    input LOCKED;
    input PLLIN0;
    input PLLIN1;


// Output signals 
    reg  ioclk0_out = 0, ioclk1_out = 0, lock_out = 0, serdesstrobe0_out = 0, serdesstrobe1_out = 0;

// Counters and Flags
    reg [2:0] ce_count = 0;
    reg [2:0] edge_count = 0;
    reg [2:0] RisingEdgeCount = 0;
    reg [2:0] FallingEdgeCount = 0;
    reg TriggerOnRise = 0;
    reg divclk_int;

    reg allEqual, RisingEdgeMatch, FallingEdgeMatch,  match, nmatch;

// Attribute settings

// Other signals
    reg attr_err_flag = 0;

    localparam lock_src_0_attr = (LOCK_SRC ==  "LOCK_TO_0" ) ? 1'b1 : 1'b0;
    localparam lock_src_1_attr = (LOCK_SRC ==  "LOCK_TO_1" ) ? 1'b1 : 1'b0;


// =====================
// Count the rising edges of the clk
// =====================
    generate if (LOCK_SRC == "LOCK_TO_0") 
       begin 
          always @(posedge PLLIN0) 
             if(allEqual) 
                 edge_count <= 3'b000;
              else
                 edge_count <= edge_count + 1; 
       end 
    else 
       begin 
          always @(posedge PLLIN1)
             if(allEqual) 
                 edge_count <= 3'b000;
              else
                 edge_count <= edge_count + 1; 
       end 
    endgenerate
          
//  Generate synchronous reset after DIVIDE number of counts
    always @(edge_count) 
        if (edge_count == ce_count) 
           allEqual = 1;
        else
          allEqual = 0;

// =====================
// Generate SERDESSTROBE 
// =====================
    generate if(LOCK_SRC == "LOCK_TO_0") 
     begin 
       always @(posedge PLLIN0)
           serdesstrobe0_out <= allEqual;
       always @(posedge PLLIN1)
           serdesstrobe1_out <= serdesstrobe0_out;
     end
    else 
     begin 
       always @(posedge PLLIN1)
           serdesstrobe1_out <= allEqual;
       always @(posedge PLLIN0)
           serdesstrobe0_out <= serdesstrobe1_out;
     end
    endgenerate
 
// =====================
// Generate divided clk 
// =====================
    always @(edge_count)
       if (edge_count == RisingEdgeCount)
           RisingEdgeMatch = 1;
       else
           RisingEdgeMatch = 0;

    always @(edge_count)
       if (edge_count == FallingEdgeCount)
           FallingEdgeMatch = 1;
       else
           FallingEdgeMatch = 0;

    generate if(LOCK_SRC == "LOCK_TO_0") 
       begin 
          always @(posedge PLLIN0)
             match <= RisingEdgeMatch | (match & ~FallingEdgeMatch);

          always @(negedge PLLIN0)
             if(~TriggerOnRise) 
                  nmatch <= match; 
               else 
                  nmatch <= 0;   
       end 
    else 
       begin 
          always @(posedge PLLIN1)
             match <= RisingEdgeMatch | (match & ~FallingEdgeMatch);

          always @(negedge PLLIN1)
             if(~TriggerOnRise) 
                  nmatch <= match; 
               else 
                  nmatch <= 0;   
       end 
    endgenerate

    always@(match or nmatch) divclk_int = match | nmatch;
// =====================
// Generate IOCLKs 
// =====================

    always @(PLLIN0)
         ioclk0_out = PLLIN0;

    always @(PLLIN1)
         ioclk1_out = PLLIN1;

// =====================
// Generate LOCK
// =====================
    always @(LOCKED)
         lock_out <= LOCKED;


    assign IOCLK0 = ioclk0_out;
    assign IOCLK1 = ioclk1_out;
    assign LOCK   = lock_out;
    assign SERDESSTROBE0 = serdesstrobe0_out;
    assign SERDESSTROBE1 = serdesstrobe1_out;

endmodule
 // BUFPLL_MCB
module BUFPLL (IOCLK, LOCK, SERDESSTROBE, GCLK, LOCKED, PLLIN);



    parameter integer DIVIDE = 1;        // {1..8}
    parameter ENABLE_SYNC = "TRUE";


    output IOCLK;
    output LOCK;
    output SERDESSTROBE;

    input GCLK;
    input LOCKED;
    input PLLIN;


// Output signals 
    reg  ioclk_out = 0, lock_out = 0, serdesstrobe_out = 0;

// Counters and Flags
    reg [2:0] ce_count = 0;
    reg [2:0] edge_count = 0;
    reg [2:0] RisingEdgeCount = 0;
    reg [2:0] FallingEdgeCount = 0;
    reg TriggerOnRise = 0; 
    reg divclk_int;

    reg allEqual, RisingEdgeMatch, FallingEdgeMatch,  match, nmatch;

    reg lock_src_indepn_attr = 0, lock_src_0_attr = 0, lock_src_1_attr= 0;

    reg  enable_sync_strobe_out = 0, strobe_out = 0;

// Attribute settings

// Other signals
    reg attr_err_flag = 0;

// =====================
// Count the rising edges of the clk
// =====================
    always @(posedge PLLIN) begin
       if(allEqual) 
           edge_count <= 3'b000;
        else
           edge_count <= edge_count + 1; 
     end 
          
//  Generate synchronous reset after DIVIDE number of counts
    always @(edge_count) 
        if (edge_count == ce_count) 
           allEqual = 1;
        else
          allEqual = 0;

// =======================================
// Generate SERDESSTROBE when ENABLE_SYNC 
// =======================================
     generate
      case(DIVIDE)
        1, 2, 3, 4, 5, 6, 7, 8 : begin
             always @(posedge GCLK)
             begin
                   enable_sync_strobe_out <= 1'b1;
             end
             end
      endcase
    endgenerate
 
// =====================
// Generate divided clk 
// =====================
    always @(edge_count)
       if (edge_count == RisingEdgeCount)
           RisingEdgeMatch = 1;
       else
           RisingEdgeMatch = 0;

    always @(edge_count)
       if (edge_count == FallingEdgeCount)
           FallingEdgeMatch = 1;
       else
           FallingEdgeMatch = 0;

    always @(posedge PLLIN)
       match <= RisingEdgeMatch | (match & ~FallingEdgeMatch);

    always @(negedge PLLIN)
       if(~TriggerOnRise) 
            nmatch <= match; 
         else 
            nmatch <= 0;   

    always@(match or nmatch) divclk_int = match | nmatch;

    always @(PLLIN)
         ioclk_out <= PLLIN;

// =====================
// Generate strobe_out 
// =====================
    always @(posedge PLLIN)
         strobe_out <= allEqual;

// =========================
// Generate serdesstrobe_out
// =========================

    always @(strobe_out or enable_sync_strobe_out)
         serdesstrobe_out = (ENABLE_SYNC == "TRUE")? enable_sync_strobe_out : strobe_out;

// =====================
// Generate LOCK 
// =====================
    always @(LOCKED)
         lock_out <= LOCKED;



    assign IOCLK = ioclk_out;
    assign LOCK  = lock_out;
    assign SERDESSTROBE = serdesstrobe_out;


endmodule
 // BUFPLL
module BUFR (O, CE, CLR, I);

    output O;

    input CE;
    input CLR;
    input I;

    parameter BUFR_DIVIDE = "BYPASS";
    parameter SIM_DEVICE = "VIRTEX4";

    
endmodule


module BUF (O, I);

    output O;

    input  I;

	buf B1 (O, I);

endmodule // BUF
