// Based on cyclonev_atoms.v from Quartus II 14.0.0 Build 200 06/17/2014

// ==========================================================================================

module dffeas (d, clk, ena, clrn, prn, aload, asdata, sclr, sload, devclrn, devpor, q );
   // GLOBAL PARAMETER DECLARATION
   parameter power_up = "DONT_CARE";
   parameter is_wysiwyg = "false";
   parameter dont_touch = "false";


   parameter x_on_violation = "on";
   parameter lpm_type = "dffeas";

   input d;
   input clk;
   input ena;
   input clrn;
   input prn;
   input aload; 
   input asdata;  
   input sclr; 
   input sload; 
   input devclrn; 
   input devpor; 

   output reg q = 0;

   always @(posedge clk) begin
      if (ena == 1'b1) begin
         if (sclr == 1'b1)
           q <= 0;
         else if (aload == 1'b1)
           q <= asdata;
         else if (sload == 1'b1)
           q <= asdata;
         else
           q <= d;
      end
   end

endmodule


module cyclonev_lcell_comb (
	                        dataa, datab, datac, datad, datae, dataf, datag, cin, sharein,
	                        combout, sumout, cout, shareout
                            );

   input dataa, datab, datac, datad, datae, dataf, datag, cin, sharein;
   output reg combout = 0, sumout = 0, cout = 0, shareout = 0;

   parameter lut_mask = 64'hFFFFFFFFFFFFFFFF;
   parameter shared_arith = "off";
   parameter extended_lut = "off";
   parameter dont_touch = "off";
   parameter lpm_type = "cyclonev_lcell_comb";

   localparam ishared_arith = shared_arith == "on";
   localparam iextended_lut = extended_lut == "on";

   localparam [15:0] f0_mask = lut_mask[15:0];
   localparam [15:0] f1_mask = lut_mask[31:16];
   localparam [15:0] f2_mask = lut_mask[47:32];
   localparam [15:0] f3_mask = lut_mask[63:48];

   reg        f0_out, f1_out, f2_out, f3_out;
   reg        g0_out, g1_out;
   reg        f2_input3, f2_f;
   reg        adder_input2;

   // 4-input LUT function
   function lut4;
      input [15:0] mask;
      input        dataa;
      input        datab;
      input        datac;
      input        datad;
      begin
         lut4 = datad ? ( datac ? ( datab ? ( dataa ? mask[15] : mask[14])
                                    : ( dataa ? mask[13] : mask[12]))
                          : ( datab ? ( dataa ? mask[11] : mask[10])
                              : ( dataa ? mask[ 9] : mask[ 8])))
           : ( datac ? ( datab ? ( dataa ? mask[ 7] : mask[ 6])
                         : ( dataa ? mask[ 5] : mask[ 4]))
               : ( datab ? ( dataa ? mask[ 3] : mask[ 2])
                   : ( dataa ? mask[ 1] : mask[ 0])));
      end
   endfunction

   // 5-input LUT function
   function lut5;
      input [31:0] mask;
      input        dataa;
      input        datab;
      input        datac;
      input        datad;
      input        datae;
      reg          e0_lut;
      reg          e1_lut;
      reg [15:0]   e0_mask;
      reg [31:16]  e1_mask;
      begin
         e0_mask = mask[15:0];
         e1_mask = mask[31:16];
         e0_lut = lut4(e0_mask, dataa, datab, datac, datad);
         e1_lut = lut4(e1_mask, dataa, datab, datac, datad);
      end
   endfunction

   // 6-input LUT function
   function lut6;
      input [63:0] mask;
      input        dataa;
      input        datab;
      input        datac;
      input        datad;
      input        datae;
      input        dataf;
      reg          f0_lut;
      reg          f1_lut;
      reg [31:0]   f0_mask;
      reg [63:32]  f1_mask ;
      begin
         f0_mask = mask[31:0];
         f1_mask = mask[63:32];
         lut6 = mask[{dataf, datae, datad, datac, datab, dataa}];
      end
   endfunction

   always @(datag or dataf or datae or datad or datac or
            datab or dataa or cin or sharein)
     begin
        // check for extended LUT mode
        if (iextended_lut == 1)
          f2_input3 = datag;
        else
          f2_input3 = datac;

        f0_out = lut4(f0_mask, dataa, datab, datac, datad);
        f1_out = lut4(f1_mask, dataa, datab, f2_input3, datad);
        f2_out = lut4(f2_mask, dataa, datab, datac, datad);
        f3_out = lut4(f3_mask, dataa, datab, f2_input3, datad);

        // combout is the 6-input LUT
        if (iextended_lut == 1)
          begin
             if (datae == 1'b0)
               begin
                  g0_out = f0_out;
                  g1_out = f2_out;
               end
             else if (datae == 1'b1)
               begin
                  g0_out = f1_out;
                  g1_out = f3_out;
               end
             else
               begin
                  if (f0_out == f1_out)
                    g0_out = f0_out;
                  else
                    g0_out = 1'bX;

                  if (f2_out == f3_out)
                    g1_out = f2_out;
                  else
                    g1_out = 1'bX;
               end

             if (dataf == 1'b0)
               combout = g0_out;
             else if ((dataf == 1'b1) || (g0_out == g1_out))
               combout = g1_out;
             else
               combout = 1'bX;
          end
        else
          combout = lut6(lut_mask, dataa, datab, datac, datad, datae, dataf);

        // check for shareed arithmetic mode
        if (ishared_arith == 1)
          adder_input2 = sharein;
        else
          begin
             f2_f = lut4(f2_mask, dataa, datab, datac, dataf);
             adder_input2 = !f2_f;
          end

        // sumout & cout
        sumout = cin ^ f0_out ^ adder_input2;
        cout = (cin & f0_out) | (cin & adder_input2) | (f0_out & adder_input2);
        shareout = f2_out;
     end

endmodule

module cyclonev_clkena    (
                           inclk,
                           ena,
                           enaout,
                           outclk);

   // leda G_521_3_B off
   parameter    clock_type    =    "auto";
   parameter    ena_register_mode    =    "always enabled";
   parameter    lpm_type    =    "cyclonev_clkena";
   parameter    ena_register_power_up    =    "high";
   parameter    disable_mode    =    "low";
   parameter    test_syn    =    "high";
   // leda G_521_3_B on

   input    inclk;
   input    ena;
   output   enaout;
   output   outclk;

   assign outclk = ena ? inclk : 1'b0;
   assign enaout = ena;

endmodule //cyclonev_clkena


// ==========================================================================================

module cyclonev_io_ibuf (i, ibar, dynamicterminationcontrol, o);

   parameter differential_mode = 0;
   parameter bus_hold = 0;
   parameter simulate_z_as = 0;
   parameter lpm_type = 0;

   input i;
   input ibar;
   input dynamicterminationcontrol;
   output o;

   assign o = i;

endmodule

// ==========================================================================================

module cyclonev_io_obuf (i, oe, dynamicterminationcontrol, seriesterminationcontrol, parallelterminationcontrol, devoe, o, obar);

   parameter open_drain_output = 0;
   parameter bus_hold = 0;
   parameter shift_series_termination_control = 0;
   parameter sim_dynamic_termination_control_is_connected = 0;
   parameter lpm_type = 0;

   input i;
   input oe;
   input devoe;
   input dynamicterminationcontrol;
   input [15:0] seriesterminationcontrol;
   input [15:0] parallelterminationcontrol;
   output       o;
   output       obar;

   assign o = i, obar = ~i;

endmodule

module cyclone10lp_lcell_comb (
	                        dataa, datab, datac, datad, datae, dataf, datag, cin, sharein,
	                        combout, sumout, cout, shareout
                            );

   input dataa, datab, datac, datad, datae, dataf, datag, cin, sharein;
   output reg combout = 0, sumout = 0, cout = 0, shareout = 0;

   parameter lut_mask = 64'hFFFFFFFFFFFFFFFF;
   parameter shared_arith = "off";
   parameter extended_lut = "off";
   parameter dont_touch = "off";
   parameter lpm_type = "cyclone10lp_lcell_comb";

   localparam ishared_arith = shared_arith == "on";
   localparam iextended_lut = extended_lut == "on";

   localparam [15:0] f0_mask = lut_mask[15:0];
   localparam [15:0] f1_mask = lut_mask[31:16];
   localparam [15:0] f2_mask = lut_mask[47:32];
   localparam [15:0] f3_mask = lut_mask[63:48];

   reg        f0_out, f1_out, f2_out, f3_out;
   reg        g0_out, g1_out;
   reg        f2_input3, f2_f;
   reg        adder_input2;

   // 4-input LUT function
   function lut4;
      input [15:0] mask;
      input        dataa;
      input        datab;
      input        datac;
      input        datad;
      begin
         lut4 = datad ? ( datac ? ( datab ? ( dataa ? mask[15] : mask[14])
                                    : ( dataa ? mask[13] : mask[12]))
                          : ( datab ? ( dataa ? mask[11] : mask[10])
                              : ( dataa ? mask[ 9] : mask[ 8])))
           : ( datac ? ( datab ? ( dataa ? mask[ 7] : mask[ 6])
                         : ( dataa ? mask[ 5] : mask[ 4]))
               : ( datab ? ( dataa ? mask[ 3] : mask[ 2])
                   : ( dataa ? mask[ 1] : mask[ 0])));
      end
   endfunction

   // 5-input LUT function
   function lut5;
      input [31:0] mask;
      input        dataa;
      input        datab;
      input        datac;
      input        datad;
      input        datae;
      reg          e0_lut;
      reg          e1_lut;
      reg [15:0]   e0_mask;
      reg [31:16]  e1_mask;
      begin
         e0_mask = mask[15:0];
         e1_mask = mask[31:16];
         e0_lut = lut4(e0_mask, dataa, datab, datac, datad);
         e1_lut = lut4(e1_mask, dataa, datab, datac, datad);
      end
   endfunction

   // 6-input LUT function
   function lut6;
      input [63:0] mask;
      input        dataa;
      input        datab;
      input        datac;
      input        datad;
      input        datae;
      input        dataf;
      reg          f0_lut;
      reg          f1_lut;
      reg [31:0]   f0_mask;
      reg [63:32]  f1_mask ;
      begin
         f0_mask = mask[31:0];
         f1_mask = mask[63:32];
         lut6 = mask[{dataf, datae, datad, datac, datab, dataa}];
      end
   endfunction

   always @(datag or dataf or datae or datad or datac or
            datab or dataa or cin or sharein)
     begin
        // check for extended LUT mode
        if (iextended_lut == 1)
          f2_input3 = datag;
        else
          f2_input3 = datac;

        f0_out = lut4(f0_mask, dataa, datab, datac, datad);
        f1_out = lut4(f1_mask, dataa, datab, f2_input3, datad);
        f2_out = lut4(f2_mask, dataa, datab, datac, datad);
        f3_out = lut4(f3_mask, dataa, datab, f2_input3, datad);

        // combout is the 6-input LUT
        if (iextended_lut == 1)
          begin
             if (datae == 1'b0)
               begin
                  g0_out = f0_out;
                  g1_out = f2_out;
               end
             else if (datae == 1'b1)
               begin
                  g0_out = f1_out;
                  g1_out = f3_out;
               end
             else
               begin
                  if (f0_out == f1_out)
                    g0_out = f0_out;
                  else
                    g0_out = 1'bX;

                  if (f2_out == f3_out)
                    g1_out = f2_out;
                  else
                    g1_out = 1'bX;
               end

             if (dataf == 1'b0)
               combout = g0_out;
             else if ((dataf == 1'b1) || (g0_out == g1_out))
               combout = g1_out;
             else
               combout = 1'bX;
          end
        else
          combout = lut6(lut_mask, dataa, datab, datac, datad, datae, dataf);

        // check for shareed arithmetic mode
        if (ishared_arith == 1)
          adder_input2 = sharein;
        else
          begin
             f2_f = lut4(f2_mask, dataa, datab, datac, dataf);
             adder_input2 = !f2_f;
          end

        // sumout & cout
        sumout = cin ^ f0_out ^ adder_input2;
        cout = (cin & f0_out) | (cin & adder_input2) | (f0_out & adder_input2);
        shareout = f2_out;
     end

endmodule

// ==========================================================================================

module cyclone10lp_io_ibuf (i, ibar, dynamicterminationcontrol, o);

   parameter differential_mode = 0;
   parameter bus_hold = 0;
   parameter simulate_z_as = 0;
   parameter lpm_type = 0;

   input i;
   input ibar;
   input dynamicterminationcontrol;
   output o;

   assign o = i;

endmodule

// ==========================================================================================

module cyclone10lp_io_obuf (i, oe, dynamicterminationcontrol, seriesterminationcontrol, parallelterminationcontrol, devoe, o, obar);

   parameter open_drain_output = 0;
   parameter bus_hold = 0;
   parameter shift_series_termination_control = 0;
   parameter sim_dynamic_termination_control_is_connected = 0;
   parameter lpm_type = 0;

   input i;
   input oe;
   input devoe;
   input dynamicterminationcontrol;
   input [15:0] seriesterminationcontrol;
   input [15:0] parallelterminationcontrol;
   output       o;
   output       obar;

   assign o = i, obar = ~i;

endmodule

module cyclone10lp_clkena    (
                           inclk,
                           ena,
                           enaout,
                           outclk);

   // leda G_521_3_B off
   parameter    clock_type    =    "auto";
   parameter    ena_register_mode    =    "always enabled";
   parameter    lpm_type    =    "cyclone10lp_clkena";
   parameter    ena_register_power_up    =    "high";
   parameter    disable_mode    =    "low";
   parameter    test_syn    =    "high";
   // leda G_521_3_B on

   input    inclk;
   input    ena;
   output   enaout;
   output   outclk;

   assign outclk = ena ? inclk : 1'b0;
   assign enaout = ena;

endmodule //cyclone10lp_clkena

module cyclone10gx_lcell_comb (
	                        dataa, datab, datac, datad, datae, dataf, datag, cin, sharein,
	                        combout, sumout, cout, shareout
                            );

   input dataa, datab, datac, datad, datae, dataf, datag, cin, sharein;
   output reg combout = 0, sumout = 0, cout = 0, shareout = 0;

   parameter lut_mask = 64'hFFFFFFFFFFFFFFFF;
   parameter shared_arith = "off";
   parameter extended_lut = "off";
   parameter dont_touch = "off";
   parameter lpm_type = "cyclone10gx_lcell_comb";

   localparam ishared_arith = shared_arith == "on";
   localparam iextended_lut = extended_lut == "on";

   localparam [15:0] f0_mask = lut_mask[15:0];
   localparam [15:0] f1_mask = lut_mask[31:16];
   localparam [15:0] f2_mask = lut_mask[47:32];
   localparam [15:0] f3_mask = lut_mask[63:48];

   reg        f0_out, f1_out, f2_out, f3_out;
   reg        g0_out, g1_out;
   reg        f2_input3, f2_f;
   reg        adder_input2;

   // 4-input LUT function
   function lut4;
      input [15:0] mask;
      input        dataa;
      input        datab;
      input        datac;
      input        datad;
      begin
         lut4 = datad ? ( datac ? ( datab ? ( dataa ? mask[15] : mask[14])
                                    : ( dataa ? mask[13] : mask[12]))
                          : ( datab ? ( dataa ? mask[11] : mask[10])
                              : ( dataa ? mask[ 9] : mask[ 8])))
           : ( datac ? ( datab ? ( dataa ? mask[ 7] : mask[ 6])
                         : ( dataa ? mask[ 5] : mask[ 4]))
               : ( datab ? ( dataa ? mask[ 3] : mask[ 2])
                   : ( dataa ? mask[ 1] : mask[ 0])));
      end
   endfunction

   // 5-input LUT function
   function lut5;
      input [31:0] mask;
      input        dataa;
      input        datab;
      input        datac;
      input        datad;
      input        datae;
      reg          e0_lut;
      reg          e1_lut;
      reg [15:0]   e0_mask;
      reg [31:16]  e1_mask;
      begin
         e0_mask = mask[15:0];
         e1_mask = mask[31:16];
         e0_lut = lut4(e0_mask, dataa, datab, datac, datad);
         e1_lut = lut4(e1_mask, dataa, datab, datac, datad);
      end
   endfunction

   // 6-input LUT function
   function lut6;
      input [63:0] mask;
      input        dataa;
      input        datab;
      input        datac;
      input        datad;
      input        datae;
      input        dataf;
      reg          f0_lut;
      reg          f1_lut;
      reg [31:0]   f0_mask;
      reg [63:32]  f1_mask ;
      begin
         f0_mask = mask[31:0];
         f1_mask = mask[63:32];
         lut6 = mask[{dataf, datae, datad, datac, datab, dataa}];
      end
   endfunction

   always @(datag or dataf or datae or datad or datac or
            datab or dataa or cin or sharein)
     begin
        // check for extended LUT mode
        if (iextended_lut == 1)
          f2_input3 = datag;
        else
          f2_input3 = datac;

        f0_out = lut4(f0_mask, dataa, datab, datac, datad);
        f1_out = lut4(f1_mask, dataa, datab, f2_input3, datad);
        f2_out = lut4(f2_mask, dataa, datab, datac, datad);
        f3_out = lut4(f3_mask, dataa, datab, f2_input3, datad);

        // combout is the 6-input LUT
        if (iextended_lut == 1)
          begin
             if (datae == 1'b0)
               begin
                  g0_out = f0_out;
                  g1_out = f2_out;
               end
             else if (datae == 1'b1)
               begin
                  g0_out = f1_out;
                  g1_out = f3_out;
               end
             else
               begin
                  if (f0_out == f1_out)
                    g0_out = f0_out;
                  else
                    g0_out = 1'bX;

                  if (f2_out == f3_out)
                    g1_out = f2_out;
                  else
                    g1_out = 1'bX;
               end

             if (dataf == 1'b0)
               combout = g0_out;
             else if ((dataf == 1'b1) || (g0_out == g1_out))
               combout = g1_out;
             else
               combout = 1'bX;
          end
        else
          combout = lut6(lut_mask, dataa, datab, datac, datad, datae, dataf);

        // check for shareed arithmetic mode
        if (ishared_arith == 1)
          adder_input2 = sharein;
        else
          begin
             f2_f = lut4(f2_mask, dataa, datab, datac, dataf);
             adder_input2 = !f2_f;
          end

        // sumout & cout
        sumout = cin ^ f0_out ^ adder_input2;
        cout = (cin & f0_out) | (cin & adder_input2) | (f0_out & adder_input2);
        shareout = f2_out;
     end

endmodule

// ==========================================================================================

module cyclone10gx_io_ibuf (i, ibar, dynamicterminationcontrol, o);

   parameter differential_mode = 0;
   parameter bus_hold = 0;
   parameter simulate_z_as = 0;
   parameter lpm_type = 0;

   input i;
   input ibar;
   input dynamicterminationcontrol;
   output o;

   assign o = i;

endmodule

// ==========================================================================================

module cyclone10gx_io_obuf (i, oe, dynamicterminationcontrol, seriesterminationcontrol, parallelterminationcontrol, devoe, o, obar);

   parameter open_drain_output = 0;
   parameter bus_hold = 0;
   parameter shift_series_termination_control = 0;
   parameter sim_dynamic_termination_control_is_connected = 0;
   parameter lpm_type = 0;

   input i;
   input oe;
   input devoe;
   input dynamicterminationcontrol;
   input [15:0] seriesterminationcontrol;
   input [15:0] parallelterminationcontrol;
   output       o;
   output       obar;

   assign o = i, obar = ~i;

endmodule

module cyclone10gx_clkena    (
                           inclk,
                           ena,
                           enaout,
                           outclk);

   // leda G_521_3_B off
   parameter    clock_type    =    "auto";
   parameter    ena_register_mode    =    "always enabled";
   parameter    lpm_type    =    "cyclone10gx_clkena";
   parameter    ena_register_power_up    =    "high";
   parameter    disable_mode    =    "low";
   parameter    test_syn    =    "high";
   // leda G_521_3_B on

   input    inclk;
   input    ena;
   output   enaout;
   output   outclk;

   assign outclk = ena ? inclk : 1'b0;
   assign enaout = ena;

endmodule //cyclone10gx_clkena
