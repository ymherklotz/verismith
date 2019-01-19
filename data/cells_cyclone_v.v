// Based on cyclonev_atoms.v from Quartus II 14.0.0 Build 200 06/17/2014

// ==========================================================================================

module cyclonev_lcell_comb (
	dataa, datab, datac, datad, datae, dataf, datag, cin, sharein,
	combout, sumout, cout, shareout
);

input dataa, datab, datac, datad, datae, dataf, datag, cin, sharein;
output reg combout, sumout, cout, shareout;

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

reg f0_out, f1_out, f2_out, f3_out;
reg g0_out, g1_out;
reg f2_input3, f2_f;
reg adder_input2;

// 4-input LUT function
function lut4;
    input [15:0] mask;
    input dataa;
    input datab;
    input datac;
    input datad;
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
    input dataa;
    input datab;
    input datac;
    input datad;
    input datae;
    reg e0_lut;
    reg e1_lut;
    reg [15:0] e0_mask;
    reg [31:16] e1_mask;
    begin
        e0_mask = mask[15:0];
        e1_mask = mask[31:16];
        begin
            e0_lut = lut4(e0_mask, dataa, datab, datac, datad);
            e1_lut = lut4(e1_mask, dataa, datab, datac, datad);
            if (datae === 1'bX) // X propogation
            begin
                if (e0_lut == e1_lut)
                begin
                    lut5 = e0_lut;
                end
                else
                begin
                    lut5 = 1'bX;
                end
            end
            else
            begin
                lut5 = (datae == 1'b1) ? e1_lut : e0_lut;
            end
        end
    end
endfunction

// 6-input LUT function
function lut6;
    input [63:0] mask;
    input dataa;
    input datab;
    input datac;
    input datad;
    input datae;
    input dataf;
    reg f0_lut;
    reg f1_lut;
    reg [31:0] f0_mask;
    reg [63:32] f1_mask ;
    begin
        f0_mask = mask[31:0];
        f1_mask = mask[63:32];
        begin
            lut6 = mask[{dataf, datae, datad, datac, datab, dataa}];
            if (lut6 === 1'bX)
            begin
                f0_lut = lut5(f0_mask, dataa, datab, datac, datad, datae);
                f1_lut = lut5(f1_mask, dataa, datab, datac, datad, datae);
                if (dataf === 1'bX) // X propogation
                begin
                    if (f0_lut == f1_lut)
                    begin
                        lut6 = f0_lut;
                    end
                    else
                    begin
                        lut6 = 1'bX;
                    end
                end
                else
                begin
                    lut6 = (dataf == 1'b1) ? f1_lut : f0_lut;
                end
            end
        end
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
output o;
output obar;

assign o = i, obar = ~i;

endmodule

// ==========================================================================================
