(* use_dsp48="no" *) module top
#(
parameter param209 = ((2'ha) ^~ (1'h4)),
param210 = ((9'hc) ~^ (~^((param209 <<< param209) > (^(9'h9))))),
param211 = ((((~&(8'h6)) & param210) && param210) ^~ (!((+param209) > (~&param210))))
)
(y, clk, wire0, wire1, wire2);
output wire [1662:0] y;
input wire clk;
input wire [2:0] wire0;
input wire signed [13:0] wire1;
input wire signed [9:0] wire2;
reg signed [9:0] reg208 = (32'h0);
reg signed [2:0] reg207 = (32'h0);
reg [8:0] reg206 = (32'h0);
reg signed [4:0] reg205 = (32'h0);
reg signed [1:0] reg204 = (32'h0);
reg signed [4:0] reg203 = (32'h0);
reg signed [3:0] reg202 = (32'h0);
reg reg201 = (32'h0);
reg [7:0] reg200 = (32'h0);
reg [8:0] reg199 = (32'h0);
reg [14:0] reg198 = (32'h0);
reg signed [12:0] reg197 = (32'h0);
reg signed [6:0] reg196 = (32'h0);
wire signed wire195;
reg signed [5:0] reg194 = (32'h0);
reg signed [4:0] reg193 = (32'h0);
reg signed [1:0] reg192 = (32'h0);
reg signed [12:0] reg191 = (32'h0);
reg signed [11:0] reg190 = (32'h0);
reg signed [7:0] reg189 = (32'h0);
reg signed reg188 = (32'h0);
reg signed [5:0] reg187 = (32'h0);
reg [10:0] reg186 = (32'h0);
reg signed [9:0] reg185 = (32'h0);
reg [3:0] reg184 = (32'h0);
reg signed [6:0] reg183 = (32'h0);
reg [12:0] reg182 = (32'h0);
reg reg181 = (32'h0);
reg signed [7:0] reg180 = (32'h0);
reg signed [5:0] reg179 = (32'h0);
reg signed [2:0] reg178 = (32'h0);
reg signed [5:0] reg177 = (32'h0);
reg [12:0] reg176 = (32'h0);
reg [12:0] reg175 = (32'h0);
reg signed [1:0] reg174 = (32'h0);
reg signed [14:0] reg173 = (32'h0);
reg [14:0] reg172 = (32'h0);
reg [7:0] reg171 = (32'h0);
reg [10:0] reg170 = (32'h0);
reg signed [3:0] reg169 = (32'h0);
reg [12:0] reg168 = (32'h0);
reg [4:0] reg167 = (32'h0);
reg [12:0] reg166 = (32'h0);
reg [4:0] reg165 = (32'h0);
reg [6:0] reg164 = (32'h0);
reg signed [8:0] reg163 = (32'h0);
reg signed [2:0] reg162 = (32'h0);
reg signed [6:0] reg161 = (32'h0);
reg signed reg160 = (32'h0);
reg [14:0] reg159 = (32'h0);
reg [14:0] reg158 = (32'h0);
reg signed [8:0] reg157 = (32'h0);
reg [5:0] reg156 = (32'h0);
reg [5:0] reg155 = (32'h0);
reg [2:0] reg154 = (32'h0);
reg [2:0] reg153 = (32'h0);
reg [7:0] reg152 = (32'h0);
reg signed [1:0] reg151 = (32'h0);
reg signed [9:0] reg150 = (32'h0);
reg signed [2:0] reg149 = (32'h0);
reg [10:0] reg148 = (32'h0);
reg [9:0] reg147 = (32'h0);
reg signed [14:0] reg146 = (32'h0);
reg signed [14:0] reg145 = (32'h0);
reg [14:0] reg144 = (32'h0);
reg reg143 = (32'h0);
reg [9:0] reg142 = (32'h0);
reg [4:0] reg141 = (32'h0);
reg [12:0] reg140 = (32'h0);
reg [7:0] reg139 = (32'h0);
reg [3:0] reg138 = (32'h0);
reg signed [2:0] reg137 = (32'h0);
reg [14:0] reg136 = (32'h0);
reg [8:0] reg135 = (32'h0);
reg [4:0] reg134 = (32'h0);
reg [10:0] reg133 = (32'h0);
reg [9:0] reg132 = (32'h0);
reg [6:0] reg131 = (32'h0);
reg [1:0] reg130 = (32'h0);
reg signed [5:0] reg129 = (32'h0);
reg [14:0] reg128 = (32'h0);
reg [5:0] reg127 = (32'h0);
reg signed [2:0] reg126 = (32'h0);
reg signed [12:0] reg125 = (32'h0);
reg signed [7:0] reg124 = (32'h0);
reg [5:0] reg123 = (32'h0);
reg signed [3:0] reg122 = (32'h0);
reg [10:0] reg121 = (32'h0);
reg [13:0] reg120 = (32'h0);
reg [5:0] reg119 = (32'h0);
reg signed [8:0] reg118 = (32'h0);
reg [5:0] reg117 = (32'h0);
reg signed [14:0] reg116 = (32'h0);
reg signed [10:0] reg115 = (32'h0);
reg reg114 = (32'h0);
reg signed [10:0] reg113 = (32'h0);
reg [1:0] reg112 = (32'h0);
reg [6:0] reg111 = (32'h0);
reg signed [5:0] reg110 = (32'h0);
reg signed [10:0] reg109 = (32'h0);
reg signed [11:0] reg108 = (32'h0);
reg signed [7:0] reg107 = (32'h0);
reg signed [1:0] reg106 = (32'h0);
reg [13:0] reg105 = (32'h0);
reg signed [4:0] reg104 = (32'h0);
reg [2:0] reg103 = (32'h0);
reg signed [10:0] reg102 = (32'h0);
reg signed [9:0] reg101 = (32'h0);
reg [9:0] reg100 = (32'h0);
reg [12:0] reg99 = (32'h0);
reg [9:0] reg98 = (32'h0);
reg reg97 = (32'h0);
reg [1:0] reg96 = (32'h0);
reg [1:0] reg95 = (32'h0);
reg [2:0] reg94 = (32'h0);
reg [6:0] reg93 = (32'h0);
reg [12:0] reg92 = (32'h0);
reg signed [13:0] reg91 = (32'h0);
reg [11:0] reg90 = (32'h0);
reg signed [13:0] reg89 = (32'h0);
reg signed [12:0] reg88 = (32'h0);
reg [13:0] reg87 = (32'h0);
reg [13:0] reg86 = (32'h0);
reg [3:0] reg85 = (32'h0);
reg [5:0] reg84 = (32'h0);
reg signed [11:0] reg83 = (32'h0);
reg signed [10:0] reg82 = (32'h0);
reg signed [7:0] reg81 = (32'h0);
reg [8:0] reg80 = (32'h0);
reg signed [9:0] reg79 = (32'h0);
reg signed [4:0] reg78 = (32'h0);
reg signed [7:0] reg77 = (32'h0);
reg [5:0] reg76 = (32'h0);
reg signed [7:0] reg75 = (32'h0);
reg signed [5:0] reg74 = (32'h0);
reg [9:0] reg73 = (32'h0);
reg signed [6:0] reg72 = (32'h0);
reg [5:0] reg71 = (32'h0);
reg [5:0] reg70 = (32'h0);
reg signed [10:0] reg69 = (32'h0);
reg signed [2:0] reg68 = (32'h0);
reg [10:0] reg67 = (32'h0);
reg signed [14:0] reg66 = (32'h0);
reg [4:0] reg65 = (32'h0);
reg signed reg64 = (32'h0);
reg [12:0] reg63 = (32'h0);
reg [7:0] reg62 = (32'h0);
reg [14:0] reg61 = (32'h0);
reg [14:0] reg60 = (32'h0);
reg signed [9:0] reg59 = (32'h0);
reg signed reg58 = (32'h0);
reg signed [4:0] reg57 = (32'h0);
reg signed [3:0] reg56 = (32'h0);
reg [2:0] reg55 = (32'h0);
reg [8:0] reg54 = (32'h0);
reg signed [13:0] reg53 = (32'h0);
reg signed [8:0] reg52 = (32'h0);
reg [10:0] reg51 = (32'h0);
reg [13:0] reg50 = (32'h0);
reg signed [12:0] reg49 = (32'h0);
reg [13:0] reg48 = (32'h0);
reg [4:0] reg47 = (32'h0);
reg signed [11:0] reg46 = (32'h0);
reg [10:0] reg45 = (32'h0);
reg [8:0] reg44 = (32'h0);
reg [2:0] reg43 = (32'h0);
reg signed [3:0] reg42 = (32'h0);
reg signed [8:0] reg41 = (32'h0);
reg signed [6:0] reg40 = (32'h0);
reg signed [13:0] reg39 = (32'h0);
reg [3:0] reg38 = (32'h0);
reg [3:0] reg37 = (32'h0);
reg [3:0] reg36 = (32'h0);
reg signed [7:0] reg35 = (32'h0);
reg [8:0] reg34 = (32'h0);
reg signed [10:0] reg33 = (32'h0);
reg signed [14:0] reg32 = (32'h0);
reg [13:0] reg31 = (32'h0);
reg signed [9:0] reg30 = (32'h0);
reg [13:0] reg29 = (32'h0);
reg signed [8:0] reg28 = (32'h0);
reg signed [8:0] reg27 = (32'h0);
reg [1:0] reg26 = (32'h0);
reg signed [13:0] reg25 = (32'h0);
reg [4:0] reg24 = (32'h0);
reg signed [8:0] reg23 = (32'h0);
reg signed [8:0] reg22 = (32'h0);
reg signed [3:0] reg21 = (32'h0);
reg signed [10:0] reg20 = (32'h0);
reg signed [3:0] reg19 = (32'h0);
reg signed [13:0] reg18 = (32'h0);
reg signed [4:0] reg17 = (32'h0);
reg [9:0] reg16 = (32'h0);
reg signed [3:0] reg15 = (32'h0);
reg signed reg14 = (32'h0);
reg [8:0] reg13 = (32'h0);
reg signed [4:0] reg12 = (32'h0);
reg signed [11:0] reg11 = (32'h0);
reg signed reg10 = (32'h0);
reg signed [2:0] reg9 = (32'h0);
reg signed [10:0] reg8 = (32'h0);
reg [3:0] reg7 = (32'h0);
reg [1:0] reg6 = (32'h0);
wire [3:0] wire5;
wire signed [13:0] wire4;
wire [9:0] wire3;
assign wire3 = (&(~^wire2));
assign wire4 = $unsigned($signed((+$unsigned((-6'h1)))));
assign wire5 = (($signed($signed(wire4)) <<< (~(2'h1))) && $signed(wire3));
always @(posedge clk) begin
reg6 = wire5;
reg7 = reg6;
reg8 = $signed(wire4);
reg9 = $signed($unsigned((-2'h1)));
reg10 = $signed((-15'h7));
reg11 <= wire1;
reg12 <= (+wire1);
reg13 = (wire1 ^ (+$signed($signed(reg7))));
reg14 <= $signed(($unsigned((reg10 - reg7)) ^ $signed($unsigned((9'ha)))));
reg15 = wire0;
reg16 = $unsigned($unsigned($unsigned(reg9)));
reg17 = (reg10 >>> $signed($signed($unsigned((-8'h6)))));
if(($unsigned((~((-wire4) >>> $unsigned(wire5)))) == (32'h0)))
begin
reg18 = ((^~(-15'h3)) ~^ (5'h7));
reg19 <= $signed($signed(wire1));
reg20 = (-14'h1);
reg21 <= ((-6'ha) < $signed($signed(wire5)));
reg22 <= (-((^~(^reg20)) < $signed($signed(reg10))));
reg23 = wire0;
if((((reg20 & reg7) ~^ ((!(9'h8)) >>> $signed((!reg11)))) == (32'h0)))
begin
reg24 = $signed((-4'h9));
reg25 = $signed($signed(reg10));
reg26 <= (!((!(+reg19)) >= reg6));
end
reg27 = ((+reg18) || (^~reg22));
reg28 <= (($signed((~|reg16)) <= (~&(&reg6))) >>> $signed($unsigned(reg14)));
reg29 = $signed($signed($unsigned((-8'hd))));
end
else
begin
reg30 = $signed(reg6);
reg31 = $unsigned((15'he));
reg32 <= $signed((reg12 ^~ $unsigned(reg8)));
reg33 <= (-7'h1);
reg34 = ((|wire2) >= (reg28 ^ $unsigned($unsigned(reg18))));
reg35 = $unsigned((10'h8));
reg36 <= ((~|((12'h1) && reg12)) <= (-3'h4));
reg37 <= (1'h1);
reg38 <= (2'h6);
reg39 <= (~^wire4);
reg40 <= $unsigned($signed($unsigned($unsigned((-11'hc)))));
reg41 <= $unsigned((~&$signed($signed((7'h7)))));
reg42 = $unsigned($unsigned((!(reg25 || reg29))));
reg43 <= reg31;
end
end
always @(posedge clk) begin
if((((reg33 << (9'hc)) >> $unsigned((~$unsigned((14'hd))))) == (32'h0)))
begin
reg44 <= $signed($unsigned((-14'hc)));
reg45 = (3'h2);
reg46 <= $signed($unsigned($signed((^~(-11'hb)))));
reg47 <= ($unsigned((~(reg8 >= reg38))) < (7'h6));
reg48 <= (wire3 > reg13);
reg49 = $signed($signed((~$unsigned((-4'hf)))));
reg50 <= $unsigned($unsigned($unsigned(((15'he) <= (6'hc)))));
reg51 = $signed((4'h9));
reg52 <= $signed((2'h9));
if(((10'h3) == (32'h0)))
begin
if(((~^(~$unsigned($signed(reg33)))) == (32'h0)))
begin
reg53 <= (7'ha);
reg54 <= ((wire2 << $signed((-14'hd))) && $unsigned($unsigned((~(9'h3)))));
reg55 = (-3'h8);
reg56 = ((-9'hb) ^ $signed(((~|(-3'hd)) >> $signed(reg28))));
reg57 <= $signed(reg40);
reg58 <= (~((&(1'h5)) * $signed($unsigned(reg42))));
end
else
begin
reg59 = ((&$unsigned($unsigned((1'h4)))) ^ (&($signed((2'h3)) <= $unsigned(wire3))));
reg60 = $unsigned((-7'h3));
end
reg61 <= reg29;
reg62 = reg12;
reg63 <= reg10;
reg64 = (-2'h7);
reg65 = wire2;
reg66 = $signed((-6'h6));
reg67 = $unsigned((-10'he));
end
else
begin
reg68 = $signed($unsigned(($unsigned(reg44) ^~ $signed((-8'h1)))));
reg69 = $signed((^~((reg51 <= reg13) || ((3'h4) <= reg8))));
end
reg70 = (~|$unsigned((-2'hf)));
reg71 = (-3'h2);
end
else
begin
reg72 = wire2;
reg73 <= ((~|$signed(reg41)) ^~ reg58);
reg74 <= reg65;
reg75 = (~^wire1);
reg76 = $unsigned(reg27);
reg77 = $signed((7'hc));
reg78 <= $unsigned(reg8);
reg79 <= $unsigned($signed(((12'hc) - ((6'hb) == reg36))));
reg80 = $signed(((~^reg45) << $unsigned($signed((-10'h6)))));
reg81 = reg75;
end
reg82 <= $unsigned((^~wire1));
reg83 = (-14'h5);
if(((-15'h9) == (32'h0)))
begin
reg84 = reg37;
reg85 = (7'h9);
reg86 = ((2'h2) ^~ ($signed($unsigned((4'he))) << reg67));
end
else
begin
reg87 = $signed(((+$signed(reg80)) < ((-14'h3) > $unsigned((2'h5)))));
reg88 <= $unsigned($signed(reg30));
reg89 <= (((!(-10'h4)) ^ reg17) && (+(~reg59)));
reg90 <= ($signed(((!reg23) - (-5'he))) & ((-$unsigned(reg77)) >>> ($signed((-15'h9)) < $signed(reg27))));
reg91 <= (-4'h9);
if(((reg28 | (4'hf)) == (32'h0)))
begin
reg92 <= reg87;
reg93 = (!(~^$unsigned((~|wire5))));
end
else
begin
reg94 <= $unsigned(reg13);
reg95 = $signed($unsigned($unsigned(wire5)));
reg96 = $signed(reg73);
reg97 <= $unsigned((($unsigned((14'hf)) ^~ (12'hf)) == (((-2'h9) > reg45) || (-9'h4))));
reg98 = $unsigned($unsigned(reg36));
reg99 <= $signed(((3'h6) * reg69));
reg100 = $signed($unsigned((5'hf)));
reg101 = (-12'h2);
reg102 <= ((($signed((14'hf)) ^~ reg76) & $unsigned($unsigned((6'h1)))) ~^ $signed(((reg60 >> reg54) ^ $signed(reg70))));
reg103 = $unsigned((|(-7'he)));
reg104 = ($unsigned(($unsigned((12'hc)) && (-13'h1))) - (|$unsigned(((-6'ha) <<< reg45))));
reg105 = ((-11'hb) ~^ (-3'hf));
reg106 = (~^(+reg75));
end
end
reg107 = reg49;
reg108 <= reg15;
reg109 = $signed(reg33);
if(((~(11'h1)) == (32'h0)))
begin
reg110 = $unsigned(reg91);
reg111 <= (-(-5'h2));
end
else
begin
reg112 <= $unsigned(($signed($signed(reg58)) ~^ reg20));
reg113 <= (~^$signed(reg23));
reg114 <= reg25;
reg115 <= (-5'h8);
if(($signed(($unsigned(reg10) >= (4'ha))) == (32'h0)))
begin
reg116 = (^((9'h7) == (~reg95)));
reg117 = (+((10'hd) <= (-3'ha)));
reg118 <= (~(reg109 & $unsigned((^~reg55))));
reg119 <= (|$signed(reg69));
end
else
begin
reg120 = (4'hd);
reg121 = (&(~^(((-5'h7) * reg92) < $unsigned((11'h2)))));
reg122 <= $unsigned((&$unsigned(((9'h7) ^ reg68))));
reg123 <= (-$unsigned((reg91 | $signed((10'ha)))));
if(($unsigned((-2'hd)) == (32'h0)))
begin
reg124 <= $signed((8'h0));
reg125 <= (-((~&reg41) != $signed((~&(-14'ha)))));
reg126 <= $unsigned(reg63);
reg127 <= ((3'hc) ^~ reg80);
reg128 <= (|(-5'h1));
reg129 = reg45;
reg130 <= reg110;
reg131 <= ((-9'h8) > $unsigned(((9'h8) <= ((-3'h6) && (-14'h2)))));
reg132 = (14'h2);
reg133 <= (-(+reg120));
reg134 = (-8'hf);
end
else
begin
reg135 = (-1'h1);
reg136 = reg133;
reg137 = $signed((-2'h5));
reg138 <= $unsigned(((-8'h2) * reg84));
reg139 <= (-((reg119 >> $unsigned((4'h1))) & $unsigned((reg133 << (8'hc)))));
reg140 <= (12'h1);
reg141 = $signed(reg100);
reg142 <= reg85;
reg143 = $signed(reg103);
reg144 = (+(~|($signed((14'h9)) + (-15'h8))));
reg145 = ((-reg34) <= reg125);
reg146 <= ($signed((-4'h4)) <= (~&(-3'h6)));
reg147 <= (-(6'h6));
end
reg148 <= reg144;
reg149 = $unsigned((+(6'h1)));
reg150 <= ($signed(($signed((4'hb)) >>> $unsigned((3'h5)))) <= $signed((~&(reg20 >>> reg111))));
reg151 = $signed($unsigned(((11'h3) && $signed((7'ha)))));
reg152 <= reg63;
end
reg153 = (-7'h3);
if(($signed((reg47 | (((-13'hf) ~^ reg129) >>> ((9'hf) - reg18)))) == (32'h0)))
begin
reg154 = (-2'h7);
reg155 <= reg69;
reg156 <= $signed($signed(((^~(-1'h3)) > (7'he))));
reg157 <= $signed(reg120);
reg158 <= (^(12'hc));
reg159 <= $signed($signed($unsigned((&(12'h3)))));
reg160 = ((6'h7) || $signed(((|(9'h1)) - $unsigned(reg76))));
reg161 <= (~(13'hb));
reg162 = (~|($signed((reg104 * reg59)) ^~ reg151));
reg163 <= (-14'hd);
reg164 <= ($signed($signed($unsigned((-11'h8)))) >= (~($signed((3'h8)) ^ $unsigned(reg150))));
reg165 = $signed($signed(($signed((-9'hd)) >> $signed((-1'he)))));
reg166 = (|(~&(-6'h6)));
reg167 = (7'h2);
reg168 = reg20;
end
else
begin
reg169 = (1'h9);
reg170 <= (4'h7);
reg171 = reg47;
reg172 <= $unsigned((~$signed((reg81 >>> reg114))));
reg173 = (~|$unsigned((~(~|(-8'ha)))));
reg174 = (1'hd);
reg175 = ((-3'h8) ^ (-1'h6));
reg176 <= (~|$signed($unsigned((~^(-15'hd)))));
reg177 = ((((-7'h7) >> (~|reg86)) + (~^reg38)) && (14'h3));
reg178 <= reg154;
if(((2'h1) == (32'h0)))
begin
reg179 <= $unsigned($signed($signed((^~(11'h2)))));
reg180 = $unsigned(reg46);
reg181 = (14'h7);
reg182 <= (($signed((reg164 * reg143)) > $signed((reg49 < reg7))) << (+$signed(reg66)));
reg183 = $unsigned((((13'h6) * (reg15 ^ reg124)) >> ((^(11'hb)) <<< $unsigned((-13'h6)))));
reg184 <= ((1'h5) * $unsigned(reg174));
end
else
begin
reg185 <= wire4;
reg186 = reg70;
reg187 <= reg51;
reg188 <= $unsigned(reg123);
reg189 = $signed((^$unsigned(reg163)));
end
reg190 = $signed($unsigned((-13'h5)));
reg191 = $signed((8'h2));
reg192 <= reg70;
end
reg193 = $unsigned($unsigned(reg14));
reg194 <= $unsigned(($unsigned((reg147 && (5'h9))) & $unsigned($signed((7'he)))));
end
end
assign wire195 = reg108;
always @(posedge clk) begin
reg196 = (!(!reg63));
reg197 = $unsigned((~(&$unsigned((6'hf)))));
reg198 = (15'h1);
reg199 <= wire1;
reg200 = (!reg41);
reg201 = reg160;
reg202 <= $unsigned(((^~(~|(9'h9))) ^ (reg104 >= (^(-11'h8)))));
reg203 <= $unsigned((-(-8'h7)));
reg204 = (7'h0);
reg205 = (9'h8);
reg206 <= (($signed(reg148) ^~ $unsigned($signed(reg65))) <<< $unsigned($signed((!reg176))));
reg207 = $unsigned(reg205);
reg208 = wire3;
end
assign y = {reg208, reg207, reg206, reg205, reg204, reg203, reg202, reg201, reg200, reg199, reg198, reg197, reg196, wire195, reg194, reg193, reg192, reg191, reg190, reg189, reg188, reg187, reg186, reg185, reg184, reg183, reg182, reg181, reg180, reg179, reg178, reg177, reg176, reg175, reg174, reg173, reg172, reg171, reg170, reg169, reg168, reg167, reg166, reg165, reg164, reg163, reg162, reg161, reg160, reg159, reg158, reg157, reg156, reg155, reg154, reg153, reg152, reg151, reg150, reg149, reg148, reg147, reg146, reg145, reg144, reg143, reg142, reg141, reg140, reg139, reg138, reg137, reg136, reg135, reg134, reg133, reg132, reg131, reg130, reg129, reg128, reg127, reg126, reg125, reg124, reg123, reg122, reg121, reg120, reg119, reg118, reg117, reg116, reg115, reg114, reg113, reg112, reg111, reg110, reg109, reg108, reg107, reg106, reg105, reg104, reg103, reg102, reg101, reg100, reg99, reg98, reg97, reg96, reg95, reg94, reg93, reg92, reg91, reg90, reg89, reg88, reg87, reg86, reg85, reg84, reg83, reg82, reg81, reg80, reg79, reg78, reg77, reg76, reg75, reg74, reg73, reg72, reg71, reg70, reg69, reg68, reg67, reg66, reg65, reg64, reg63, reg62, reg61, reg60, reg59, reg58, reg57, reg56, reg55, reg54, reg53, reg52, reg51, reg50, reg49, reg48, reg47, reg46, reg45, reg44, reg43, reg42, reg41, reg40, reg39, reg38, reg37, reg36, reg35, reg34, reg33, reg32, reg31, reg30, reg29, reg28, reg27, reg26, reg25, reg24, reg23, reg22, reg21, reg20, reg19, reg18, reg17, reg16, reg15, reg14, reg13, reg12, reg11, reg10, reg9, reg8, reg7, reg6, wire5, wire4, wire3};
endmodule
