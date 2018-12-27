module and_comb(clk, out, in1, in2);
   input wire [7:0] in1;
   input wire [7:0] in2;
   input wire       clk;
   output reg [7:0] out;

   always @(posedge clk)
     begin
        out <= in1 & in2;
     end
endmodule

module main;
   wire [7:0] c;
   reg [7:0]  a;
   reg [7:0]  b;
   reg        clk;

   and_comb gate(.in1(a), .in2(b), .out(c), .clk(clk));

   initial
     begin
        a = 8'd29;
        b = 8'd95;
        clk = 1'b0;
        #10;
        clk = 1'b1;
        #10 $display("%d & %d = %d", a, b, c);
        $finish;
     end
endmodule
