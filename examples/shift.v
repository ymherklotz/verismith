module shift1(input clk, in, output [3:0] q);
   reg q0 = 0, q1 = 0, q2 = 0, q3 = 0;

   always @(posedge clk) begin
      q0 = in;
   end
   always @(posedge clk) begin
      q1 = q0;
   end
   always @(posedge clk) begin
      q2 = q1;
   end
   always @(posedge clk) begin
      q3 = q2;
   end

   assign q = {q0, q1, q2, q3};
endmodule // shift1

module shift2(input clk, in, output [3:0] q);
   reg q0 = 0, q1 = 0, q2 = 0, q3 = 0;

   always @(*) begin
      q2 = q1;
      q3 = q2;
      q1 = q0;
      q0 = in;
   end

   assign q = {q0, q1, q2, q3};
endmodule // shift2

module shift3(input clk, in, output [3:0] q);
   reg q0 = 0, q1 = 0, q2 = 0, q3 = 0;

   always @(posedge clk) begin
      q3 <= q2;
      q2 <= q1;
      q1 <= q0;
      q0 <= in;
   end

   assign q = {q0, q1, q2, q3};
endmodule // shift3

module shift4(input clk, in, output [3:0] q);
   reg q0 = 0, q1 = 0, q2 = 0, q3 = 0;

   always @(posedge clk) begin
      q0 <= in;
   end
   always @(posedge clk) begin
      q1 <= q0;
   end
   always @(posedge clk) begin
      q2 <= q1;
   end
   always @(posedge clk) begin
      q3 <= q2;
   end

   assign q = {q0, q1, q2, q3};
endmodule // shift4

`ifdef FORMAL
module top(input clk, in);
   wire [3:0] q1, q2, q3, q4;

   shift1 shift1(.clk(clk), .in(in), .q(q1));
   shift2 shift2(.clk(clk), .in(in), .q(q2));
   shift3 shift3(.clk(clk), .in(in), .q(q3));
   shift4 shift4(.clk(clk), .in(in), .q(q4));

   always @(posedge clk) begin
      assert(q1 == q2);
      assert(q2 == q3);
      assert(q3 == q4);
   end
endmodule // top
`endif

`ifndef SYNTHESIS
module main;
   reg clk = 0, qin = 0;
   wire [256:0] val = 256'h73a27383942819780a5765c741b2949712384778493895283249178574;
   reg [5:0]   count = 0;
   wire [3:0] q1out, q2out, q3out, q4out;

   shift1 shift1(.clk(clk), .in(qin), .q(q1out));
   shift2 shift2(.clk(clk), .in(qin), .q(q2out));
   shift3 shift3(.clk(clk), .in(qin), .q(q3out));
   shift4 shift4(.clk(clk), .in(qin), .q(q4out));

   always #10 clk = ~clk;

   always @(posedge clk) begin
      count <= count + 1;
      qin <= val >> count;
      if(q1out !== q2out || q2out !== q3out || q3out !== q4out)
        $strobe("q: %b\n q1: %b\n q2: %b\n q3: %b\n q4: %b\n\n", qin, q1out, q2out, q3out, q4out);
   end
endmodule // main
`endif
