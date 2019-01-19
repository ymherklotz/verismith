module and_comb(input wire  in1,
                input wire  in2,
                output wire out
                );

   and and1(out, in1, in2);

endmodule

module main;
   reg a, b;
   wire c;

   and_comb gate(.in1(a), .in2(b), .out(c));

   initial
     begin
        a = 1'b1;
        b = 1'b1;
        #1;
        $display("%d & %d = %d", a, b, c);
        $finish;
     end
endmodule
