module fmul_tb();

   parameter CYCLE = 10;
   parameter STROBE = 9;
   parameter CASES = 1000;

   reg [31:0] MEM[CASES*3-1:0];
   reg clk;
   wire [31:0] s;
   reg [31:0] a, b;
   integer    i;

   fmul dut (clk, a, b, s);

   initial begin
      $readmemh ("fmul_short.vec", MEM);

      #STROBE;

      for (i = 0; i < CASES; i = i + 1) begin
         //遅延させる
         a <= MEM[i*3];
         b <= MEM[i*3+1];
         #CYCLE;
         if (i >= 2 && s !== MEM[(i-2)*3+2]) begin
            $display ("a = %h b = %h => s = %h (expected: %h)", a, b, s, MEM[(i-2)*3+2]);
            $stop;
         end
      end

      $finish;
   end

   always begin
      clk <= 1;
      #(CYCLE/2) clk <= 0;
      #(CYCLE/2);
   end

endmodule
