module finv_tb();

   parameter CYCLE = 10;
   parameter STROBE = 9;
   parameter CASES = 1000;

   reg [31:0] MEM[CASES*2-1:0];
   reg clk;
   wire [31:0] s;
   reg [31:0]  a;
   integer     i;

   finv dut (clk, a, s);

   initial begin
      $readmemh ("finv_short.vec", MEM);

      #STROBE;
      
      for (i = 0; i < CASES; i = i + 1) begin
         //遅延させる
         a <= MEM[i*2];
         #CYCLE;
         if (i >= 2 && s !== MEM[(i-2)*2+1]) begin
            $display ("a = %h => s = %h (expected: %h)", MEM[(i-2)*2], s, MEM[(i-2)*2+1]);
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
