module testbench_alu();

   reg [31:0] a,b;
   wire [31:0] out;
   reg [2:0]  control;
   wire zero;

   alu dut (a,b,control,out,zero);

   // initialize test by resetting
   initial begin
      a <= 18;
      b <= 9;
      control <= 3'b000;
      #5;
      if (~(out === 0 && zero === 1)) begin
         $display ("FAILED: AND");
      end
      
      control <= 3'b001;
      #5;
      if (~(out === 27 && zero === 0)) begin
         $display ("FAILED: OR");
      end

      control <= 3'b010;
      #5;
      if (~(out === 27 && zero === 0)) begin
         $display ("FAILED: +");
      end

      control <= 3'b110;
      #5;
      if (~(out === 9 && zero === 0)) begin
         $display ("FAILED: -");
      end
      
      control <= 3'b111;
      #5;
      if (~(out === 0 && zero === 1)) begin
         $display ("FAILED: SLT");
      end

      a <= 18;
      b <= 18;
      #5;
      if (~(out === 0 && zero === 1)) begin
         $display ("FAILED: SLT");
      end

      a <= 18;
      b <= 19;
      #5;
      if (~(out === 1 && zero === 0)) begin
         $display ("FAILED: SLT");
      end

      a <= 18;
      b <= 100;
      #5;
      if (~(out === 1 && zero === 0)) begin
         $display ("FAILED: SLT");
      end

      $stop;
   end

endmodule
