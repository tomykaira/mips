module test_program_counter ();

   reg clk;
   reg reset;

   reg [31:0] inst, rs;
   reg branch_taken;
   wire [31:0] pc;

   program_counter dut (clk, reset, inst, rs, branch_taken, pc);

   task pc_is;
      input [31:0] expected;
      begin
         if (pc !== expected) begin
            $display ("FAIL %d != %d", pc, expected);
            $stop;
         end
      end
   endtask

   initial begin
      reset <= 1;
      #22;
      reset <= 0;
      #10;

      rs <= 0;
      branch_taken <= 0;

      inst <= 32'b11100000000000000000000000110101;
      #10 pc_is(53);

      inst <= 32'b11100100010000000000000000000000;
      #10 rs <= 5; inst <= 32'h00000000;
      #10 pc_is(5);

      inst <= 32'b11101000000000000000000000110101;
      #10 pc_is(53);
      inst <= 32'b11110000000000000000000000000000;
      #10 pc_is(5);

      inst <= 32'b10000000000000000000000000110101;
      #10 inst <= 32'h00000000;
      #10 branch_taken <= 1;
      #10 pc_is(58);

   end

   // geenrate clock to sequence tests
   always begin
      clk <= 1;
      #5;
      clk <= 0;
      #5;
   end

endmodule
