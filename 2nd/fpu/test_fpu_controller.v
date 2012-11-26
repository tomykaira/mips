module test_fpu_controller ();

   `include "../opcode.h"

   reg clk;
   reg reset;

   reg [31:0] inst;
   reg [31:0] rs, rt;
   wire       enable, float;
   wire [4:0] addr;
   wire [31:0] data;

   fpu_controller dut (clk, inst, rs ,rt,
                       enable, addr, data, float);

   task dispatch;
      input [5:0] op;
      begin
         inst <= {op,26'b00000000000001000000000000}; // output to $f2
         #10;
      end
   endtask

   task check;
      input [31:0] expected;
      begin
         if (enable !== 1 || float !== 1 || addr !== 2 || data !== expected) begin
            $display("expectation not met");
            $stop;
         end
      end
   endtask

   initial begin
      reset <= 1;
      #22;
      reset <= 0;
      #10;

      rs <= 32'h41100000; // 9.0
      rt <= 32'h40a00000; // 5.0

      dispatch(FADD);
      dispatch(FSUB);
      dispatch(FMUL);
      check(32'h41600000); // FADD
      dispatch(FMULN);
      check(32'h40800000); // FSUB
      dispatch(FINV);
      check(32'h42340001); // FMUL
      dispatch(FSQRT);
      check(32'hc2340001); // FMULN
      dispatch(0'b111111);
      check(32'h3de38e36); // FINV
      #10 check(32'h40400000); // FSQRT
      #10
      if (enable !== 0) begin
         $display("expectation not met");
         $stop;
      end
   end

   // geenrate clock to sequence tests
   always begin
      clk <= 1;
      #5;
      clk <= 0;
      #5;
   end

endmodule
