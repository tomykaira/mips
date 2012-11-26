module test_sram_manager ();

   `include "../opcode.h"

   reg clk;
   reg reset;

   reg [31:0] inst, rs, rt, imm;
   wire [31:0] memory_read, memory_write, memory_address;
   wire        memory_write_enable;
   wire        enable;
   wire [4:0]  addr;
   wire [31:0] data;
   wire        float;

   sram_manager dut (clk, inst, rs, rt, imm,
                         memory_read, memory_write, memory_address,
                         memory_write_enable, enable, addr, data, float);

   wire [31:0] ZD;
   wire [3:0]  ZDP;
   wire [19:0] ZA;
   wire        XWA;

   sramc sramc_inst (ZD, ZDP, ZA, XWA, memory_read, memory_write, memory_address[19:0], memory_write_enable, clk);

   wire [1:0]  ZCLKMA;

   fake_sram fake_inst(ZD, ZDP, ZA, XWA, ZCLKMA, 1'b0, 1'b1, 1'b0, 4'b0, 1'b0, 1'b0, 1'b0, 1'b0, 1'b1, 1'b0);

   assign ZCLKMA[0] = clk;
   assign ZCLKMA[1] = clk;

   task dispatch;
      input [5:0] op;
      begin
         inst <= {op,5'b00000,5'b00010,5'b00010,11'b0}; // output to $f2
         #10;
      end
   endtask

   task assertion;
      input [31:0] float_expected;
      input [31:0] expected;
      begin

         if (enable !== 1 || float !== float_expected || addr !== 2 || data !== expected) begin
            $display ("FAIL %d %d(%d) %d %d(%d)", enable, float, float_expected, addr, data, expected);
            $stop;
         end
      end
   endtask

   initial begin
      reset <= 1;
      #22;
      reset <= 0;
      #15;

      rs <= 5;
      rt <= 10;
      imm <= 15;

      dispatch(STI);
      imm <= 0;
      dispatch(NOP);
      imm <= 15;
      dispatch(LDI);
      imm <= 0;
      dispatch(NOP);
      assertion(0, 10);

      rt <= 30;

      dispatch(FSTI);
      dispatch(NOP);
      dispatch(FLDI);
      dispatch(NOP);
      assertion(1, 30);

      rt <= 15;

      dispatch(LDR);
      dispatch(NOP);
      assertion(0, 10);

      rt <= 9;
      imm <= 20;
      dispatch(STI);
      rt <= 10;
      imm <= 21;
      dispatch(STI);
      imm <= 20;
      dispatch(LDI);
      imm <= 21;
      dispatch(LDI);
      assertion(0, 9);
      dispatch(NOP);
      assertion(0, 10);

   end

   // geenrate clock to sequence tests
   always begin
      clk <= 1;
      #5;
      clk <= 0;
      #5;
   end

endmodule
