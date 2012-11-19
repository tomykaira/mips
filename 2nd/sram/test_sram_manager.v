module test_sram_manager ();

   parameter LDI  = 6'b101000;
   parameter STI  = 6'b101001;
   parameter LDR  = 6'b101100;
   parameter FLDI = 6'b101010;
   parameter FSTI = 6'b101011;
   parameter FLDR = 6'b101110;
   parameter NOP  = 6'b000000;

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
         inst <= {op,26'b00000000000001000010000000}; // output to $f2
         #10;
      end
   endtask

   task assertion;
      input [31:0] float_expected;
      input [31:0] expected;
      begin

         if (enable !== 1 || float !== float_expected || addr !== 2 || data !== expected) begin
            $display ("FAIL");
            $stop;
         end
      end
   endtask

   initial begin
      reset <= 1;
      #22;
      reset <= 0;
      #10;

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
      #10;
      dispatch(FLDI);
      #10;
      #10;
      assertion(1, 30);

      dispatch(LDI);
      #10;
      #10;
      assertion(0, 10);

      rt <= 15;

      dispatch(STI);
      #10;
      dispatch(LDR);
      #10;
      #10;
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
      #10;
      assertion(0, 9);
      #10;
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
