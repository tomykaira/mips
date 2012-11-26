module test_rs232c ();

   `include "../opcode.h"

   reg clk;
   reg reset;

   reg [31:0] inst;
   reg [31:0] rt;

   wire       push_send_data;
   wire [7:0] send_data;

   reg        rx_wait;
   reg [7:0] received_data;

   wire       enable, float;
   wire [4:0] addr;
   wire [31:0] data;

   rs232c dut (clk, inst, rt,
               push_send_data, send_data,
               rx_wait, received_data,
               enable, float, addr, data);


   task assert_not_write_enable;
      begin
         if (enable !== 0) begin
            $display("write not enable expectation not met");
            $stop;
         end
      end
   endtask

   task assert_write_enable;
      input [4:0] address;
      input [31:0] expected;

      begin
         if (enable !== 1 || addr !== address || data !== expected) begin
            $display("write enable expectation not met");
            $stop;
         end
      end
   endtask

   task assert_send_data;
      input [7:0] expected;

      begin
         if (push_send_data !== 1 || send_data !== expected) begin
            $display("send data expectation not met");
            $stop;
         end
      end
   endtask

   initial begin
      reset <= 1;
      #22;
      reset <= 0;
      #10;

      // INPUTB
      inst <= {INPUTB,5'b0,5'h3,16'b0};
      rt <= 32'b0;

      rx_wait <= 1;
      received_data <= 5;

      #10 assert_not_write_enable();
      #10 assert_not_write_enable();
      #10 assert_not_write_enable();
      #10 assert_not_write_enable();

      rx_wait <= 0;
      received_data <= 8;
      #10 assert_write_enable(3, 8);

      rx_wait <= 1;
      #10 assert_not_write_enable();

      // OUTPUTB
      inst <= {OUTPUTB,5'b0,5'h3,16'b0};
      rt <= 95;
      #10 assert_not_write_enable();
      assert_send_data(95);

   end

   // geenrate clock to sequence tests
   always begin
      clk <= 1;
      #5;
      clk <= 0;
      #5;
   end

endmodule
