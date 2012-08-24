module testbench();

   reg clk;
   reg xreset;
   reg res_rx;
   wire [31:0] write_data, data_addr;
   wire mem_write;

   test_top dut (clk, xreset, rs_rx, write_data, data_addr, mem_write);

   // initialize test by xresetting
   initial begin
      xreset <= 0;
      rs_rx  <= 1;
      #22;
      xreset <= 1;
   end

   // geenrate clock to sequence tests
   always begin
      clk <= 1;
      #5;
      clk <= 0;
      #5;
   end

   always @ (negedge clk) begin
      if (mem_write) begin
         if (data_addr === 84 & write_data === 7) begin
            $display ("Succeeded: [84] <= 7");
            $stop;
         end else if (data_addr === 80 && write_data === 7) begin
            $display ("Succeeded: [80] <= 7");
         end else begin
            $display ("Failed: unexpected memory write");
            $stop;
         end
      end
   end

endmodule
