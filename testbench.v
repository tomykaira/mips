module testbench();

   reg clk;
   reg xreset;
   reg rs_rx;
   wire [31:0] send_data;
   wire send_enable;

   test_top dut (clk, xreset, rs_rx, send_data, send_enable);

   // initialize test by xresetting
   initial begin
      xreset <= 0;
      rs_rx  <= 1;
      #22;
      xreset <= 1;
      #180;

      // input 0_10010110_1
      rs_rx <= 0;
      #50;
      rs_rx <= 1;
      #50;
      rs_rx <= 0;
      #50;
      rs_rx <= 0;
      #50;
      rs_rx <= 1;
      #50;
      rs_rx <= 0;
      #50;
      rs_rx <= 1;
      #50;
      rs_rx <= 1;
      #50;
      rs_rx <= 0;
      #50;
      rs_rx <= 1;
   end

   // geenrate clock to sequence tests
   always begin
      clk <= 1;
      #5;
      clk <= 0;
      #5;
   end

   always @ (negedge clk) begin
      if (send_enable) begin
         if (send_data === 0) begin
            $display ("Succeeded: 0, test end");
            $stop;
         end if (send_data === 7) begin
            $display ("Succeeded: 7");
         end else if (send_data === 105) begin
            $display ("Succeeded: 105");
         end else begin
            $display ("Failed: unexpected sending");
            $stop;
         end
      end
   end

endmodule
