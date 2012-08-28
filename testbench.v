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

      // input 0_01010000_1
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
      rs_rx <= 0;
      #50;
      rs_rx <= 0;
      #50;
      rs_rx <= 0;
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
         end if (send_data === 1)
           $display ("Succeeded: 1");
         else if (send_data === 2)
           $display ("Succeeded: 2");
         else if (send_data === 3)
           $display ("Succeeded: 3");
         else if (send_data === 5)
           $display ("Succeeded: 5");
         else if (send_data === 8)
           $display ("Succeeded: 8");
         else if (send_data === 13)
           $display ("Succeeded: 13");
         else if (send_data === 21)
           $display ("Succeeded: 21");
         else if (send_data === 34)
           $display ("Succeeded: 34");
         else if (send_data === 55)
           $display ("Succeeded: 55");
         else if (send_data === 89)
           $display ("Succeeded: 89");
         else begin
            $display ("Failed: unexpected sending %d", send_data);
            // $stop;
         end
      end
   end

endmodule
