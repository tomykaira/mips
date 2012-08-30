module testbench_endtoend();

   reg clk;
   reg xreset;
   reg rs_rx;
   wire rs_tx;

   wire check_changed;
   wire [7:0] check_data;

   top dut (.CLK(clk), .XRST(xreset), .RS_RX(rs_rx), .RS_TX(rs_tx));

   // in post-map simulation, other modules are not available.
   // i232c decoder(.clk(clk), .enable(1'b1), .rx(rs_tx), .data(check_data), .changed(check_changed));

   // initialize test by xresetting
   initial begin
      xreset <= 0;
      rs_rx  <= 1;
      #22;
      xreset <= 1;
      #260;

      rs_rx <= 0;
      #2000;
      rs_rx <= 0;
      #2000;
      rs_rx <= 1;
      #2000;
      rs_rx <= 0;
      #2000;
      rs_rx <= 1;
      #2000;
      rs_rx <= 0;
      #2000;
      rs_rx <= 0;
      #2000;
      rs_rx <= 0;
      #2000;
      rs_rx <= 0;
      #2000;
      rs_rx <= 1;
   end

   // geenrate clock to sequence tests
   always begin
      clk <= 1;
      #7;
      clk <= 0;
      #7;
   end

endmodule
