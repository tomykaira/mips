module testbench_endtoend();

   reg clk;
   reg xreset;
   wire rs_rx, rs_tx;

   top dut (.CLK(clk), .XRST(xreset), .RS_RX(rs_rx), .RS_TX(rs_tx));

   // initialize test by xresetting
   initial begin
      xreset <= 0;
      #22;
      xreset <= 1;
   end

   // geenrate clock to sequence tests
   always begin
      clk <= 1;
      #7;
      clk <= 0;
      #7;
   end

endmodule
