module testbench_endtoend();

   reg clk;
   reg xreset;
   reg rs_rx;
   wire rs_tx;

   wire check_changed;
   wire [7:0] check_data;

   // for sram
   wire [31:0] ZD;
   wire [3:0]  ZDP;
   wire [19:0] ZA;
   wire [3:0]  XZBE;
   wire [1:0]  ZCLKMA;
   wire XE1, E2A, XE3, XGA, XWA, XZCKE, ADVA, XFT, XLBO, ZZA;

   // debug output
   integer fd;

   fake_sram fake (.ZD(ZD), .ZDP(ZDP), .ZA(ZA), .XE1(XE1), .E2A(E2A), .XE3(XE3),
            .XZBE(XZBE), .XGA(XGA), .XWA(XWA), .XZCKE(XZCKE), .ZCLKMA(ZCLKMA),
            .ADVA(ADVA), .XFT(XFT), .XLBO(XLBO), .ZZA(ZZA));
   
   top dut (.ZD(ZD), .ZDP(ZDP), .ZA(ZA), .XE1(XE1), .E2A(E2A), .XE3(XE3),
            .XZBE(XZBE), .XGA(XGA), .XWA(XWA), .XZCKE(XZCKE), .ZCLKMA(ZCLKMA),
            .ADVA(ADVA), .XFT(XFT), .XLBO(XLBO), .ZZA(ZZA),

            .CLK(clk), .XRST(xreset), .RS_RX(rs_rx), .RS_TX(rs_tx));

   // in post-map simulation, other modules are not available.
   i232c decoder(.clk(clk), .enable(1'b1), .rx(rs_tx), .data(check_data), .changed(check_changed));

   integer i;
   task send;
      input [7:0] data;
      begin

         // input 0_????????_1
         rs_rx <= 0;
         #2000;
         for (i=0; i<8; i = i+1) begin
            rs_rx <= data[i];
            #2000;
         end
         rs_rx <= 1;
         #2000;

      end
   endtask

   // initialize test by xresetting
   initial begin
      xreset <= 0;
      rs_rx  <= 1;
      #22;
      xreset <= 1;
      #100;

      #6000;

      // "10\0"
      send(49);
      send(48);
      send(0);
   end
  
   // geenrate clock to sequence tests
   // 14 ns / clock is realistic
   always begin
      clk <= 1;
      #7;
      clk <= 0;
      #7;
   end

endmodule
