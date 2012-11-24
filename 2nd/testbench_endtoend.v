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

   reg key_clk, key_data;

   fake_sram fake (.ZD(ZD), .ZDP(ZDP), .ZA(ZA), .XE1(XE1), .E2A(E2A), .XE3(XE3),
            .XZBE(XZBE), .XGA(XGA), .XWA(XWA), .XZCKE(XZCKE), .ZCLKMA(ZCLKMA),
            .ADVA(ADVA), .XFT(XFT), .XLBO(XLBO), .ZZA(ZZA));

   top dut (.ZD(ZD), .ZDP(ZDP), .ZA(ZA), .XE1(XE1), .E2A(E2A), .XE3(XE3),
            .XZBE(XZBE), .XGA(XGA), .XWA(XWA), .XZCKE(XZCKE), .ZCLKMA(ZCLKMA),
            .ADVA(ADVA), .XFT(XFT), .XLBO(XLBO), .ZZA(ZZA),

            .CLK(clk), .XRST(xreset), .RS_RX(rs_rx), .RS_TX(rs_tx),

            .KEY_CLK(key_clk), .KEY_DATA(key_data));

   // in post-map simulation, other modules are not available.
   i232c #(.wtime(16'h0006)) decoder(.clk(clk), .rx(rs_tx), .data(check_data), .changed(check_changed));

   // set by instruction loader
   parameter MEM_SIZE=3578;
   parameter RS232C_DELAY=84;
   reg [31:0] RAM[MEM_SIZE-1:0];

   integer i;
   task send;
      input [7:0] data;
      begin

         // input 0_????????_1
         rs_rx <= 0;
         #RS232C_DELAY;
         for (i=0; i<8; i = i+1) begin
            rs_rx <= data[i];
            #RS232C_DELAY;
         end
         rs_rx <= 1;
         #RS232C_DELAY;

      end
   endtask

   task send_word;
      input [31:0] data;
      begin

         send(data[31:24]);
         send(data[23:16]);
         send(data[15:8]);
         send(data[7:0]);

      end
   endtask

   parameter KEY_CLK_LENGTH=30;
   task send_key_signal;
      input data;
      begin
         key_clk <= 1;
         #(KEY_CLK_LENGTH/4);
         key_data <= data;
         #(KEY_CLK_LENGTH/4);
         key_clk <= 0;
         #(KEY_CLK_LENGTH/2);
      end
   endtask

   integer i;
   task send_key;
      input [7:0] data;
      begin

         // input 0_????????_p_1

         send_key_signal(0);
         for (i=0; i < 8; i = i+1) begin
            send_key_signal(data[i]);
         end
         send_key_signal(1);  // fake parity
         send_key_signal(1);

         key_clk <= 1;

      end
   endtask

   integer j;
   // initialize test by xresetting
   initial begin
      xreset <= 0;
      rs_rx  <= 1;
      #92;
      xreset <= 1;
      #100;

      #300;

      send_word(32'hffffffff); // end marker

      #300;

      // "10\0"
      send(1);
      send(2);
      send(3);
      send(4);

      #100;
      send(8'h14); // press ctrl
      send(8'hf0);
      send(8'h14); // release ctrl
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
