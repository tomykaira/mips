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
   wire key_clk_io, key_data_io;

   wire SD_CS, SD_SCLK, SD_DO;
   reg SD_DI;

   initial
     SD_DI <= 1'b1;

   assign key_clk_io = key_clk;
   assign key_data_io = key_data;

   fake_sram fake (.ZD(ZD), .ZDP(ZDP), .ZA(ZA), .XE1(XE1), .E2A(E2A), .XE3(XE3),
            .XZBE(XZBE), .XGA(XGA), .XWA(XWA), .XZCKE(XZCKE), .ZCLKMA(ZCLKMA),
            .ADVA(ADVA), .XFT(XFT), .XLBO(XLBO), .ZZA(ZZA));

   top dut (.ZD(ZD), .ZDP(ZDP), .ZA(ZA), .XE1(XE1), .E2A(E2A), .XE3(XE3),
            .XZBE(XZBE), .XGA(XGA), .XWA(XWA), .XZCKE(XZCKE), .ZCLKMA(ZCLKMA),
            .ADVA(ADVA), .XFT(XFT), .XLBO(XLBO), .ZZA(ZZA),

            .CLK(clk), .XRST(xreset), .RS_RX(rs_rx), .RS_TX(rs_tx),

            .KEY_CLK(key_clk_io), .KEY_DATA(key_data_io),

            .SD_CS(SD_CS), .SD_DI(SD_DI), .SD_SCLK(SD_SCLK), .SD_DO(SD_DO));

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

   // clk より十分長ければ、適当でよい
   parameter KEY_CLK_LENGTH=105;
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

   integer k;
   task send_key;
      input [7:0] keycode;
      begin

         // input 0_????????_p_1

         send_key_signal(0);
         for (k=0; k < 8; k = k+1) begin
            send_key_signal(keycode[k]);
         end
         send_key_signal(1);  // fake parity
         send_key_signal(1);

         key_clk <= 1;

      end
   endtask

   integer j;
   // initialize test by xresetting
   initial begin
      rs_rx  <= 1;
      key_clk <= 1;
      key_data <= 1;

      xreset <= 0;
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
      send_key(8'h14); // press ctrl
      send_key(8'hf0);
      send_key(8'h14); // release ctrl
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
