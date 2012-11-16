module testbench_sramc();

   reg clk;

   // for sram
   wire [31:0] ZD;
   wire [3:0]  ZDP;
   wire [19:0] ZA;
   wire [3:0]  XZBE;
   wire [1:0]  ZCLKMA;
   wire XE1, E2A, XE3, XGA, XWA, XZCKE, ADVA, XFT, XLBO, ZZA;

   wire [31:0] data_read;
   reg [31:0] data_write;
   reg [19:0] address;
   reg write_enable;

   assign XZBE      = 4'b0;
   assign XE1       = 0;
   assign E2A       = 1;
   assign XE3       = 0;
   assign XGA       = 0;
   assign XZCKE     = 0;
   assign ZCLKMA[0] = clk;
   assign ZCLKMA[1] = clk;
   assign ADVA      = 0;
   assign XFT       = 1;
   assign XLBO      = 1;
   assign ZZA       = 0;

   task write;
      input [31:0] data;
      input [31:0] addr;
      begin
         data_write   <= data;
         write_enable <= 1;
         address      <= addr;
         #14;
         #14;
         #14;
      end
   endtask

   task read;
      input [31:0] addr;
      begin
         write_enable <= 0;
         address      <= addr;
         #14;
         #14;
         #14;
      end
   endtask
      

   fake_sram fake (.ZD(ZD), .ZDP(ZDP), .ZA(ZA), .XE1(1'b0), .E2A(1'b1), .XE3(1'b0),
            .XZBE(XZBE), .XGA(1'b0), .XWA(XWA), .XZCKE(XZCKE), .ZCLKMA(ZCLKMA),
            .ADVA(ADVA), .XFT(XFT), .XLBO(XLBO), .ZZA(ZZA));

   sramc dut (.ZD(ZD), .ZDP(ZDP), .ZA(ZA), .XWA(XWA),
              .data_read(data_read), .data_write(data_write), .address(address),
              .write_enable(write_enable), .clk(clk));

   // initialize test by x-resetting
   initial begin
      #12; // strobe
      write(32'h0, 9);

      write(32'hffffffff, 15);

      write(32'hffff0000, 92);

      write(32'h0000ffff, 12);

      write(32'hcccccccc, 42);

      write(32'h33333333, 83);

      write(32'haaaaaaaa, 99);

      write(32'h66666666, 99);

      read(9);
      read(15);
      read(92);
      read(12);
      read(42);
      read(83);
      read(99);

      $finish;
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
