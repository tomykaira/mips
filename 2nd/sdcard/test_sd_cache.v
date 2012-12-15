module test_sd_cache ();

   reg clk;

   reg read, write;
   reg busy;

   wire read_spi, write_spi, write_ram, ready;

   reg [31:0] addr;
   wire [22:0] block;

   sd_cache dut (clk, read, write, busy, read_spi, write_spi, write_ram, addr, block, ready);

   task assert_read_spi;
      begin
         if (read_spi !== 1'b1) begin
            $display ("FAIL: read_spi");
            $stop;
         end
      end
   endtask

   task assert_not_read_spi;
      begin
         if (read_spi !== 1'b0) begin
            $display ("FAIL: not read_spi");
            $stop;
         end
      end
   endtask

   task assert_write_spi;
      begin
         if (write_spi !== 1'b1) begin
            $display ("FAIL: write_spi");
            $stop;
         end
      end
   endtask

   task assert_not_write_spi;
      begin
         if (write_spi !== 1'b0) begin
            $display ("FAIL: not write_spi");
            $stop;
         end
      end
   endtask

   task assert_write_ram;
      begin
         if (write_ram !== 1'b1) begin
            $display ("FAIL: write_ram");
            $stop;
         end
      end
   endtask

   task assert_not_write_ram;
      begin
         if (write_ram !== 1'b0) begin
            $display ("FAIL: not write_ram");
            $stop;
         end
      end
   endtask

   task assert_ready;
      begin
         if (ready !== 1'b1) begin
            $display ("FAIL: ready");
            $stop;
         end
      end
   endtask

   task assert_not_ready;
      begin
         if (ready !== 1'b0) begin
            $display ("FAIL: not ready");
            $stop;
         end
      end
   endtask

   task assert_block_is;
      input [22:0] expected;
      begin
         if (expected !== block) begin
            $display ("FAIL: block not met expected: %h actual: %h", expected, block);
            $stop;
         end
      end
   endtask

   initial begin
      busy <= 1'h1;
      #28;
      busy <= 1'h0;
      #10;
      read <= 1'b0;
      write <= 0;

      // issue read
      read <= 1'b1;
      addr <= 32'h00000000;
      #10;
      assert_read_spi();
      assert_block_is(23'h0);
      assert_not_ready();
      assert_not_write_ram();
      assert_not_write_spi();
      busy <= 1'h1;
      #50;
      busy <= 1'h0;
      #10;
      #10;
      #10;
      assert_ready();
      assert_not_write_ram();

      // read from the same block
      addr <= 32'h00000055;
      #10;
      #10;
      assert_ready();

      // read from different block
      addr <= 32'h00030005;
      #10;
      assert_read_spi();
      assert_block_is(23'h000180);
      assert_not_ready();
      busy <= 1'h1;
      #50;
      busy <= 1'h0;
      #10;
      #10;
      #10;
      assert_ready();

      // read from the same block
      addr <= 32'h00030005;
      #10;
      #10;
      assert_ready();

      // address is for different block, but read = 0
      read <= 1'b0;
      addr <= 32'h00020005;
      #10;
      #10;
      assert_not_read_spi();
      assert_not_write_spi();
      assert_not_ready();

      ////////////////
      // write

      // write into current block
      write <= 1;
      addr <= 32'h00030008;
      #10;
      #10;
      assert_ready();
      assert_write_ram();
      assert_not_write_spi();

      // read current block
      write <= 0;
      read <= 1;
      addr <= 32'h00030009;
      #10;
      #10;
      assert_ready();
      assert_not_read_spi();
      assert_not_write_ram();

      // read other block
      write <= 0;
      read <= 1;
      addr <= 32'h00000009;
      #10;
      assert_not_ready();
      assert_not_read_spi();
      assert_write_spi();
      assert_block_is(23'h000180);
      busy <= 1;
      #50;
      busy <= 0;
      #10;
      #10;
      assert_read_spi();
      assert_not_write_spi();
      assert_block_is(23'h0);
      busy <= 1;
      #50;
      busy <= 0;
      #10;
      #10;
      #10;
      assert_ready();

      // write other block
      write <= 1;
      read <= 0;
      addr <= 32'h00040009;
      #10;
      assert_not_ready();
      assert_read_spi();
      assert_not_write_spi();
      assert_not_write_ram();
      assert_block_is(23'h000200);
      busy <= 1;
      #50;
      busy <= 0;
      #10;
      #10;
      #10;
      assert_write_ram();
      assert_ready();

      // write other block
      write <= 1;
      read <= 0;
      addr <= 32'h00060009;
      #10;
      assert_write_spi();
      assert_block_is(23'h000200);
      assert_not_ready();
      busy <= 1;
      #50;
      busy <= 0;
      #10;
      #10;
      assert_read_spi();
      assert_not_ready();
      assert_block_is(23'h000300);
      busy <= 1;
      #50;
      busy <= 0;
      #10;
      #10;
      #10;
      assert_write_ram();
      assert_ready();

   end

   // geenrate clock to sequence tests
   always begin
      clk <= 1;
      #5;
      clk <= 0;
      #5;
   end

endmodule
