module test_sd_cache ();

   reg clk;

   reg [31:0] sd_addr;
   reg sd_go;
   reg sd_busy;

   wire to_load, ready;

   sd_cache dut (clk, sd_go, sd_addr, sd_busy, to_load, ready);

   task assert_load;
      begin
         if (to_load !== 1'b1) begin
            $display ("FAIL: load");
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

   task assert_not_load;
      begin
         if (to_load !== 1'b0) begin
            $display ("FAIL: not load");
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

   initial begin
      sd_busy <= 1'h1;
      #28;
      sd_busy <= 1'h0;
      #10;
      sd_go <= 1'b0;

      sd_go <= 1'b1;
      sd_addr <= 32'h00000000;
      #10;
      assert_load();
      assert_not_ready();
      sd_busy <= 1'h1;
      #50;
      sd_busy <= 1'h0;
      #10;
      #10;
      assert_ready();

      sd_addr <= 32'h00000055;
      #10;
      assert_ready();

      sd_addr <= 32'h00030005;
      #10;
      assert_load();
      assert_not_ready();
      sd_busy <= 1'h1;
      #50;
      sd_busy <= 1'h0;
      #10;
      #10;
      assert_ready();

      sd_addr <= 32'h00030005;
      #10;
      assert_ready();

      sd_go <= 1'b0;
      sd_addr <= 32'h00020005;
      #10;
      assert_not_load();
      assert_not_ready();
   end

   // geenrate clock to sequence tests
   always begin
      clk <= 1;
      #5;
      clk <= 0;
      #5;
   end

endmodule
