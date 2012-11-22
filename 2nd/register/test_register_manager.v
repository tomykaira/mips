module test_register_manager ();

   reg clk;
   reg reset;

   reg [4:0]       rs_addr, rt_addr;
   reg             rs_float, rt_float;
   reg             write_enable_misc, write_enable_alu, write_enable_mem, write_enable_fpu;
   reg [4:0]       write_addr_misc, write_addr_alu, write_addr_mem, write_addr_fpu;
   reg [31:0]      write_data_misc, write_data_alu, write_data_mem, write_data_fpu;
   reg             write_float_misc, write_float_alu, write_float_mem, write_float_fpu;

   wire [31:0] rs_data, rt_data;

   register_manager dut (clk, reset, rs_addr, rt_addr, rs_float, rt_float,
                         rs_data, rt_data,
                         write_enable_misc, write_enable_alu, write_enable_mem, write_enable_fpu,
                         write_addr_misc, write_addr_alu, write_addr_mem, write_addr_fpu,
                         write_data_misc, write_data_alu, write_data_mem, write_data_fpu,
                         write_float_misc, write_float_alu, write_float_mem, write_float_fpu);

   task test1;
      input [31:0] data;
      begin

         #10;

         if (rs_data !== data) begin
            $display ("FAIL");
            $stop;
         end
      end
   endtask

   task test2;
      input [31:0] rs, rt;
      begin

         #10;

         if (rs_data !== rs) begin
            $display ("FAIL rs");
            $stop;
         end

         if (rt_data !== rt) begin
            $display ("FAIL rt");
            $stop;
         end
      end
   endtask

   task test_4clk;
      input [31:0] data;
      begin
         test1(data);
         test1(data);
         test1(data);
         test1(data);
      end
   endtask

   task test_rs_rt_4clk;
      input [31:0] rs;
      input [31:0] rt;
      begin
         test2(rs, rt);
         test2(rs, rt);
         test2(rs, rt);
         test2(rs, rt);
      end
   endtask

   initial begin
      reset <= 1;
      #22;
      reset <= 0;
      #10;

      write_enable_misc <= 0;
      write_enable_alu <= 0;
      write_enable_mem <= 0;
      write_enable_fpu <= 0;

      #10;

      write_enable_alu <= 1;
      write_addr_alu <= 4;
      write_data_alu <= 8;
      write_float_alu <= 0;

      #10;

      write_enable_alu <= 0;

      rs_addr <= 4;
      rs_float <= 0;

      test_4clk(8);

      #10;

      // overwrite with misc
      write_enable_misc <= 1;
      write_addr_misc <= 4;
      write_data_misc <= 16;
      write_float_misc <= 0;

      #10;

      write_enable_misc <= 0;

      rs_addr <= 4;
      rs_float <= 0;

      test_4clk(16);

      // overwrite with mem
      write_enable_mem <= 1;
      write_addr_mem <= 4;
      write_data_mem <= 42;
      write_float_mem <= 0;

      #10;

      write_enable_mem <= 0;

      rs_addr <= 4;
      rs_float <= 0;

      test_4clk(42);

      write_enable_fpu <= 1;
      write_addr_fpu <= 4;
      write_data_fpu <= 92;
      write_float_fpu <= 1;

      #10;

      write_enable_fpu <= 0;

      rs_addr <= 4;
      rs_float <= 1;

      test_4clk(92);

      rs_addr <= 4;
      rs_float <= 0;

      test1(42);

      write_enable_alu <= 1;
      write_addr_alu <= 4;
      write_data_alu <= 92;
      write_float_alu <= 0;

      write_enable_mem <= 1;
      write_addr_mem <= 8;
      write_data_mem <= 3;
      write_float_mem <= 1;

      write_enable_fpu <= 1;
      write_addr_fpu <= 4;
      write_data_fpu <= 30;
      write_float_fpu <= 1;

      #10;

      write_enable_alu <= 0;
      write_enable_mem <= 0;
      write_enable_fpu <= 0;

      rs_addr <= 4;
      rs_float <= 0;
      rt_addr <= 4;
      rt_float <= 1;

      test_rs_rt_4clk(92, 30);

      rs_addr <= 8;
      rs_float <= 1;

      test1(3);

      // you read data just when you write by forwarding
      write_enable_alu <= 1;
      write_addr_alu <= 5;
      write_data_alu <= 29;
      write_float_alu <= 0;

      rs_addr <= 5;
      rs_float <= 0;

      test1(29);

      write_enable_alu <= 0;

      test_4clk(29);
   end

   // geenrate clock to sequence tests
   always begin
      clk <= 1;
      #5;
      clk <= 0;
      #5;
   end

endmodule
