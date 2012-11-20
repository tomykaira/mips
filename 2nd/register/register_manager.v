// register manager
// This handle read / write access to registers
module register_manager (input clk, input reset,
                         input [4:0]   rs_addr, rt_addr,
                         input         rs_float, rt_float,
                         output [31:0] rs_data, rt_data,
                         input         write_enable_misc, write_enable_alu, write_enable_mem, write_enable_fpu,
                         input [4:0]   write_addr_misc, write_addr_alu, write_addr_mem, write_addr_fpu,
                         input [31:0]  write_data_misc, write_data_alu, write_data_mem, write_data_fpu,
                         input         write_float_misc, write_float_alu, write_float_mem, write_float_fpu);

   // array x array does not works good
   wire        write_enable_0;
   wire [4:0]  write_addr_0;
   wire [31:0] write_data_0;
   wire        write_float_0;

   wire        write_enable_1;
   wire [4:0]  write_addr_1;
   wire [31:0] write_data_1;
   wire        write_float_1;

   wire        write_enable_2;
   wire [4:0]  write_addr_2;
   wire [31:0] write_data_2;
   wire        write_float_2;

   reg int_write_enable;
   reg [4:0] int_write_addr;
   reg [31:0] int_write_data;

   reg float_write_enable;
   reg [4:0] float_write_addr;
   reg [31:0] float_write_data;

   wire [1:0] rs_position, rt_position;
   reg [31:0] raw_rs_data, raw_rt_data;

   wire [31:0] int_rs_data, int_rt_data, float_rs_data, float_rt_data;

   register_queue_block register_queue_block2(.clk(clk),

                                              .enable_left(write_enable_alu),
                                              .addr_left(write_addr_alu),
                                              .data_left(write_data_alu),
                                              .float_left(write_float_alu),

                                              .enable_up(write_enable_misc),
                                              .addr_up(write_addr_misc),
                                              .data_up(write_data_misc),
                                              .float_up(write_float_misc),

                                              .enable_down(write_enable_2),
                                              .addr_down(write_addr_2),
                                              .data_down(write_data_2),
                                              .float_down(write_float_2));

   register_queue_block register_queue_block1(.clk(clk),

                                              .enable_left(write_enable_mem),
                                              .addr_left(write_addr_mem),
                                              .data_left(write_data_mem),
                                              .float_left(write_float_mem),

                                              .enable_up(write_enable_2),
                                              .addr_up(write_addr_2),
                                              .data_up(write_data_2),
                                              .float_up(write_float_2),

                                              .enable_down(write_enable_1),
                                              .addr_down(write_addr_1),
                                              .data_down(write_data_1),
                                              .float_down(write_float_1));

   register_queue_block register_queue_block0(.clk(clk),

                                              .enable_left(write_enable_fpu),
                                              .addr_left(write_addr_fpu),
                                              .data_left(write_data_fpu),
                                              .float_left(write_float_fpu),

                                              .enable_up(write_enable_1),
                                              .addr_up(write_addr_1),
                                              .data_up(write_data_1),
                                              .float_up(write_float_1),

                                              .enable_down(write_enable_0),
                                              .addr_down(write_addr_0),
                                              .data_down(write_data_0),
                                              .float_down(write_float_0));

   register_file int_register(.clk(clk),
                              .read_addr1(rs_addr),
                              .read_addr2(rt_addr),
                              .read_data1(int_rs_data),
                              .read_data2(int_rt_data),
                              .write_enable3(int_write_enable),
                              .write_addr3(int_write_addr),
                              .write_data3(int_write_data));

   register_file float_register(.clk(clk),
                                .read_addr1(rs_addr),
                                .read_addr2(rt_addr),
                                .read_data1(float_rs_data),
                                .read_data2(float_rt_data),
                                .write_enable3(float_write_enable),
                                .write_addr3(float_write_addr),
                                .write_data3(float_write_data));

   register_mux_buffer mux_buffer_rs(.addr(rs_addr),
                                     .float(rs_float),
                                     .write_enable_0(write_enable_0),
                                     .write_enable_1(write_enable_1),
                                     .write_enable_2(write_enable_2),
                                     .write_addr_0(write_addr_0),
                                     .write_addr_1(write_addr_1),
                                     .write_addr_2(write_addr_2),
                                     .write_float_0(write_float_0),
                                     .write_float_1(write_float_1),
                                     .write_float_2(write_float_2),
                                     .position(rs_position));

   register_mux_buffer mux_buffer_rt(.addr(rt_addr),
                                     .float(rt_float),
                                     .write_enable_0(write_enable_0),
                                     .write_enable_1(write_enable_1),
                                     .write_enable_2(write_enable_2),
                                     .write_addr_0(write_addr_0),
                                     .write_addr_1(write_addr_1),
                                     .write_addr_2(write_addr_2),
                                     .write_float_0(write_float_0),
                                     .write_float_1(write_float_1),
                                     .write_float_2(write_float_2),
                                     .position(rt_position));

   reg [31:0] rs_data_clk, rt_data_clk;
   reg [4:0] rs_addr_clk, rt_addr_clk;
   reg rs_float_clk, rt_float_clk;

   always @ (posedge(clk)) begin
      rs_data_clk <= raw_rs_data;
      rs_addr_clk <= rs_addr;
      rs_float_clk <= rs_float;

      rt_data_clk <= raw_rt_data;
      rt_addr_clk <= rt_addr;
      rt_float_clk <= rt_float;
   end

   // TODO: from here, instantiate forwarding and pass test
   register_forwarding rs_forwarding
     (.addr(rs_addr_clk), .data(rs_data_clk), .float(rs_float),
      .write_enable_misc(write_enable_misc), .write_enable_alu(write_enable_alu), .write_enable_mem(write_enable_mem), .write_enable_fpu(write_enable_fpu),
      .write_addr_misc(write_addr_misc), .write_addr_alu(write_addr_alu), .write_addr_mem(write_addr_mem), .write_addr_fpu(write_addr_fpu),
      .write_data_misc(write_data_misc), .write_data_alu(write_data_alu), .write_data_mem(write_data_mem), .write_data_fpu(write_data_fpu),
      .write_float_misc(write_float_misc), .write_float_alu(write_float_alu), .write_float_mem(write_float_mem), .write_float_fpu(write_float_fpu),
      .forward_data(rs_data));

   register_forwarding rt_forwarding
     (.addr(rt_addr_clk), .data(rt_data_clk), .float(rt_float),
      .write_enable_misc(write_enable_misc), .write_enable_alu(write_enable_alu), .write_enable_mem(write_enable_mem), .write_enable_fpu(write_enable_fpu),
      .write_addr_misc(write_addr_misc), .write_addr_alu(write_addr_alu), .write_addr_mem(write_addr_mem), .write_addr_fpu(write_addr_fpu),
      .write_data_misc(write_data_misc), .write_data_alu(write_data_alu), .write_data_mem(write_data_mem), .write_data_fpu(write_data_fpu),
      .write_float_misc(write_float_misc), .write_float_alu(write_float_alu), .write_float_mem(write_float_mem), .write_float_fpu(write_float_fpu),
      .forward_data(rt_data));

   always @ (*) begin
      if (write_float_0 == 1) begin
         float_write_enable <= write_enable_0;
         float_write_addr <= write_addr_0;
         float_write_data <= write_data_0;

         int_write_enable <= 0;
      end else begin
         int_write_enable <= write_enable_0;
         int_write_addr <= write_addr_0;
         int_write_data <= write_data_0;

         float_write_enable <= 0;
      end
   end

   always @ (*) begin
      case (rs_position)
        0: raw_rs_data <= write_data_0;
        1: raw_rs_data <= write_data_1;
        2: raw_rs_data <= write_data_2;
        3:
          if (rs_float == 1)
            raw_rs_data <= float_rs_data;
          else
            raw_rs_data <= int_rs_data;
      endcase
   end

   always @ (*) begin
      case (rt_position)
        0: raw_rt_data <= write_data_0;
        1: raw_rt_data <= write_data_1;
        2: raw_rt_data <= write_data_2;
        3:
          if (rt_float == 1)
            raw_rt_data <= float_rt_data;
          else
            raw_rt_data <= int_rt_data;
      endcase
   end

endmodule
