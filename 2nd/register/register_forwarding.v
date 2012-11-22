// Insert this circuit after mux

module register_forwarding(input [4:0] addr,
                           input [31:0]  data,
                           input         float,

                           input         write_enable_misc, write_enable_alu, write_enable_mem, write_enable_fpu,
                           input [4:0]   write_addr_misc, write_addr_alu, write_addr_mem, write_addr_fpu,
                           input [31:0]  write_data_misc, write_data_alu, write_data_mem, write_data_fpu,
                           input         write_float_misc, write_float_alu, write_float_mem, write_float_fpu,

                           output [31:0] forward_data);

   wire [31:0] fpu_data, mem_data, alu_data;

   forwarding forwarding_fpu
     (.addr(addr),
      .data(data),
      .float(float),

      .write_addr(write_addr_fpu),
      .write_data(write_data_fpu),
      .write_enable(write_enable_fpu),
      .write_float(write_float_fpu),

      .forward_data(fpu_data));

   forwarding forwarding_mem
     (.addr(addr),
      .data(fpu_data),
      .float(float),

      .write_addr(write_addr_mem),
      .write_data(write_data_mem),
      .write_enable(write_enable_mem),
      .write_float(write_float_mem),

      .forward_data(mem_data));

   forwarding forwarding_alu
     (.addr(addr),
      .data(mem_data),
      .float(float),

      .write_addr(write_addr_alu),
      .write_data(write_data_alu),
      .write_enable(write_enable_alu),
      .write_float(write_float_alu),

      .forward_data(alu_data));

   forwarding forwarding_misc
     (.addr(addr),
      .data(alu_data),
      .float(float),

      .write_addr(write_addr_misc),
      .write_data(write_data_misc),
      .write_enable(write_enable_misc),
      .write_float(write_float_misc),

      .forward_data(forward_data));
endmodule
