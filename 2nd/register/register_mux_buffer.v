// find requried data from register buffer
// position 0 - 2: corresponding buffer
//              3: not found
// higher buffer has higher priority
module register_mux_buffer (input [4:0]      addr,
                            input            float,

                            input            write_enable_0,
                            input            write_enable_1,
                            input            write_enable_2,

                            input [4:0]      write_addr_0, // workaround that multi-dimentional port is not allowed
                            input [4:0]      write_addr_1,
                            input [4:0]      write_addr_2,

                            input            write_float_0,
                            input            write_float_1,
                            input            write_float_2,

                            output reg [1:0] position);

   always @ (*) begin
      if (write_enable_2 == 1 && write_addr_2 == addr && write_float_2 == float)
        position <= 2;
      else if (write_enable_1 == 1 && write_addr_1 == addr && write_float_1 == float)
        position <= 1;
      else if (write_enable_0 == 1 && write_addr_0 == addr && write_float_0 == float)
        position <= 0;
      else
        position <= 3;
   end

endmodule
