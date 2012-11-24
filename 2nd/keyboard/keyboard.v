// Wrap keyboard driver, translate signals, add metadata

module keyboard(input clk,
                input        key_clk,
                input        key_data,
                output [7:0] key_status,
                output [7:0] keycode);

   wire is_break;
   keyboard_driver driver_inst
     (.clk(clk),
      .key_clk(key_clk),
      .key_code(keycode)
      .is_break(is_break));

   assign key_status = {7'b0, is_break};
endmodule
