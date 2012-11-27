module image_drawer(input clk,
                   input        reset,

                   output [7:0] red,green,blue,
                   input [8:0]  row,
                   input [9:0]  column,

                   input        buffer_write_enable,
                   input [23:0] color_code);

   wire [6:0] x;
   wire [6:0] y;
   assign x = column[9:1] - 121;
   assign y = row[8:1] - 56;

   wire [13:0] display_address;
   assign display_address = {y, 7'b0} + x;

   wire [11:0] compressed;

   assign compressed = {color_code[23:20], color_code[15:12], color_code[7:4]};

   wire [11:0] rgb;
   display_buffer display_buffer_inst
     (.clk(clk),
      .write_enable(buffer_write_enable),
      .address(display_address),
      .write_data(compressed),
      .read_data(rgb));

   wire display_in_range;
   wire boundary;
   assign display_in_range = column >= 242 && column < 498 && row >= 112 && row < 368;
   assign boundary = column == 241 || column == 498 || row == 111 || row == 368;

   assign red   = boundary == 1'b1 ? 8'hff : display_in_range == 1'b1 ? {rgb[11:8], 4'b0} : 8'b0;
   assign green = display_in_range == 1'b1 ? {rgb[ 7:4], 4'b0} : 8'b0;
   assign blue  = display_in_range == 1'b1 ? {rgb[ 3:0], 4'b0} : 8'b0;

endmodule
