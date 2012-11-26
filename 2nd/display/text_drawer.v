module text_drawer(input clk,
                   input        reset,

                   output [7:0] red,green,blue,
                   input [8:0]  row,
                   input [9:0]  column,

                   input        buffer_write_enable,
                   input [11:0] position,
                   input [6:0]  char_code);

   reg [127:0] BITMAP[127:0];

   // bitmap ファイルは converter で都度生成する
   initial
     $readmemh ("bitmap.dat", BITMAP);

   wire [11:0] display_address;
   assign display_address = {2'b0,row[8:5],5'b0} + {4'b0,row[8:5],3'b0} + {5'b0,column[9:4]};

   wire [6:0] char_to_show;
   display_buffer display_buffer_inst
     (.clk(clk),
      .write_enable(buffer_write_enable),
      .address(buffer_write_enable == 1'b1 ? position : display_address),
      .write_data(char_code),
      .read_data(char_to_show));

   wire [127:0] char_bitmap;
   assign char_bitmap = BITMAP[char_to_show];

   wire [6:0] index;
   assign index = 7'h7f - {row[4:1],column[3:1]}; // reverse order

   wire pixel_on;
   assign pixel_on = char_bitmap[index];

   assign red   = 8'h0;
   assign green = pixel_on == 1 ? 8'hff : 8'h0;
   assign blue  = 8'h0;

endmodule
