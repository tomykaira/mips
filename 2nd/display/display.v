module display(input clk,
               input        clk100,
               input        reset,

               input        buffer_write_enable,
               input [11:0] position,
               input [6:0]  char_code,

               output [7:0] r_data,g_data,b_data,
               output       vs_data, hs_data);

   reg vga_reset;
   reg [1:0] counter;
   reg clk25;

   initial begin
      vga_reset <= 1'b1;
      counter <= 2'b00;
   end

   // generate pseudo clock.  25MHz is too slow to generate from DCM
   always @ (posedge(clk100)) begin
      if (counter == 2'b00 || counter == 2'b01)
        clk25 <= 1'b1;
      else if (counter == 2'b10 || counter == 2'b11)
        clk25 <= 1'b0;

      counter <= counter + 1;

      if (vga_reset == 1'b1 && counter == 2'b11)
        vga_reset <= 1'b0;
   end

   wire [7:0] red, green, blue;
   wire [8:0] row;
   wire [9:0] column;
   text_drawer text_drawer_inst
      (.clk(clk), .reset(reset),
       .red(red), .green(green), .blue(blue), .row(row), .column(column),
       .buffer_write_enable(buffer_write_enable), .position(position), .char_code(char_code));

   vga vga_inst
      (.clk(clk25),
       .reset(vga_reset),
       .red(red), .green(green), .blue(blue),
       .r(r_data), .g(g_data), .b(b_data),
       .hsync(hs_data), .vsync(vs_data), .row(row), .column(column));

endmodule
