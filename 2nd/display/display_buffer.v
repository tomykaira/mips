module display_buffer (input clk,
                       input             write_enable,
                       input [13:0]      address,
                       input [11:0]      write_data,
                       output reg [11:0] read_data);
   parameter MEM_SIZE=16384;

   reg [11:0] RAM[MEM_SIZE-1:0];
   reg [13:0] write_counter;

   initial
      write_counter <= 14'b0;

   always @ (posedge clk) begin
      if (write_enable == 1) begin
         RAM[write_counter] <= write_data;
         write_counter <= write_counter + 1;
      end
      else begin
         read_data <= RAM[address];
      end
   end

endmodule
