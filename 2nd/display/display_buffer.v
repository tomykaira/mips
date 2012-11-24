module display_buffer (input clk,
                           input            write_enable,
                           input [11:0]     address,
                           input [6:0]      write_data,
                           output reg [6:0] read_data);
   parameter MEM_SIZE=2400;

   reg [6:0] RAM[MEM_SIZE-1:0];

   initial
     $readmemb ("initial_text.dat", RAM);

   always @ (posedge clk) begin
      if (write_enable == 1) begin
         RAM[address] <= write_data;
      end
      else begin
         read_data <= RAM[address];
      end
   end

endmodule
