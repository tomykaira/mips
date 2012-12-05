// instruction memory
// instruction is sync with clock, because block ram is far away
// TODO: use BlockRAM backend, and set data with COE
module instruction_memory (input clk,
                           input [15:0]      read_address,
                           output reg [31:0] read_data,
                           input             write_enable,
                           input [15:0]      write_address,
                           input [31:0]      write_data);
   parameter MEM_SIZE=20000; // resize if instruction exceeds this.

   reg [31:0] RAM[MEM_SIZE-1:0];

   initial
     $readmemh ("instruction.dat", RAM);

   always @ (posedge clk) begin
      if (write_enable == 1) begin
         RAM[write_address] <= write_data;
         $display("WRITE: %h: %h", write_address, write_data);
      end
      read_data <= RAM[read_address]; // word align
      $display("INST: %8d %h", read_address, read_data);
   end

endmodule
