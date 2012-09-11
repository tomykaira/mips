// instruction memory
// instruction is sync with clock, because block ram is far away
// TODO: use BlockRAM backend, and set data with COE
module instruction_memory (input clk,
                           input [15:0]  a,
                           output reg [31:0] rd);

   // size is fixed to 64KB = 2^16
   reg [31:0] RAM[64*1024-1:0];

   initial
      $readmemh ("instruction.dat", RAM);

   always @ (posedge clk) begin
      rd <= RAM[a]; // word align
      $display("inst: %h", rd);
   end

endmodule
