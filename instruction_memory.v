// instruction memory
// TODO: use BlockRAM backend, and set data with COE
module instruction_memory (input  [15:0] a,
                           output [31:0] rd);

   // size is fixed to 64KB = 2^16
   reg [31:0] RAM[64*1024-1:0];

   initial
      $readmemh ("instruction.dat", RAM);

   assign rd = RAM[a]; // word align

endmodule
