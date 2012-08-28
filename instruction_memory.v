// instruction memory
// TODO: use BlockRAM backend, and set data with COE
module instruction_memory (input [5:0] a,
                           output [31:0] rd);

   reg [31:0] RAM[63:0];

   initial
      $readmemh ("instruction.dat", RAM);

   assign rd = RAM[a]; // word align

endmodule
