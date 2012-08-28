// instruction memory
// TODO: use BlockRAM backend, and set data with COE
module instruction_memory (input [5:0] a,
                           output [31:0] rd);

   reg [31:0] RAM[63:0];
   integer i;

   initial begin
      RAM[0]  <= 32'h28020005; // addi $2, $0, 5
      RAM[1]  <= 32'h2803000c; // addi $3, $0, 12
      RAM[2]  <= 32'h2867fff7; // addi $7, $3, -9
      RAM[3]  <= 32'h04e22000; // or   $4, $7, $2
      RAM[4]  <= 32'h00642800; // and  $5, $3, $4
      RAM[5]  <= 32'h08a42800; // add  $5, $5, $4
      RAM[6]  <= 32'hf8a7000b; // beq  $5, $7, end
      RAM[7]  <= 32'h1c642000; // slt  $4, $3, $4
      RAM[8]  <= 32'hf8800001; // beq  $4, $0, around
      RAM[9]  <= 32'h28050000; // addi $5, $0, 0
      RAM[10] <= 32'h1ce22000; // slt  $4, $7, $2  AROUND
      RAM[11] <= 32'h08853800; // add  $7, $4, $5
      RAM[12] <= 32'h18e23800; // sub  $7, $7, $2
      RAM[13] <= 32'h10070000; // out  $7         expects: 7
      RAM[14] <= 32'hac670044; // sw   $7, 68($3)
      RAM[15] <= 32'h8c020050; // lw   $2, 80($0)
      RAM[16] <= 32'hfc000012; // J    end
      RAM[17] <= 32'h28020001; // addi $2, $0, 1
      RAM[18] <= 32'h30050000; // in   $5          END
      RAM[19] <= 32'h10050000; // out  $5         expects: input value(105 = x69 in test)
      RAM[20] <= 32'hac65004c; // sw   $5, 76($3)
      RAM[21] <= 32'h8c020050; // lw   $2, 80($0)
      RAM[22] <= 32'hac020054; // sw   $2, 84($0)
      RAM[23] <= 32'h10020000; // out  $2         expects: 7
      RAM[24] <= 32'h10000000; // out  $0  test done flag
      RAM[25] <= 32'h30000000; // in   $0          BLOCK
      for (i = 26; i < 64; i = i + 1)
        RAM[i] <= 32'h0;
   end

   assign rd = RAM[a]; // word align

endmodule
