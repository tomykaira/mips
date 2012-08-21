// instruction memory
// TODO: use BlockRAM backend, and set data with COE
module instruction_memory (input [5:0] a,
                           output [31:0] rd);

   reg [31:0] RAM[63:0];
   integer i;

   initial begin
      RAM[0]  <= 32'h20020005;
      RAM[1]  <= 32'h20020005;
      RAM[2]  <= 32'h2003000c;
      RAM[3]  <= 32'h2067fff7;
      RAM[4]  <= 32'h00e22025;
      RAM[5]  <= 32'h00642824;
      RAM[6]  <= 32'h00a42820;
      RAM[7]  <= 32'h10a7000a;
      RAM[8]  <= 32'h0064202a;
      RAM[9]  <= 32'h10800001;
      RAM[10] <= 32'h20050000;
      RAM[11] <= 32'h00e2202a;
      RAM[12] <= 32'h00853820;
      RAM[13] <= 32'h00e23822;
      RAM[14] <= 32'hac670044;
      RAM[15] <= 32'h8c020050;
      RAM[16] <= 32'h08000011;
      RAM[17] <= 32'h20020001;
      RAM[18] <= 32'hac020054;
      for (i = 19; i < 64; i = i + 1)
        RAM[i] <= 32'h0;
   end

   assign rd = RAM[a]; // word align

endmodule
         
