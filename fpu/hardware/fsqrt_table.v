// fsqrt table
// table is generated from pre-software calculation
// See: fsqrt.cc, join.cc

// This is expected to be implemented on BlockRAM.
module fsqrt_table(input clk,
                  input [9:0]      key,
                  output reg [35:0] value);
   reg [35:0] RAM[1023:0];

   initial
      $readmemh ("fsqrt.dat", RAM);

   always @ (posedge clk) begin
      value <= RAM[key];
   end

endmodule
