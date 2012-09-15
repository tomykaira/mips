// finv table
// table is generated from pre-software calculation
// See: finv_ikuta.cc, join.cc

// This is expected to be implemented on BlockRAM.
module finv_table(input clk,
                  input [9:0]      key,
                  output reg [35:0] value);
   reg [35:0] RAM[1023:0];

   initial
      $readmemh ("finv.dat", RAM);

   always @ (posedge clk) begin
      value <= RAM[key];
   end

endmodule
