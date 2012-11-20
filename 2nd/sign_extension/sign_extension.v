// clock edged sign extension
module sign_extension (input clk,
                       input [15:0]  imm_in,
                       output reg [31:0] imm_out);

   always @ (posedge(clk)) begin
      imm_out[31:15] <= {17{imm_in[15]}};
      imm_out[14:0] <= imm_in[14:0];
   end
endmodule
