// Wrap combinational ALU to be clock edged
module alu_wrapper (input clk,
                    input             reset,
                    input [31:0]      inst,
                    input [31:0]      rs, rt, imm,
                    output reg        enable,
                    output reg [4:0]  addr,
                    output reg [31:0] data,
                    output reg        float);

   wire i_enable;
   wire [4:0] i_addr;
   wire [31:0] i_data;
   wire i_float;
   alu alu_inst(.inst(inst), .rs(rs), .rt(rt), .imm(imm),
                .enable(i_enable), .addr(i_addr), .data(i_data), .float(i_float));

   always @ (posedge(clk)) begin
      if (reset == 1) begin
         enable <= 1'b0;
         addr   <= 5'b0;
         data   <= 32'b0;
         float  <= 1'b0;
      end else begin
         enable <= i_enable;
         addr   <= i_addr;
         data   <= i_data;
         float  <= i_float;
      end
   end

endmodule
