module instruction_writer (input clk,
                           input [31:0] inst,
                           input [31:0]      rs, rt,
                           output reg        write_enable,
                           output reg [15:0] address,
                           output reg [31:0] write_data);

   wire [5:0] op;

   assign op = inst[31:26];

   always @ (posedge(clk)) begin
      if (op == 6'b001010)
        write_enable <= 1'b1;
      else
        write_enable <= 1'b0;

      address <= rs[15:0];
      write_data <= rt;
   end

endmodule
