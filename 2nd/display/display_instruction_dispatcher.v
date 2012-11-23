// Dispatch display instructions to display modules

module display_instruction_dispatcher(input clk,
                                      input [31:0]      inst,
                                      input [31:0]      rs,
                                      input [31:0]      rt,

                                      output reg        buffer_write_enable
                                      output reg [11:0] position,
                                      output reg [6:0] char_code);

   parameter DISPLAY = 6'b001000;

   wire [5:0] op;

   assign op = inst[31:26];

   always @ (posedge(clk)) begin
      if (op == DISPLAY) begin
         buffer_write_enable <= 1'b1;
         position <= rs[11:0];
         char_code <= rt[6:0];

      end else begin
         buffer_write_enable <= 0;
         position <= 11'b0;
         char_code <= 7'b0;

      end
   end

endmodule
