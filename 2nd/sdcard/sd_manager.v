// Dispatch RS232C instructions

module sd_manager(input clk,
                  input [31:0]  inst,
                  input [31:0]  rs,
                  input [31:0]  rt,

                  output        enable,
                  output        float, // always false
                  output [4:0]  addr,
                  output [31:0] data,

                  input [7:0]   sd_read_data,
                  output [7:0]  sd_write_data,
                  output [31:0] sd_addr,
                  output        sd_read,
                  output        sd_write,
                  input         sd_ready);

   `include "../opcode.h"

   wire [5:0] op;

   reg [1:0] was_ready;
   wire ready;

   // wait for register delay, this should work like multi-clock cycle
   always @ (posedge(clk)) begin
      was_ready[1] <= was_ready[0];
      was_ready[0] <= sd_ready;
   end

   assign ready = (sd_ready == 1 || was_ready[0] == 1 || was_ready[1] == 1) ? 1'b1 : 1'b0;

   assign op      = inst[31:26];

   assign sd_read  = op == READSD  && ready == 0 ? 1'b1 : 1'b0;
   assign sd_write = op == WRITESD && ready == 0 ? 1'b1 : 1'b0;

   assign sd_write_data = rt[7:0];
   assign sd_addr = rs;

   assign data    = {24'b0,sd_read_data};
   assign enable  = op == READSD && sd_ready == 1'b1 ? 1'b1 : 1'b0;
   assign addr    = inst[20:16];
   assign float   = 1'b0;

endmodule
