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

   assign op      = inst[31:26];

   assign sd_read  = op == READSD  && sd_ready == 1'b0 ? 1'b1 : 1'b0;
   assign sd_write = op == WRITESD && sd_ready == 1'b0 ? 1'b1 : 1'b0;

   assign sd_write_data = rs[7:0];
   assign sd_addr = rs;

   assign data    = {24'b0,sd_read_data};
   assign enable  = op == READSD && sd_ready == 1'b1 ? 1'b1 : 1'b0;
   assign addr    = inst[20:16];
   assign float   = 1'b0;

endmodule
