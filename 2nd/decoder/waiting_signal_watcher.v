module waiting_signal_watcher (input [31:0] inst,

                               input  rx_wait,
                               input  [7:0] key_status,

                               output freeze);

   `include "../opcode.h"

   wire [5:0] op;
   assign op = inst[31:26];

   assign freeze = (op == INPUTB && rx_wait == 1'b1
                    || op == READKEY && key_status[1] == 1'b0) ? 1'b1 : 1'b0;

endmodule
