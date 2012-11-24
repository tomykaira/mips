module waiting_signal_watcher (input [31:0] inst,

                               input  rx_wait,
                               input  [7:0] key_status,

                               output freeze);

   parameter INPUTB  = 6'b111101;
   parameter READKEY = 6'b001001;

   wire [5:0] op;
   assign op = inst[31:26];

   assign freeze = (op == INPUTB && rx_wait == 1'b1
                    || op == READKEY && key_status[0] == 1'b1) ? 1'b1 : 1'b0;

endmodule
