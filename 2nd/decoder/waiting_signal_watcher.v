module waiting_signal_watcher (input [31:0] inst,

                               input  rx_wait,

                               output freeze);

   parameter INPUTB  = 6'b111101;

   wire [5:0] op;
   assign op = inst[31:26];

   assign freeze = (op == INPUTB && rx_wait == 1'b1) ? 1'b1 : 1'b0;

endmodule
