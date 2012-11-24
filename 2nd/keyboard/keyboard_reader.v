// read keyboard input when instruction received

module keyboard_reader(input clk,
                       input [31:0]     inst,

                       // for extension
                       // 0: is_break
                       // 1: signal_comes_now
                       input [7:0]      key_status,
                       input [7:0]      keycode,

                       output reg       enable,
                       output           float, // always false
                       output reg [4:0] addr,
                       output [31:0]    data);

   parameter READKEY = 6'b001001;

   wire [5:0] op;

   assign op = inst[31:26];
   assign float = 1'b0;

   always @ (posedge(clk)) begin
      if (op == readkey && key_status[1] == 1'b1) begin
         enable <= 1'b1;
         addr <= inst[20:16];
         data <= {24'b0, keycode};
      end else
        enable <= 1'b0;
   end

endmodule;
