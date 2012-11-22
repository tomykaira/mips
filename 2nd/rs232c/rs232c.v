// Dispatch RS232C instructions

module rs232c(input clk,
              input [31:0]     inst,
              input [31:0]     rt,

              output reg       push_send_data,
              output reg [7:0] send_data,

              input            rx_wait,
              input [7:0]      received_data,
              output           rx_pop,

              output reg       enable,
              output           float, // always false
              output reg [4:0] addr,
              output [31:0]    data);

   parameter INPUTB  = 6'b111101;
   parameter OUTPUTB = 6'b111110;

   wire [5:0] op;

   assign op = inst[31:26];
   assign float = 1'b0;

   reg prev_rx_wait;

   assign data = {24'b0,received_data};
   assign rx_pop = (op == INPUTB && rx_wait == 0) ? 1'b1 : 1'b0;

   always @ (posedge(clk)) begin
      prev_rx_wait <= rx_wait;

      if (op == INPUTB && prev_rx_wait == 0) begin
         enable <= 1'b1;
         addr <= inst[20:16];
      end else begin
         enable <= 0;

      end
   end

   always @ (posedge(clk)) begin
      if (op == OUTPUTB) begin
         push_send_data <= 1'b1;
         send_data <= rt[7:0];

      end else
        push_send_data <= 0;
   end

endmodule
