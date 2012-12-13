module sd_cache(input clk,

                output       sd_clk,
                output       sd_ce,
                output       sd_out,
                input        sd_in,

                output [7:0] sd_data,
                input [31:0] sd_addr,
                input        sd_go,
                output       sd_ready);

   wire [8:0] sd_index;
   wire sd_controller_go;
   wire sd_busy;

   reg [22:0] current_block;
   wire [22:0] required_block;

   sd_cont sd_cont_inst
      (.clk(clk),
       .sd_clk(sd_clk),
       .sd_ce(sd_ce),
       .sd_out(sd_out),
       .sd_in(sd_in),

       .sd_data(sd_data),
       .sd_addr(required_block),
       .sd_index(sd_index),
       .sd_go(sd_controller_go),
       .sd_busy(sd_busy));

   assign required_block =  sd_addr[31:9];
   assign sd_index = sd_addr[8:0];

   reg [2:0] state;
   // 000: default
   // 001: loaded (ready)
   // 010: required, loading started (go = 1, busy = 1)
   // 011: required, load finished (busy = 0)

   initial
     state <= 3'b0;

   always @ (posedge(clk)) begin
      case (state)
        3'b000:
           if (sd_go == 1'b1)
             if (current_block == required_block)
               state <= 3'b001;
             else
               state <= 3'b010;
           else
              state <= state;

        3'b001:
          state <= 3'b000;

        3'b010:
          if (sd_busy == 1'b0)
            state <= 3'b011;
          else
            state <= state;

        3'b011: begin
           state <= 3'b0;
           current_block <= required_block;
        end
      endcase
   end

   assign sd_ready         = state == 3'b001 ? 1'b1 : 1'b0;
   assign sd_controller_go = state == 3'b010 ? 1'b1 : 1'b0;

endmodule
