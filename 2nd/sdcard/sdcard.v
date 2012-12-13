module sdcard(input clk,

                output       sd_clk,
                output       sd_ce,
                output       sd_out,
                input        sd_in,

                output [7:0] sd_data,
                input [31:0] sd_addr,
                input        sd_go,
                output       sd_ready);

   wire sd_busy;
   wire sd_controller_go;

   wire [8:0] sd_index;

   assign sd_index = sd_addr[8:0];

   sd_cont sd_cont_inst
      (.clk(clk),
       .sd_clk(sd_clk),
       .sd_ce(sd_ce),
       .sd_out(sd_out),
       .sd_in(sd_in),

       .sd_data(sd_data),
       .sd_addr(sd_addr[31:9]),
       .sd_index(sd_index),
       .sd_go(sd_controller_go),
       .sd_busy(sd_busy));

   sd_cache sd_cache_inst
      (.clk(clk),
       .sd_go(sd_go),
       .sd_addr(sd_addr),
       .to_load(sd_controller_go),
       .sd_busy(sd_busy),
       .ready(sd_ready));

endmodule
