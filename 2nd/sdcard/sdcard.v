module sdcard(input clk,

              output       sd_clk,
              output       sd_ce,
              output       sd_out,
              input        sd_in,

              output [7:0] sd_read_data,
              input [7:0]  sd_write_data,
              input [31:0] sd_addr,
              input        sd_read,
              input        sd_write,
              output       sd_ready,

              output [7:0] debug);

   wire sd_busy;
   wire sd_controller_go;

   wire write_ram;
   wire [22:0] sd_block;

   wire sd_read_spi, sd_write_spi;

   assign debug = {4'b0, sd_go, sd_busy, sd_controller_go, sd_ready};

   sd_cont sd_cont_inst
      (.clk(clk),
       .sd_clk(sd_clk),
       .sd_ce(sd_ce),
       .sd_out(sd_out),
       .sd_in(sd_in),

       .sd_index(sd_addr[8:0]),
       .sd_read_data(sd_read_data),
       .sd_write_data(sd_write_data),
       .sd_write_enable(write_ram),

       .sd_addr(sd_block),
       .sd_read(sd_read_spi),
       .sd_write(sd_write_spi),
       .sd_busy(sd_busy));

   sd_cache sd_cache_inst
      (.clk(clk),

       .read(sd_read),
       .write(sd_write),
       .busy(sd_busy),

       .read_spi(sd_read_spi),
       .write_spi(sd_write_spi),
       .write_ram(write_ram),

       .addr(sd_block),
       .block(sd_block),

       .ready(sd_ready));

endmodule
