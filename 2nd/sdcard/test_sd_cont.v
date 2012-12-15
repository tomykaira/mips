module test_sd_cont();

   reg clk;

   reg sd_in;
   wire sd_clk, sd_ce, sd_out;

   reg [8:0] sd_index;
   wire [7:0] sd_read_data;
   reg [7:0] sd_write_data;
   reg sd_write_enable;

   reg sd_write;

   wire sd_busy;

   sd_cont dut
     (.clk(clk),
      .sd_clk(sd_clk),
      .sd_ce(sd_ce),
      .sd_out(sd_out),
      .sd_in(sd_in),

      .sd_index(sd_index),
      .sd_read_data(sd_read_data),
      .sd_write_data(sd_write_data),
      .sd_write_enable(sd_write_enable),

      .sd_addr(23'b0),
      .sd_write(sd_write),
      .sd_busy(sd_busy));

   initial begin
      sd_in <= 1'b0;
      #28;

      sd_index <= 9'h3;
      sd_write_data <= 8'haa;
      sd_write_enable <= 1;
      #10;

      sd_index <= 9'h1;
      sd_write_data <= 8'hcc;
      sd_write_enable <= 1;
      #10;

      sd_write <= 1;
      #10;

      sd_in <= 1'b0;

      #600000;
      sd_in <= 1'b1;
   end

   // geenrate clock to sequence tests
   always begin
      clk <= 1;
      #5;
      clk <= 0;
      #5;
   end

endmodule
