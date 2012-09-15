module testbench_rs232c_buffer();

   parameter wait_time = 16'd20;

   reg clk, reset, push;
   reg [7:0] push_data;
   wire tx;
   wire [7:0] decoded;
   wire changed;

   rs232c_buffer #(wait_time) dut (.clk(clk), .reset(reset), .push(push), .push_data(push_data), .tx(tx));
   i232c #(wait_time) decoder (.clk(clk), .enable(1'b1), .rx(tx), .data(decoded), .changed(changed));

   initial begin
      reset <= 1;
      push  <= 0;
      #20;
      reset <= 0;
      #50;
      // HERE should be enough time
      // As for simulation, at least 50 ns

      push_data <= 8'h00;
      push <= 1;
      #10;
      push <= 0;
      #200;

      push_data <= 8'hff;
      push <= 1;
      #10;
      push <= 0;
      #10;
      push_data <= 8'hF0;
      push <= 1;
      #10;
      push <= 0;
      #10;
      push_data <= 8'h0F;
      push <= 1;
      #10;
      push_data <= 8'hcc;
      push <= 1;
      #10;
      push_data <= 8'h33;
      push <= 1;
      #10;
      push_data <= 8'haa;
      push <= 1;
      #10;
      push_data <= 8'h66;
      push <= 1;
      #10;
      push <= 0;
   end

   // geenrate clock to sequence tests
   always begin
      clk <= 1;
      #5;
      clk <= 0;
      #5;
   end

endmodule
