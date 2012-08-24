module testbench_rs232c_buffer();

   reg clk, reset, push;
   reg [31:0] push_data;
   wire tx;

   rs232c_buffer #(16'd4) dut (.clk(clk), .reset(reset), .push(push), .push_data(push_data), .tx(tx));

   initial begin
      reset <= 1;
      push  <= 0;
      #20;
      reset <= 0;
      #50;
      // HERE should be enough time
      // As for simulation, at least 50 ns

      push_data <= 32'h12345678;
      push <= 1;
      #10;
      push <= 0;
      #200;

      push_data <= 32'h11112222;
      push <= 1;
      #10;
      push <= 0;
      #10;
      push_data <= 32'hff00ff00;
      push <= 1;
      #10;
      push <= 0;
      #10;
      push_data <= 32'h7800ad16;
      push <= 1;
      #10;
      push_data <= 32'heee80a0e;
      push <= 1;
      #10;
      push_data <= 32'h68971e60;
      push <= 1;
      #10;
      push_data <= 32'h4ba73e0d;
      push <= 1;
      #10;
      push_data <= 32'h8cf3eb97;
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
