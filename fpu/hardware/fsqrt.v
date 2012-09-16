module fsqrt (input clk,
             input [31:0]  a, // assumed to be positive
             output reg [31:0] s);

   wire [35:0] value;
   wire [23:0] const_part;
   reg  [23:0] const_part2;
   wire [12:0] inc_part;
   reg  [9:0] key;
   reg [14:0] a1;

   fsqrt_table fsqrt_t (.clk(clk), .key(key), .value(value));

   reg [7:0] exponent1, exponent2;
   reg [27:0] lower; // 15 x 13 bit
   reg [23:0] sum;

   assign const_part = {value[35:13],1'b0}; // << 1
   assign inc_part   = value[12:0];

   always @ (posedge clk) begin

      // stage 1
      exponent1 <= a[30:23]+1;
      key       <= a[23:14]; // LSB of exponent and 9 MSBs of mantissa
      a1        <= a[23] ? a[14:0] : {a[13:0],1'b0};

      // stage 2
      exponent2    <= 63 + exponent1[7:1];
      lower        <= a1 * inc_part;
      const_part2  <= const_part;

       // stage 3
      sum = const_part2 + lower[27:14]; // >> 14
      s <= {0, exponent2, sum[22:0]};
   end

endmodule
  
