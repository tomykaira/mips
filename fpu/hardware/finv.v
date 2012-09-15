module finv (input clk,
             input [31:0]  a,
             output reg [31:0] s);

   wire [35:0] value;
   wire [22:0] const_part;
   reg  [22:0] const_part2;
   wire [12:0] inc_part;
   reg  [9:0] key;

   finv_table finv_t (.clk(clk), .key(key), .value(value));

   reg [8:0] exponent1, exponent2, exponent2inv;
   reg sign1, sign2;
   reg [25:0] lower;
   wire [23:0] sum;

   assign const_part = value[35:13];
   assign inc_part   = value[12:0];

   always @ (posedge clk) begin

      // stage 1
      sign1     <= a[31];
      exponent1 <= a[31:23];
      key       <= a[22:13];
      a1        <= a[12:0];

      // stage 2
      sign2        <= sign1;
      exponent2    <= exponent1;
      exponent2inv <= 253 - exponent1;
      lower        <= a1 * inc_part;
      const_part2  <= const_part;

       // stage 3
      sum := const_part2 + lower[25:13];
      s <= {sign2, exponent2 == 0 ? exponent2 : exponent2inv, sum[23:1]};
   end

endmodule
  
