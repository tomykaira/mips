// finv implementation
// Apply Newton method to the value from table.
// Table is created with ../software/finv_ikuta.cc.
// Current error is at most 6 ulp, which is too large.
// 140 MHz

module finv (input clk,
             input [31:0]  a,
             output reg [31:0] s);

   wire [35:0] value;
   wire [22:0] const_part;
   wire [12:0] inc_part;
   reg  [9:0] key;
   reg [12:0] a1, a12;

   finv_table finv_t (.clk(clk), .key(key), .value(value));

   reg [8:0] exponent1, exponent2, exponent2inv;
   reg sign1, sign2, iszero2;
   wire [25:0] lower;
   reg [23:0] sum;

   assign const_part = value[35:13];
   assign inc_part   = value[12:0];
   assign lower      = a12 * inc_part;

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
      iszero2      <= a1 == 0 && key == 0;
      a12          <= a1;

       // stage 3
      sum = const_part - lower[25:13];
      s <= {sign2, exponent2 == 0 ? exponent2 : (iszero2 ? exponent2inv + 1 : exponent2inv),
            iszero2 ? 23'b0 : {sum[21:0],1'b0}};
   end

endmodule
