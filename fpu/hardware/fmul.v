module fmul (input clk,
             input [31:0]  a,
             input [31:0]  b,
             output reg [31:0] s);

   wire [12:0] ah, bh;
   wire [10:0] al, bl;
   wire [7:0] ae, be;
   reg [26:0] hh, hl, lh, m;
   reg [8:0] exponent, exponent2, exponent21;
   reg sign1, sign2;
   reg is_zero1, is_zero2;

   assign ae = a[30:23];
   assign be = b[30:23];

   assign ah = {1'b1,a[22:11]};
   assign bh = {1'b1,b[22:11]};
   assign al = a[10:0];
   assign bl = b[10:0];

   always @ (posedge clk) begin

      // stage 1
      hh <= ah * bh;
      hl <= ah * bl;
      lh <= al * bh;
      exponent <= (ae == 0 || be == 0) ? 0 : ae + be - 127;
      is_zero1 <= (a[30:0] == 0 || b[30:0] == 0) ? 1 : 0;
      sign1 <= a[31] ^ b[31];

      // stage2
      is_zero2 <= is_zero1;
      sign2 <= sign1;
      m <= hh + hl[26:11] + lh[26:11] + 2;
      exponent2 <= exponent;
      exponent21 <= exponent + 1;

      // stage3
      if (is_zero2 || exponent2[8] == 1)
        s <= {sign2, 31'b0};
      else
        s <= {sign2, m[25] ? exponent21[7:0] : exponent2[7:0], m[25] ? m[24:2] : m[23:1]};
   end

endmodule
  
