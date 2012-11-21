// FPU specification
// units takes basically 3 clk
// a, b should flow through.
// Signal should in units' register at the end of decode phase.
// doctest cannot test this, because it uses the clock.

// 110000 # fadd
// 110001 # fsub
// 110010 # fmul
// 110011 # fmuln
// 110100 # finv
// 110101 # fsqrt

module fpu_controller (input clk,
                       input [31:0]      inst,
                       input [31:0]      rs,
                       input [31:0]      rt,
                       output            enable,
                       output [4:0]      addr,
                       output reg [31:0] data,
                       output            float);

   reg [31:0] b;
   reg current_enable;
   wire [4:0] current_addr;
   wire [31:0] neg_b;
   wire [31:0] fadd_data, fmul_data, finv_data, fsqrt_data;
   wire [5:0] op;
   reg [2:0] write_enable;
   reg [4:0] write_addr[2:0];
   reg [1:0] source[2:0]; // fadd, fmul, finv, fsqrt
   reg [1:0] current_source;

   wire [31:0] rs_or_0;
   assign rs_or_0 = current_enable == 0 ? 32'b0 : rs;
   fadd fadd_inst (.clk(clk),.i1(rs_or_0),.i2(b),.o(fadd_data));
   fmul fmul_inst (.clk(clk),.a(rs_or_0),.b(b),.s(fmul_data));
   finv finv_inst (.clk(clk),.a(rs_or_0),.s(finv_data));
   fsqrt fsqrt_inst (.clk(clk),.a(rs_or_0),.s(fsqrt_data));
   fneg b_inverter (.a(rt),.s(neg_b));

   assign op = inst[31:26];
   assign current_addr = inst[15:11];

   assign enable = write_enable[0];
   assign addr   = write_addr[0];
   assign float  = 1'b1;

   always @ (*) begin
      if (op >= 6'b110000 && op <= 6'b110101)
        current_enable <= 1;
      else
        current_enable <= 0;
   end

   always @ (*) begin
      if (current_enable == 1)
        if (op[0] == 1) // b is not used for fsqrt
          b <= neg_b;
        else
          b <= rt;
      else
         b <= 32'b0;
   end

   always @ (*) begin
      case (op)
        6'b110000: current_source <= 0;
        6'b110001: current_source <= 0;
        6'b110010: current_source <= 1;
        6'b110011: current_source <= 1;
        6'b110100: current_source <= 2;
        6'b110101: current_source <= 3;
        default: current_source <= 0;
      endcase
   end

   always @ (source[0], source[1], source[2], fadd_data, fmul_data, finv_data, fsqrt_data) begin

      case (source[0])
        0: data <= fadd_data;
        1: data <= fmul_data;
        2: data <= finv_data;
        3: data <= fsqrt_data;
      endcase
   end

   always @ (posedge(clk)) begin
      write_enable[0] <= write_enable[1];
      write_enable[1] <= write_enable[2];
      write_enable[2] <= current_enable;

      write_addr[0] <= write_addr[1];
      write_addr[1] <= write_addr[2];
      write_addr[2] <= current_addr;

      source[0] <= source[1];
      source[1] <= source[2];
      source[2] <= current_source;
   end

endmodule
