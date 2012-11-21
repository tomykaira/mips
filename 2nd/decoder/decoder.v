// decode instruction
// if stall is needed, stall signal is high
// TODO: define auto-generated inst code

module decoder(input clk,
                input             reset,
                input [31:0]      inst,
                input             rx_wait,
                output [31:0]     inst_out,
                output [4:0]      rs_addr, rt_addr,
                output            rs_float, rt_float,
                output reg [15:0] raw_imm,
                output            keep_pc);

   parameter LDI  = 6'b101000;
   parameter STI  = 6'b101001;
   parameter LDR  = 6'b101100;
   parameter FLDI = 6'b101010;
   parameter FSTI = 6'b101011;
   parameter FLDR = 6'b101110;

   parameter JR     = 6'b111001;
   parameter CALLR  = 6'b111011;
   parameter INPUTB = 6'b111101;

   parameter HALT   = 6'b111111;

   wire [5:0] op;
   assign op = inst[31:26];


   wire [4:0] i_rs_addr, i_rt_addr;
   assign i_rs_addr = inst[25:21];
   assign i_rt_addr = inst[20:16];
   flip_reset #(5) rs_addr_ff (.clk(clk), .reset(reset), .d(i_rs_addr), .q(rs_addr));
   flip_reset #(5) rt_addr_ff (.clk(clk), .reset(reset), .d(i_rt_addr), .q(rt_addr));


   reg i_rs_float, i_rt_float;

   always @ (op) begin
      if (op == 6'b010010
          || op == 6'b010010
          || op == 6'b010011
          || op == 6'b010111
          || op[5:3] == 3'b110 // fpu
          || op[5:2] == 4'b1001 // branch
          ) begin
         i_rs_float <= 1'b1;
         i_rt_float <= 1'b1;
      end else if (op == 6'b101011) begin // fsti
         i_rs_float <= 1'b1;
         i_rt_float <= 1'b0;
      end else begin
         i_rs_float <= 1'b0;
         i_rt_float <= 1'b0;
      end
   end

   FD rs_float_ff (.C(clk), .D(i_rs_float), .Q(rs_float));
   FD rt_float_ff (.C(clk), .D(i_rt_float), .Q(rt_float));


   // stall reg is defined afterword
   reg stall;
   wire [31:0] inst_or_nop;
   assign inst_or_nop = ((stall == 1
                          || (op == JR && inst_out[31:26] == JR)
                          || (op == CALLR && inst_out[31:26] == CALLR))
                         ? 32'b0 : inst);
   flip_reset #(32) inst_ff (.clk(clk), .reset(reset), .d(inst_or_nop), .q(inst_out));


   wire keep_inst;
   assign keep_inst = (op == INPUTB || op == HALT ? 1'b1 : 1'b0);
   assign keep_pc   = (stall == 1 || keep_inst == 1) ? 1'b1 : 1'b0;


   always @ (posedge(clk)) begin
      raw_imm <= inst[15:0];
   end


   ///////////////////////////////////////////////
   // stall

   reg use_rs, use_rt;
   always @ (op) begin
      if (op == 6'b010011 || op == 6'b111000 || op == 6'b111010 || op == 6'b111100
          || op == 6'b111101 || op == 6'b111110 || op == 6'b111111)
        use_rs <= 1'b0;
      else
        use_rs <= 1'b1;

      if (op == 6'b000011
          || op == 6'b000100
          || op == 6'b000101
          || op == 6'b000110
          || op == 6'b000111

          || op == 6'b010010 // misc
          || op == 6'b010011
          || op == 6'b010110
          || op == 6'b010111

          || op == 6'b101000 // load
          || op == 6'b101100
          || op == 6'b101010
          || op == 6'b101110

          || op == 6'b111000 // jump, call
          || op == 6'b111001
          || op == 6'b111010
          || op == 6'b111011
          || op == 6'b111100
          || op == 6'b111101
          || op == 6'b111111)
        use_rt <= 1'b0;
      else
        use_rt <= 1'b1;
   end


   reg [6:0] fpu_history[2:0]; // use + float flag + address(5)
   reg [6:0] mem_history[1:0]; // use + float flag + address(5)

   always @ (*) begin
      if (op[5:3] == 3'b110)
         fpu_history[0] = {2'b11,inst[15:11]};
      else
         fpu_history[0] = 7'b0;

      case (op)
        LDI:  mem_history[0] = {2'b10,inst[20:16]};
        LDR:  mem_history[0] = {2'b10,inst[15:11]};
        FLDI: mem_history[0] = {2'b11,inst[20:16]};
        FLDR: mem_history[0] = {2'b11,inst[15:11]};
        default: mem_history[0] = 7'b0;
      endcase
   end

   always @ (posedge(clk)) begin
      fpu_history[1] <= fpu_history[0];
      fpu_history[2] <= fpu_history[1];

      mem_history[1] <= mem_history[0];
   end

   reg [6:0] rs_code, rt_code;
   always @ (*) begin
      rs_code = {1'b1, i_rs_float, i_rs_addr};
      rt_code = {1'b1, i_rt_float, i_rt_addr};
      if (use_rs == 1
          && (fpu_history[1] == rs_code
              || fpu_history[2] == rs_code
              || mem_history[1] == rs_code))
         stall <= 1'b1;
      else if (use_rt == 1
               && (fpu_history[1] == rt_code
                   || fpu_history[2] == rt_code
                   || mem_history[1] == rt_code))
         stall <= 1'b1;
      else
         stall <= 1'b0;

   end

endmodule
