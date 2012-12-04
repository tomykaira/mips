// decode instruction
// if stall is needed, stall signal is high
// TODO: define auto-generated inst code

module decoder(input clk,
                input             reset,
                input [31:0]      inst,
                input             freeze,
                output [31:0]     inst_out,
                output [4:0]      rs_addr, rt_addr,
                output            rs_float, rt_float,
                output reg [15:0] raw_imm,
                output            keep_pc);

   `include "../opcode.h"

   wire [5:0] op;
   assign op = inst[31:26];


   wire [4:0] i_rs_addr, i_rt_addr;
   assign i_rs_addr = inst[25:21];
   assign i_rt_addr = inst[20:16];
   flip_reset #(.width(5)) rs_addr_ff (.clk(clk), .reset(reset), .d(i_rs_addr), .q(rs_addr));
   flip_reset #(.width(5)) rt_addr_ff (.clk(clk), .reset(reset), .d(i_rt_addr), .q(rt_addr));


   reg i_rs_float, i_rt_float;

   always @ (op) begin
      if (op == FMVLO || op == FMVHI || op == FMOVI
          || op == FBEQ || op == FBLT || op == FBLE
          || op[5:3] == FPU_GROUP // fpu
          ) begin
         i_rs_float <= 1'b1;
         i_rt_float <= 1'b1;
      end else if (op == FSTI) begin // fsti
         i_rs_float <= 1'b0;
         i_rt_float <= 1'b1;
      end else begin
         i_rs_float <= 1'b0;
         i_rt_float <= 1'b0;
      end
   end

   FD rs_float_ff (.C(clk), .D(i_rs_float), .Q(rs_float));
   FD rt_float_ff (.C(clk), .D(i_rt_float), .Q(rt_float));


   reg stall;
   wire [5:0] prev_op;
   assign inst_out = ((stall == 1
                       || (op == JR && prev_op == JR)
                       || (op == CALLR && prev_op == CALLR))
                      ? 32'b0 : inst);

   // currently active op
   // if once callR or JR is sent, it is active until next inst is loaded
   wire [5:0] active_op;
   assign active_op = stall == 1 ? 6'b0 : inst[31:26];
   flip_reset #(.width(6)) active_op_ff(.clk(clk), .reset(reset), .D(active_op), .Q(prev_op));


   wire keep_inst;
   assign keep_inst = (freeze == 1'b1 || op == HALT || op == CALLR || op == JR ? 1'b1 : 1'b0);
   assign keep_pc   = (stall == 1 || keep_inst == 1) ? 1'b1 : 1'b0;


   always @ (posedge(clk)) begin
      raw_imm <= inst[15:0];
   end


   ///////////////////////////////////////////////
   // stall

   reg use_rs, use_rt;
   always @ (op) begin
      if (op == SETL || op == FMVHI || op == J || op == CALL || op == RETURN
          || op == INPUTB || op == OUTPUTB || op == HALT
          || op == READKEY)
        use_rs <= 1'b0;
      else
        use_rs <= 1'b1;

      if (op == ADDI
          || op == SUBI
          || op == XORI
          || op == SLLI
          || op == SRAI

          || op == SETL  // misc
          || op == FMVLO
          || op == FMVHI
          || op == IMOVF
          || op == FMOVI

          || op == LDI // load
          || op == FLDI

          || op == J // jump, call
          || op == JR
          || op == CALL
          || op == CALLR
          || op == RETURN
          || op == INPUTB
          || op == HALT

          || op == READKEY)
        use_rt <= 1'b0;
      else
        use_rt <= 1'b1;
   end


   reg [6:0] fpu_history[2:0]; // use + float flag + address(5)
   reg [6:0] mem_history[1:0]; // use + float flag + address(5)

   always @ (*) begin
      if (op[5:3] == FPU_GROUP && stall == 0)
         fpu_history[0] = {2'b11,inst[15:11]};
      else
         fpu_history[0] = 7'b0;

      if (stall == 0)
        case (op)
          LDI:  mem_history[0] = {2'b10,inst[20:16]};
          LDR:  mem_history[0] = {2'b10,inst[15:11]};
          FLDI: mem_history[0] = {2'b11,inst[20:16]};
          FLDR: mem_history[0] = {2'b11,inst[15:11]};
          default: mem_history[0] = 7'b0;
        endcase
      else
        mem_history[0] = 7'b0;
   end

   always @ (posedge(clk)) begin
      fpu_history[1] <= fpu_history[0];
      fpu_history[2] <= fpu_history[1];

      mem_history[1] <= mem_history[0];
   end

   wire [6:0] rs_code, rt_code;
   assign rs_code = {use_rs, i_rs_float, i_rs_addr};
   assign rt_code = {use_rt, i_rt_float, i_rt_addr};
   always @ (fpu_history[0], fpu_history[1], fpu_history[2], mem_history[0], mem_history[1], rs_code, rt_code, use_rs, use_rt) begin
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
