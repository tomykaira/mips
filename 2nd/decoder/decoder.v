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

   parameter ADD     = 6'b000000;
   parameter SUB     = 6'b000001;
   parameter XOR     = 6'b000010;
   parameter ADDI    = 6'b000011;
   parameter SUBI    = 6'b000100;
   parameter XORI    = 6'b000101;
   parameter SLLI    = 6'b000110;
   parameter SRAI    = 6'b000111;

   parameter FMVLO   = 6'b010010;
   parameter FMVHI   = 6'b010011;
   parameter IMOVF   = 6'b010110;
   parameter FMOVI   = 6'b010111;

   parameter FADD    = 6'b110000;
   parameter FSUB    = 6'b110001;
   parameter FMUL    = 6'b110010;
   parameter FMULN   = 6'b110011;
   parameter FINV    = 6'b110100;
   parameter FSQRT   = 6'b110101;

   parameter LDI     = 6'b101000;
   parameter LDR     = 6'b101100;
   parameter STI     = 6'b101001;
   parameter FLDI    = 6'b101010;
   parameter FSTI    = 6'b101011;
   parameter FLDR    = 6'b101110;

   parameter BEQ     = 6'b100000;
   parameter BLT     = 6'b100001;
   parameter BLE     = 6'b100010;
   parameter FBEQ    = 6'b100100;
   parameter FBLT    = 6'b100101;
   parameter FBLE    = 6'b100110;

   parameter J       = 6'b111000;
   parameter JR      = 6'b111001;
   parameter CALL    = 6'b111010;
   parameter CALLR   = 6'b111011;
   parameter RETURN  = 6'b111100;
   parameter INPUTB  = 6'b111101;
   parameter OUTPUTB = 6'b111110;
   parameter HALT    = 6'b111111;

   wire [5:0] op;
   assign op = inst[31:26];


   wire [4:0] i_rs_addr, i_rt_addr;
   assign i_rs_addr = inst[25:21];
   assign i_rt_addr = inst[20:16];
   flip_reset #(.width(5)) rs_addr_ff (.clk(clk), .reset(reset), .d(i_rs_addr), .q(rs_addr));
   flip_reset #(.width(5)) rt_addr_ff (.clk(clk), .reset(reset), .d(i_rt_addr), .q(rt_addr));


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
      if (op == 6'b010011 || op == 6'b111000 || op == 6'b111010 || op == 6'b111100
          || op == 6'b111101 || op == 6'b111110 || op == 6'b111111)
        use_rs <= 1'b0;
      else
        use_rs <= 1'b1;

      if (op == ADDI
          || op == SUBI
          || op == XORI
          || op == SLLI
          || op == SRAI

          || op == FMVLO // misc
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
          || op == HALT)
        use_rt <= 1'b0;
      else
        use_rt <= 1'b1;
   end


   reg [6:0] fpu_history[2:0]; // use + float flag + address(5)
   reg [6:0] mem_history[1:0]; // use + float flag + address(5)

   always @ (*) begin
      if (op[5:3] == 3'b110 && stall == 0)
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
