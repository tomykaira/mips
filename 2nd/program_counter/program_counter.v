// top module of program counter
// this gets instruction and other status, puts pc.

module program_counter(input clk,
                       input         reset,
                       input [31:0]  inst,
                       input [31:0]  rs,
                       input         keep_pc,
                       input         branch_taken,
                       output [31:0] pc);

   wire [1:0] current_kind;
   wire [31:0] pc_clk;

   wire push_stack, pop_stack;
   wire [31:0] stack_top;

   wire current_is_branch, current_is_jump_reg;
   reg [1:0] is_jump_reg;
   reg [2:0] is_branch;
   reg [31:0] branch_addr[2:0];

   wire [31:0] decoded_addr;

   program_counter_decoder decoder_inst
     (.inst(inst), .current_pc(pc_clk),
      .kind(current_kind),
      .is_jump_reg(current_is_jump_reg),
      .is_branch(current_is_branch),
      .address(decoded_addr),
      .push_stack(push_stack));

   // current_kind is 0(current) for jr and callr, but it is not important.
   // keep for register call is decided by decoder.
   wire [1:0] current_kind_including_decoded;
   assign current_kind_including_decoded = keep_pc == 1 ? 2'b00 : current_kind;

   wire [31:0] prev_prev_address;
   assign prev_prev_address = branch_addr[2];
   program_counter_calculator calculator_inst
     (.current_pc(pc_clk),
      .current_kind(current_kind_including_decoded),
      .decoded_addr(decoded_addr),
      .prev_is_jump_reg(is_jump_reg[1]),
      .prev_register(rs),
      .prev_prev_is_branch(is_branch[2]),
      .prev_prev_taken(branch_taken),
      .prev_prev_address(prev_prev_address),
      .stack_top(stack_top),
      .next_pc(pc),
      .pop_stack(pop_stack));

   call_stack stack_inst (.clk(clk), .do_push(push_stack), .do_pop(pop_stack), .current_pc(pc_clk+1),
                          .stack_top(stack_top));

   flip_reset #(.width(32)) pc_ff (.clk(clk), .reset(reset), .d(pc), .q(pc_clk));

   always @ (posedge(clk)) begin
      is_jump_reg[1] <= is_jump_reg[0];
      is_jump_reg[0] <= current_is_jump_reg;

      is_branch[1] <= current_is_branch;
      is_branch[2] <= is_branch[1];
      branch_addr[1] <= decoded_addr;
      branch_addr[2] <= branch_addr[1];
   end
endmodule
