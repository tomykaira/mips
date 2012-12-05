// Top module

module mimic(input clk,
             input         reset,

             output        mem_write_enable,
             output [31:0] mem_addr,
             output [31:0] mem_write_data,
             input [31:0]  mem_read_data,

             output        tx_send_enable,
             output [7:0]  tx_send_data,

             input [7:0]   rx_received_data,
             input         rx_waiting,
             output        rx_fifo_pop,

             output        display_buffer_write_enable,
             output [11:0] display_position,
             output [6:0]  display_char_code,

             input [7:0]   key_status,
             input [7:0]   keycode);

   wire [15:0] inst_address, inst_write_address;
   wire [31:0] inst_write_data, inst_fetch;
   wire        inst_write_enable;
   instruction_memory instruction_memory_inst(.clk(clk),
                                              .write_enable(inst_write_enable),
                                              .read_address(inst_address),
                                              .write_address(inst_write_address),
                                              .write_data(inst_write_data),
                                              .read_data(inst_fetch));


   wire [31:0] pc;
   assign inst_address = pc[15:0];

   ////////////////////////////////////////////////////////////////
   // decode
   wire [31:0] raw_inst_decode, inst_decode;
   wire [4:0]  rs_addr, rt_addr;
   wire        rs_float, rt_float;
   wire [15:0] raw_imm;
   wire        keep_pc;
   wire        freeze;

   decoder decoder_inst(.clk(clk),
                        .reset(reset),
                        .inst(inst_fetch),
                        .freeze(freeze),
                        .inst_out(raw_inst_decode),
                        .rs_addr(rs_addr),
                        .rt_addr(rt_addr),
                        .rs_float(rs_float),
                        .rt_float(rt_float),
                        .raw_imm(raw_imm),
                        .keep_pc(keep_pc));

   flip_reset inst_decode_ff(.clk(clk), .reset(reset), .d(raw_inst_decode), .q(inst_decode));

   waiting_signal_watcher watcher_inst
     (.inst(inst_fetch),
      .rx_wait(rx_waiting),
      .key_status(key_status),
      .freeze(freeze));

   wire [31:0] rs_data, rt_data;
   wire branch_taken;
   program_counter pc_inst
     (.clk(clk), .reset(reset), .inst(raw_inst_decode), .rs(rs_data), .keep_pc(keep_pc), .branch_taken(branch_taken),
      .pc(pc));


   ////////////////////////////////////////////////////////////////
   // Register read / Write back
   wire        write_enable_misc, write_enable_alu, write_enable_mem, write_enable_fpu;
   wire [4:0]  write_addr_misc, write_addr_alu, write_addr_mem, write_addr_fpu;
   wire [31:0] write_data_misc, write_data_alu, write_data_mem, write_data_fpu;
   wire        write_float_misc, write_float_alu, write_float_mem, write_float_fpu;
   register_manager register_manager_inst(.clk(clk),
                                          .reset(reset),
                                          .rs_addr(rs_addr),
                                          .rt_addr(rt_addr),
                                          .rs_float(rs_float),
                                          .rt_float(rt_float),
                                          .rs_data(rs_data),
                                          .rt_data(rt_data),

                                          .write_enable_misc(write_enable_misc),
                                          .write_enable_alu(write_enable_alu),
                                          .write_enable_mem(write_enable_mem),
                                          .write_enable_fpu(write_enable_fpu),
                                          .write_addr_misc(write_addr_misc),
                                          .write_addr_alu(write_addr_alu),
                                          .write_addr_mem(write_addr_mem),
                                          .write_addr_fpu(write_addr_fpu),
                                          .write_data_misc(write_data_misc),
                                          .write_data_alu(write_data_alu),
                                          .write_data_mem(write_data_mem),
                                          .write_data_fpu(write_data_fpu),
                                          .write_float_misc(write_float_misc),
                                          .write_float_alu(write_float_alu),
                                          .write_float_mem(write_float_mem),
                                          .write_float_fpu(write_float_fpu));

   wire [31:0] extended_imm;
   sign_extension sign_ext_inst(.clk(clk), .imm_in(raw_imm), .imm_out(extended_imm));

   wire [31:0] inst_reg_read;
   flip_reset #(.width(32)) inst_reg_ff(.clk(clk), .reset(reset), .D(inst_decode), .Q(inst_reg_read));


   ////////////////////////////////////////////////////////////////
   // Execution
   alu_wrapper alu_inst
     (.clk(clk), .reset(reset),
      .inst(inst_reg_read), .rs(rs_data), .rt(rt_data), .imm(extended_imm),
      .enable(write_enable_alu), .addr(write_addr_alu), .data(write_data_alu), .float(write_float_alu));

   fpu_controller fpu_inst
      (.clk(clk),
       .inst(inst_reg_read), .rs(rs_data), .rt(rt_data),
       .enable(write_enable_fpu), .addr(write_addr_fpu), .data(write_data_fpu), .float(write_float_fpu));

   sram_manager sram_inst
     (.clk(clk), .inst(inst_reg_read), .rs(rs_data), .rt(rt_data), .imm(extended_imm),
      .memory_read(mem_read_data), .memory_write(mem_write_data),
      .memory_address(mem_addr), .memory_write_enable(mem_write_enable),
      .enable(write_enable_mem), .addr(write_addr_mem), .data(write_data_mem), .float(write_float_mem));

   // FIXME: this will get rotten...
   wire write_enable_rs, write_float_rs;
   wire [4:0] write_addr_rs;
   wire [31:0] write_data_rs;
   rs232c rs232c_inst
     (.clk(clk), .inst(inst_reg_read), .rt(rt_data),
      .push_send_data(tx_send_enable), .send_data(tx_send_data),
      .rx_wait(rx_waiting), .received_data(rx_received_data), .rx_pop(rx_fifo_pop),
      .enable(write_enable_rs), .addr(write_addr_rs), .data(write_data_rs), .float(write_float_rs));

   wire write_enable_keyboard, write_float_keyboard;
   wire [4:0] write_addr_keyboard;
   wire [31:0] write_data_keyboard;
   keyboard_reader keyboard_inst
      (.clk(clk), .inst(inst_reg_read),
      .key_status(key_status), .keycode(keycode),
      .enable(write_enable_keyboard), .addr(write_addr_keyboard), .data(write_data_keyboard), .float(write_float_keyboard));

   assign write_enable_misc = write_enable_rs == 1'b1 ? write_enable_rs : write_enable_keyboard;
   assign write_addr_misc   = write_enable_rs == 1'b1 ? write_addr_rs   : write_addr_keyboard;
   assign write_data_misc   = write_enable_rs == 1'b1 ? write_data_rs   : write_data_keyboard;
   assign write_float_misc  = write_enable_rs == 1'b1 ? write_float_rs  : write_float_keyboard;

   display_instruction_dispatcher display_inst
     (.clk(clk), .inst(inst_reg_read), .rs(rs_data), .rt(rt_data),
      .buffer_write_enable(display_buffer_write_enable), .position(display_position), .char_code(display_char_code));

   instruction_writer inst_writer_inst
     (.clk(clk), .inst(inst_reg_read), .rs(rs_data), .rt(rt_data),
      .write_enable(inst_write_enable), .address(inst_write_address), .write_data(inst_write_data));

   branch_condition_checker branch_condition_checker_inst
      (.op(inst_reg_read[31:26]), .rs(rs_data), .rt(rt_data), .go_branch(branch_taken));

endmodule
