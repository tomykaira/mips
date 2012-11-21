// SRAM manager

// 101000 ldi
// 101001 sti
// 100100 ldr
// 101010 fldi
// 101011 fsti
// 101110 fldr

module sram_manager (input clk,
                     input [31:0]      inst,
                     input [31:0]      rs,
                     input [31:0]      rt,
                     input [31:0]      imm,

                     input [31:0]      memory_read,
                     output [31:0]     memory_write,
                     output reg [31:0] memory_address,
                     output            memory_write_enable,

                     output            enable,
                     output [4:0]      addr,
                     output [31:0]     data,
                     output            float);

   parameter LDI  = 6'b101000;
   parameter STI  = 6'b101001;
   parameter LDR  = 6'b101100;
   parameter FLDI = 6'b101010;
   parameter FSTI = 6'b101011;
   parameter FLDR = 6'b101110;

   wire [5:0] op;
   reg [4:0] current_addr;
   reg current_enable;
   wire current_float;
   reg [1:0] write_enable;
   reg [1:0] write_float;
   reg [4:0] write_addr[1:0];

   assign op = inst[31:26];

   // memory config
   assign memory_write_enable = (op == STI || op == FSTI) ? 1'b1 : 1'b0;
   assign memory_write = rt;

   always @ (*) begin
      if (op[2] == 1)
        memory_address <= rs + rt;
      else
        memory_address <= rs + imm;
   end


   // write-back
   assign enable = write_enable[0];
   assign addr   = write_addr[0];
   assign float  = write_float[0];
   assign data   = memory_read;

   // current status
   assign current_float = op[1];

   // current addr and current enable
   always @ (*) begin
      current_enable <= 1;
      case (op)
        LDI: current_addr <= inst[20:16];
        LDR: current_addr <= inst[15:11];
        FLDI: current_addr <= inst[20:16];
        FLDR: current_addr <= inst[15:11];
        default: begin
           current_addr <= 0;
           current_enable <= 0;
        end
      endcase
   end

   always @ (posedge(clk)) begin
      write_enable[0] <= write_enable[1];
      write_enable[1] <= current_enable;

      write_addr[0] <= write_addr[1];
      write_addr[1] <= current_addr;

      write_float[0] <= write_float[1];
      write_float[1] <= current_float;
   end

endmodule
