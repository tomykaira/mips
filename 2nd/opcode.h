// -*- mode: verilog -*-
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

parameter FPU_GROUP = 3'b110;
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

parameter DISPLAY = 6'b001000;
parameter READKEY = 6'b001001;
