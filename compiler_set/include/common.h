#ifndef _COMMON_H
#define _COMMON_H

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/types.h>
#include <vector>
#include <map>
#define rep(i, n) for (int i = 0; i < (int)n; i++)
#define repi(i, n) for (int i = 1; i < (int)n; i++)
#define eq(a, b) (strcmp(a, b) == 0)

#define ROM_NUM (64 * 1024) // 64KByte
#define RAM_NUM (512.00)

#define MAX_INSTS 64 // 6bit

#define INTREG_NUM (32)
#define FLOATREG_NUM (32)


#define ADD     (0b000000)
#define SUB     (0b000001)
#define XOR     (0b000010)
#define ADDI    (0b000011)
#define SUBI    (0b000100)
#define XORI    (0b000101)
#define SLLI    (0b000110)
#define SRAI    (0b000111)

#define FMVLO   (0b010010)
#define FMVHI   (0b010011)
#define IMOVF   (0b010110)
#define FMOVI   (0b010111)

#define FADD    (0b110000)
#define FSUB    (0b110001)
#define FMUL    (0b110010)
#define FMULN   (0b110011)
#define FINV    (0b110100)
#define FSQRT   (0b110101)

#define LDI     (0b101000)
#define LDR     (0b101100)
#define STI     (0b101001)
#define FLDI    (0b101010)
#define FSTI    (0b101011)
#define FLDR    (0b101110)

#define BEQ     (0b100000)
#define BLT     (0b100001)
#define BLE     (0b100010)
#define FBEQ    (0b100100)
#define FBLT    (0b100101)
#define FBLE    (0b100110)

#define J       (0b111000)
#define JR      (0b111001)
#define CALL    (0b111010)
#define CALLR   (0b111011)
#define RETURN  (0b111100)
#define INPUTB  (0b111101)
#define OUTPUTB (0b111110)
#define HALT    (0b111111)
#define DEBUG   (0b101111)

using namespace std;
#endif
