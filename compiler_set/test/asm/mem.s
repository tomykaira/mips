#EXPECTED: 10 0 0
	nop
  addi $r2, $r0, 0
  addi $r3, $r0, 5
  sti $r3, $r2, 3
  ldi $r4, $r2, 3
  add $r5, $r3, $r4 # stall
	outputb $r5
  ldi $r4, $r2, 3
  add $r8, $r2, $r0 # not stall
	outputb $r8
  sti $r5, $r5, 3
  nop
  ldr $r9, $r3, $r5
	outputb $r9
  halt
