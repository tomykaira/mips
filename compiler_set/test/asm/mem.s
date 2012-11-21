  nop
  addi $r2, $r0, 0
  addi $r3, $r0, 5
  sti $r3, $r2, 3
  ldi $r4, $r2, 3
  add $r5, $r3, $r4 # stall
  ldi $r4, $r2, 3
  add $r8, $r2, $r0 # not stall
  sti $r5, $r5, 3
  nop
  ldr $r9, $r3, $r5
  halt
