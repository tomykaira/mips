  nop
  addi $r3, $r0, 0
loop:
  addi $r3, $r3, 1
  display $r3, $r3 # dummy
  j loop
  halt
