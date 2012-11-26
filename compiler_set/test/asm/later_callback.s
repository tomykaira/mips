  addi $r3, $r0, 100
loop:
  subi $r3, $r3, 1
  ble $r3, $r0, start
  nop
  nop
  j loop
start:
  inputb $r3
  outputb $r3
  j start
