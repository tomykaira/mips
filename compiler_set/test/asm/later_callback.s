  addi $r3, $r0, 10000
loop_10000:
  subi $r3, $r3, 1
  ble $r3, $r0, start
  j loop_10000
start:
  inputb $r3
  outputb $r3
  j start
