	nop
	addi $r3, $r0, 4
	j start
	add $r3, $r3, $r3 # should not run
start:
	add $r3, $r3, $r3 # should run
	outputb $r3 # shoud be 8, not 16
	halt
