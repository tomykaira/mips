#EXPECTED: 8 0
	nop
	setl $r2, start
	addi $r3, $r0, 4
	callr $r2
	subi $r3, $r3, 8 # should not run first time
	outputb $r3 # shoud be 0
	halt
start:
	add $r3, $r3, $r3 # should run
	outputb $r3 # shoud be 8
	return
