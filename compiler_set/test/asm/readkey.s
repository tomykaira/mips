	nop
start:
	readkey $r3
	outputb $r3
	j start
	halt
