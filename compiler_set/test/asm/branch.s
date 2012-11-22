# TODO: implement delay slot
	nop
	addi $r3, $r0, 4
	blt $r3, $r0, start # not taken
	subi $r3, $r3, 8
	blt $r3, $r0, start # taken
	outputb $r3 # shoud run  fc
	outputb $r3 # shoud run  fc
	outputb $r3 # shoud not run
start:
	addi $r3, $r3, 4
	outputb $r3 # shoud be 0
	halt
