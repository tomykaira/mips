	nop
start:
	addi $r3, $r0, 0
	addi $r4, $r0, 2400
	addi $r5, $r0, 65
	blt $r4, $r3, end
	nop
	nop
	display $r3, $r5
	addi $r3, $r3, 1
	j start
end:
	halt
