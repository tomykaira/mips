#EXPECTED: 5
	nop
	setl $r3, here
	addi $r5, $r0, 1
	sti $r3, $r3, 5
	nop
	nop
	ldi $r28, $r3, 5
	callr $r28
	outputb $r5
	halt
here:
	addi $r5, $r0, 5
	return
