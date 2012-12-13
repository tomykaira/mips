	nop
	addi	$r3, $r0, 2
	slli	$r3, $r3, 15
	addi	$r3, $r3, 16384
	addi	$r5, $r0, 0
	addi	$r6, $r0, 512
loop:
	readsd	$r3, $r4
	outputb	$r4
	addi	$r3, $r3, 1
	addi	$r5, $r5, 1
	blt	$r5, $r6, loop
	nop
	nop
	halt
