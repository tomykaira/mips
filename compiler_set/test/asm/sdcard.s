	nop
begin:
	addi	$r3, $r0, 1
	slli	$r4, $r3, 25
loop:
	nop
	addi	$r3, $r3, 1
	srai	$r5, $r3, 20
	slli	$r5, $r5, 20
	sub	$r5, $r3, $r5
	beq	$r5, $r0, print
	nop
	nop
	blt	$r3, $r4, loop
	nop
	nop
print:
	addi	$r6, $r0, 46
	outputb	$r6
	blt	$r3, $r4, loop
	nop
	nop
loop:
	addi	$r3, $r0, 510
	readsd	$r3, $r4
	outputb	$r4

	addi	$r3, $r0, 511
	readsd	$r3, $r4
	outputb	$r4	# 0xaa

	addi	$r4, $r0, 172
	writesd	$r3, $r4
	outputb	$r4

	readsd	$r3, $r4
	outputb	$r4	# 0xac

	addi	$r3, $r0, 32
	slli	$r3, $r3, 15
	readsd	$r3, $r4
	outputb	$r4	# 0xeb

	addi	$r3, $r0, 511
	readsd	$r3, $r4
	outputb	$r4	# 0xac
	halt
