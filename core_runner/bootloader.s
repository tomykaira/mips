	nop
# r3: temporary
# r4: loaded instruction
# r5: opcode (+ 2bits)
# r8: 0xffffffff
# r9: pc offset (end of loader)
# r10: instruction pointer (increment)
# r11: jump opcode
# r12: call opcode
# r13: setl opcode
loader:
	subi	$r8, $r0, 1	# end marker ffffffff
	setl	$r9, end_of_loader	# offset
	setl	$r10, end_of_loader	# instruction pointer
	addi	$r11, $r0, 224	# jump
	addi	$r12, $r0, 232	# call
	addi	$r13, $r0, 64	# setl
load_start:
	inputb	$r3
	addi	$r5, $r3, 0	# backup
	slli	$r4, $r3, 8
	inputb	$r3
	add	$r4, $r4, $r3
	slli	$r4, $r4, 8
	inputb	$r3
	add	$r4, $r4, $r3
	slli	$r4, $r4, 8
	inputb	$r3
	add	$r4, $r4, $r3
	beq	$r4, $r8, load_end
	nop
	nop

	# add pc for jump and call
	beq	$r5, $r11, add_pc
	nop
	nop
	beq	$r5, $r12, add_pc
	nop
	nop
	beq	$r5, $r13, add_pc
	nop
	nop

	program	$r10, $r4
	addi	$r10, $r10, 1
	j	load_start
add_pc:
	add	$r4, $r4, $r9	# add offset
	program	$r10, $r4
	addi	$r10, $r10, 1
	j	load_start
load_end:
	call	end_of_loader
	j	loader
	halt			# never reach here
end_of_loader:
