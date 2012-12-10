# put value into all elements from start to end
# start end value
initialize_array:
	add	$r6, $r2, $r3
	sti	$r5, $r6, 0
	addi	$r3, $r3, 1
	blt	$r3, $r4, initialize_array
	nop
	nop
initialize_array_end:
	return

mul_sub:
	beq	$r4, $r0, mul_beq_taken.1325
	nop
	nop
	srai	$r5, $r4, 1
	slli	$r6, $r5, 1
	sub	$r4, $r4, $r6
	beq	$r4, $r0, mul_beq_taken.1274
	nop
	nop
	slli	$r4, $r3, 1
	beq	$r5, $r0, mul_return.1374
	nop
	nop
	srai	$r8, $r5, 1
	slli	$r6, $r8, 1
	sub	$r5, $r5, $r6
	beq	$r5, $r0, mul_beq_taken.1276
	nop
	nop
	slli	$r6, $r4, 1
	beq	$r8, $r0, mul_beq_taken.1285
	nop
	nop
	srai	$r7, $r8, 1
	slli	$r5, $r7, 1
	sub	$r5, $r8, $r5
	beq	$r5, $r0, mul_beq_taken.1278
	nop
	nop
	slli	$r5, $r6, 1
	sti	$r3, $r1, 0
	sti	$r4, $r1, -1
	beq	$r7, $r0, mul_beq_taken.1279
	nop
	nop
	srai	$r4, $r7, 1
	slli	$r3, $r4, 1
	sub	$r3, $r7, $r3
	sti	$r6, $r1, -2
	beq	$r3, $r0, mul_beq_taken.1281
	nop
	nop
	slli	$r3, $r5, 1
	sti	$r5, $r1, -3
	subi	$r1, $r1, 4
	call	mul_sub
	addi	$r1, $r1, 4
	ldi	$r5, $r1, -3
	add	$r27, $r3, $r5
	j	mul_beq_cont.1282
mul_beq_taken.1281:
	slli	$r3, $r5, 1
	subi	$r1, $r1, 4
	call	mul_sub
	addi	$r1, $r1, 4
	addi	$r27, $r3, 0
mul_beq_cont.1282:
	ldi	$r6, $r1, -2
	add	$r5, $r27, $r6
	j	mul_beq_cont.1280
mul_beq_taken.1279:
	addi	$r5, $r6, 0
mul_beq_cont.1280:
	ldi	$r4, $r1, -1
	add	$r4, $r5, $r4
	ldi	$r3, $r1, 0
	add	$r3, $r4, $r3
	return
mul_beq_taken.1278:
	slli	$r5, $r6, 1
	beq	$r7, $r0, mul_beq_taken.1285
	nop
	nop
	sti	$r4, $r1, -1
	srai	$r4, $r7, 1
	sti	$r3, $r1, 0
	slli	$r3, $r4, 1
	sub	$r3, $r7, $r3
	beq	$r3, $r0, mul_beq_taken.1286
	nop
	nop
	slli	$r3, $r5, 1
	sti	$r5, $r1, -4
	subi	$r1, $r1, 5
	call	mul_sub
	addi	$r1, $r1, 5
	ldi	$r5, $r1, -4
	add	$r27, $r3, $r5
	j	mul_beq_cont.1287
mul_beq_taken.1286:
	slli	$r3, $r5, 1
	subi	$r1, $r1, 5
	call	mul_sub
	addi	$r1, $r1, 5
	addi	$r27, $r3, 0
mul_beq_cont.1287:
	ldi	$r4, $r1, -1
	add	$r4, $r27, $r4
	ldi	$r3, $r1, 0
	add	$r3, $r4, $r3
	return
mul_beq_taken.1285:
	add	$r3, $r4, $r3
	return
mul_beq_taken.1276:
	slli	$r7, $r4, 1
	beq	$r8, $r0, mul_return.1374
	nop
	nop
	srai	$r6, $r8, 1
	slli	$r4, $r6, 1
	sub	$r4, $r8, $r4
	beq	$r4, $r0, mul_beq_taken.1291
	nop
	nop
	slli	$r5, $r7, 1
	sti	$r3, $r1, 0
	beq	$r6, $r0, mul_beq_taken.1292
	nop
	nop
	srai	$r4, $r6, 1
	slli	$r3, $r4, 1
	sub	$r3, $r6, $r3
	sti	$r7, $r1, -5
	beq	$r3, $r0, mul_beq_taken.1294
	nop
	nop
	slli	$r3, $r5, 1
	sti	$r5, $r1, -6
	subi	$r1, $r1, 7
	call	mul_sub
	addi	$r1, $r1, 7
	ldi	$r5, $r1, -6
	add	$r27, $r3, $r5
	j	mul_beq_cont.1295
mul_beq_taken.1294:
	slli	$r3, $r5, 1
	subi	$r1, $r1, 7
	call	mul_sub
	addi	$r1, $r1, 7
	addi	$r27, $r3, 0
mul_beq_cont.1295:
	ldi	$r7, $r1, -5
	add	$r4, $r27, $r7
	j	mul_beq_cont.1293
mul_beq_taken.1292:
	addi	$r4, $r7, 0
mul_beq_cont.1293:
	ldi	$r3, $r1, 0
	add	$r3, $r4, $r3
	return
mul_beq_taken.1291:
	slli	$r5, $r7, 1
	beq	$r6, $r0, mul_return.1374
	nop
	nop
	srai	$r4, $r6, 1
	sti	$r3, $r1, 0
	slli	$r3, $r4, 1
	sub	$r3, $r6, $r3
	beq	$r3, $r0, mul_beq_taken.1299
	nop
	nop
	slli	$r3, $r5, 1
	sti	$r5, $r1, -7
	subi	$r1, $r1, 8
	call	mul_sub
	addi	$r1, $r1, 8
	ldi	$r5, $r1, -7
	add	$r27, $r3, $r5
	j	mul_beq_cont.1300
mul_beq_taken.1299:
	slli	$r3, $r5, 1
	subi	$r1, $r1, 8
	call	mul_sub
	addi	$r1, $r1, 8
	addi	$r27, $r3, 0
mul_beq_cont.1300:
	ldi	$r3, $r1, 0
	add	$r3, $r27, $r3
	return
mul_return.1374:
	return
mul_beq_taken.1274:
	slli	$r3, $r3, 1
	beq	$r5, $r0, mul_beq_taken.1325
	nop
	nop
	srai	$r7, $r5, 1
	slli	$r4, $r7, 1
	sub	$r4, $r5, $r4
	beq	$r4, $r0, mul_beq_taken.1304
	nop
	nop
	slli	$r4, $r3, 1
	beq	$r7, $r0, mul_return.1374
	nop
	nop
	srai	$r6, $r7, 1
	slli	$r5, $r6, 1
	sub	$r5, $r7, $r5
	beq	$r5, $r0, mul_beq_taken.1306
	nop
	nop
	slli	$r5, $r4, 1
	sti	$r3, $r1, -8
	beq	$r6, $r0, mul_beq_cont.1308
	nop
	nop
	sti	$r4, $r1, -9
	srai	$r4, $r6, 1
	slli	$r3, $r4, 1
	sub	$r3, $r6, $r3
	beq	$r3, $r0, mul_beq_taken.1309
	nop
	nop
	slli	$r3, $r5, 1
	sti	$r5, $r1, -10
	subi	$r1, $r1, 11
	call	mul_sub
	addi	$r1, $r1, 11
	ldi	$r5, $r1, -10
	add	$r27, $r3, $r5
	j	mul_beq_cont.1310
mul_beq_taken.1309:
	slli	$r3, $r5, 1
	subi	$r1, $r1, 11
	call	mul_sub
	addi	$r1, $r1, 11
	addi	$r27, $r3, 0
mul_beq_cont.1310:
	ldi	$r4, $r1, -9
	add	$r4, $r27, $r4
mul_beq_cont.1308:
	ldi	$r3, $r1, -8
	add	$r3, $r4, $r3
	return
mul_beq_taken.1306:
	slli	$r5, $r4, 1
	beq	$r6, $r0, mul_return.1374
	nop
	nop
	srai	$r4, $r6, 1
	sti	$r3, $r1, -8
	slli	$r3, $r4, 1
	sub	$r3, $r6, $r3
	beq	$r3, $r0, mul_beq_taken.1314
	nop
	nop
	slli	$r3, $r5, 1
	sti	$r5, $r1, -11
	subi	$r1, $r1, 12
	call	mul_sub
	addi	$r1, $r1, 12
	ldi	$r5, $r1, -11
	add	$r27, $r3, $r5
	j	mul_beq_cont.1315
mul_beq_taken.1314:
	slli	$r3, $r5, 1
	subi	$r1, $r1, 12
	call	mul_sub
	addi	$r1, $r1, 12
	addi	$r27, $r3, 0
mul_beq_cont.1315:
	ldi	$r3, $r1, -8
	add	$r3, $r27, $r3
	return
mul_beq_taken.1304:
	slli	$r4, $r3, 1
	beq	$r7, $r0, mul_beq_taken.1325
	nop
	nop
	srai	$r5, $r7, 1
	slli	$r3, $r5, 1
	sub	$r3, $r7, $r3
	beq	$r3, $r0, mul_beq_taken.1319
	nop
	nop
	slli	$r3, $r4, 1
	beq	$r5, $r0, mul_beq_taken.1320
	nop
	nop
	sti	$r4, $r1, -12
	srai	$r4, $r5, 1
	slli	$r6, $r4, 1
	sub	$r5, $r5, $r6
	beq	$r5, $r0, mul_beq_taken.1321
	nop
	nop
	sti	$r3, $r1, -13
	slli	$r3, $r3, 1
	subi	$r1, $r1, 14
	call	mul_sub
	addi	$r1, $r1, 14
	ldi	$r5, $r1, -13
	add	$r27, $r3, $r5
	j	mul_beq_cont.1322
mul_beq_taken.1321:
	slli	$r3, $r3, 1
	subi	$r1, $r1, 14
	call	mul_sub
	addi	$r1, $r1, 14
	addi	$r27, $r3, 0
mul_beq_cont.1322:
	ldi	$r4, $r1, -12
	add	$r3, $r27, $r4
	return
mul_beq_taken.1320:
	addi	$r3, $r4, 0
	return
mul_beq_taken.1319:
	slli	$r3, $r4, 1
	beq	$r5, $r0, mul_beq_taken.1325
	nop
	nop
	srai	$r4, $r5, 1
	slli	$r6, $r4, 1
	sub	$r5, $r5, $r6
	beq	$r5, $r0, mul_beq_taken.1326
	nop
	nop
	sti	$r3, $r1, -14
	slli	$r3, $r3, 1
	subi	$r1, $r1, 15
	call	mul_sub
	addi	$r1, $r1, 15
	ldi	$r4, $r1, -14
	add	$r3, $r3, $r4
	return
mul_beq_taken.1326:
	slli	$r3, $r3, 1
	j	mul_sub
mul_beq_taken.1325:
	addi	$r3, $r0, 0
	return

min_caml_mul:
	ble	$r0, $r4, MUL_GE_ZERO
	nop
	nop
	sub	$r3, $r0, $r3
	sub	$r4, $r0, $r4
MUL_GE_ZERO:
	j	mul_sub


read_key:
	readkey	$r3
	srai	$r5, $r3, 8
	slli	$r5, $r5, 8
	sub	$r3, $r3, $r5
	return

# void move_memory(char * array, int offset, int size)
move_memory:
	blt	$r4, $r0, move_memory_minus
	nop
	nop
move_memory_plus:
	add	$r6, $r3, $r5	# index limit
	subi	$r6, $r6, 1	# last element
move_memory_plus_loop:
	ldi	$r8, $r6, 0
	add	$r7, $r6, $r4
	sti	$r8, $r7, 0
	subi	$r6, $r6, 1	# use stall to increment
	ble	$r3, $r6, move_memory_plus_loop
	nop
	nop
	return
move_memory_minus:
	add	$r6, $r3, $r5       # index limit
	sub	$r4, $r0, $r4       # - offset (>0)
move_memory_minus_loop:
	ldr	$r8, $r3, $r4
	addi	$r3, $r3, 1	# use stall to increment
	sti	$r8, $r3, -1
	blt	$r3, $r6, move_memory_minus_loop
	nop
	nop
	return

move_and_clear_memory:
	blt	$r4, $r0, move_and_clear_minus
	nop
	nop
move_and_clear_plus:
	add	$r6, $r3, $r5	# index limit
	subi	$r6, $r6, 1	# last element
move_and_clear_plus_loop:
	ldi	$r8, $r6, 0
	sti	$r0, $r6, 0
	add	$r7, $r6, $r4
	sti	$r8, $r7, 0
	subi	$r6, $r6, 1	# use stall to increment
	ble	$r3, $r6, move_and_clear_plus_loop
	nop
	nop
	return
move_and_clear_minus:
	add	$r6, $r3, $r5       # index limit
	sub	$r4, $r0, $r4       # - offset (>0)
move_and_clear_minus_loop:
	ldr	$r8, $r3, $r4
	add	$r7, $r3, $r4
	sti	$r0, $r7, 0
	addi	$r3, $r3, 1	# use stall to increment
	sti	$r8, $r3, -1
	blt	$r3, $r6, move_and_clear_minus_loop
	nop
	nop
	return

display:
	display	$r3, $r4
	return

send_display:
	addi	$r5, $r0, 0
	addi	$r4, $r0, 2400
clear_display_start:
	ldr	$r8, $r3, $r5
	display	$r5, $r8
	addi	$r5, $r5, 1
	blt	$r5, $r4, clear_display_start
	nop
	nop
	debug	8
	return

send_rs:
	addi	$r5, $r0, 0
send_rs_start:
	ble	$r4, $r5, send_rs_end
	nop
	nop
	ldr	$r8, $r3, $r5
	outputb	$r8
	addi	$r5, $r5, 1
	j	send_rs_start
send_rs_end:
	return

print_int:
	outputb	$r3
	srai	$r4, $r3, 8
	outputb	$r4
	srai	$r4, $r3, 16
	outputb	$r4
	srai	$r4, $r3, 24
	outputb	$r4
	addi	$r3, $r0, 10
	outputb	$r3
	return

print_char:
	outputb	$r3
	addi	$r3, $r0, 10
	outputb	$r3
	return

# from bootloader.s

# r3: temporary
# r4: loaded instruction
# r5: opcode (+ 2bits)
# r8: 0xffffffff
# r9: pc offset (end of loader)
# r10: instruction pointer (increment)
# r11: jump opcode
# r12: call opcode
# r13: setl opcode
load_program:
	subi	$r8, $r0, 1	# end marker ffffffff
	setl	$r9, program_end	# offset
	setl	$r10, program_end	# instruction pointer
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
	j	invoke_subprocess	# generated by compiler

halt:
	halt

inputb:
	inputb	$r3
	return

str_equal:
	addi	$r6, $r0, 0
str_equal_loop:
	ldr	$r7, $r3, $r6
	ldr	$r8, $r4, $r6
	beq	$r7, $r8, str_equal_next
	nop
	nop
	j	str_equal_false
str_equal_next:
	addi	$r6, $r6, 1
	blt	$r6, $r5, str_equal_loop
	nop
	nop
str_equal_true:
	addi	$r3, $r0, 1
	return
str_equal_false:
	addi	$r3, $r0, 0
	return

# return string length
copy_string:
	addi	$r6, $r3, 0
	addi	$r3, $r0, 0
copy_string_loop:
	ldr	$r7, $r4, $r3
	beq	$r7, $r0, copy_string_end
	add	$r8, $r6, $r3
	nop
	sti	$r7, $r8, 0
	addi	$r3, $r3, 1
	j	copy_string_loop
	nop
	nop
copy_string_end:
	return

copy_n_string:
	addi	$r6, $r3, 0
	addi	$r3, $r0, 0
copy_n_string_loop:
	ldr	$r7, $r4, $r3
	beq	$r7, $r0, copy_n_string_end
	add	$r8, $r6, $r3
	nop
	sti	$r7, $r8, 0
	addi	$r3, $r3, 1
	blt	$r3, $r5, copy_n_string_loop
	nop
	nop
copy_n_string_end:
	return

error:
	debug	6
	halt

debug:
	debug	6
	return

debug_input:
	debug	9
	return

# byte read_sd(int address)
read_sd:
	addi	$r4, $r3, 0
	readsd	$r4, $r3
	return

# void write_sd(int address, int data)
write_sd:
	writesd	$r3, $r4
	return
