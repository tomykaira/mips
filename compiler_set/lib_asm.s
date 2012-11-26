
#----------------------------------------------------------------------
#
# lib_asm.s
#
#----------------------------------------------------------------------

# * create_array
min_caml_create_array:
	add $r5, $r3, $r2
	mov $r3, $r2
CREATE_ARRAY_LOOP:
	blt  $r2, $r5, CREATE_ARRAY_CONTINUE
	nop
	nop
	return
CREATE_ARRAY_CONTINUE:
	sti $r4, $r2, 0
	addi $r2, $r2, 1
	j CREATE_ARRAY_LOOP

# * create_float_array
min_caml_create_float_array:
	add $r4, $r3, $r2
	mov $r3, $r2
CREATE_FLOAT_ARRAY_LOOP:
	blt $r2, $r4, CREATE_FLOAT_ARRAY_CONTINUE
	nop
	nop
	return
CREATE_FLOAT_ARRAY_CONTINUE:
	fsti $f1, $r2, 0
	addi $r2, $r2, 1
	j CREATE_FLOAT_ARRAY_LOOP

# * int_tuple_array
min_caml_int_tuple_array:
	ble	$r2, $r4, INT_TUPLE_ARRAY_RETURN
	nop
	nop
	sti	$r3, $r4, 0
	add	$r4, $r4, $r5
	j	min_caml_int_tuple_array
INT_TUPLE_ARRAY_RETURN:
	return

# * float_tuple_array
min_caml_float_tuple_array:
	ble	$r2, $r3, FLOAT_TUPLE_ARRAY_RETURN
	nop
	nop
	fsti	$f1, $r3, 0
	add	$r3, $r3, $r4
	j	min_caml_float_tuple_array
FLOAT_TUPLE_ARRAY_RETURN:
	return


# * floor		$f1 + MAGICF - MAGICF
min_caml_floor:
	fadd $f2, $f1, $f0
	# $f3 <- 8388608.0(0x4b000000) (delay slot)
	fmvhi $f3, 19200
	fmvlo $f3, 0
	fblt $f1, $f0, FLOOR_NEGATIVE	# if ($f0 <= $f1) goto FLOOR_POSITIVE
	nop
	nop
FLOOR_POSITIVE:
	fblt $f3, $f1, FLOOR_POSITIVE_RET
	nop
	nop
FLOOR_POSITIVE_MAIN:
	fadd $f2, $f1, $f0
	fadd $f1, $f1, $f3
	fsti $f1, $r1, 0
	ldi $r4, $r1, 0
	fsub $f1, $f1, $f3
	fsti $f1, $r1, 0
	ldi $r4, $r1, 0
	fblt $f2, $f1, FLOOR_POSITIVE_RET
	nop
	nop
	return
FLOOR_POSITIVE_RET:
	# $f4 <- 1.0
	# fset $f4, 1.0
	fmvhi $f4, 16256
	fmvlo $f4, 0
	fsub $f1, $f1, $f4
	return
FLOOR_NEGATIVE:
	fsub $f1, $f0, $f1
	fblt $f3, $f1, FLOOR_NEGATIVE_RET
	nop
	nop
FLOOR_NEGATIVE_MAIN:
	fadd $f1, $f1, $f3
	fsub $f1, $f1, $f3
	fsub $f2, $f0, $f2
	fblt $f1, $f2, FLOOR_NEGATIVE_PRE_RET
	nop
	nop
	j FLOOR_NEGATIVE_RET
FLOOR_NEGATIVE_PRE_RET:
	fadd $f1, $f1, $f3
	# $f4 <- 1.0
	# fset $f4, 1.0
	fmvhi $f4, 16256
	fmvlo $f4, 0
	fadd $f1, $f1, $f4
	fsub $f1, $f1, $f3
FLOOR_NEGATIVE_RET:
	fsub $f1, $f0, $f1
	return

min_caml_ceil:
	fsub $f1, $f0, $f1
	call min_caml_floor
	fsub $f1, $f0, $f1
	return

# * float_of_int
min_caml_float_of_int:
	blt $r3, $r0, ITOF_NEGATIVE_MAIN		# if ($r0 <= $r3) goto ITOF_MAIN
	nop
	nop
ITOF_MAIN:
	# $f2 <- 8388608.0(0x4b000000)
	fmvhi $f2, 19200
	fmvlo $f2, 0
	# $r4 <- 0x4b000000
	addi $r4, $r0, 19200
	slli $r4, $r4, 16
	# $r5 <- 0x00800000
	addi $r5, $r0, 128
	slli $r5, $r5, 16
	blt $r3, $r5, ITOF_SMALL
	nop
	nop
ITOF_BIG:
	# $f3 <- 0.0
	fadd $f3, $f0, $f0
ITOF_LOOP:
	sub $r3, $r3, $r5
	fadd $f3, $f3, $f2
	blt $r3, $r5, ITOF_RET
	nop
	nop
	j ITOF_LOOP
ITOF_RET:
	add $r3, $r3, $r4
	sti $r3, $r1, 0
	fldi $f1, $r1, 0
	fsub $f1, $f1, $f2
	fadd $f1, $f1, $f3
	return
ITOF_SMALL:
	add $r3, $r3, $r4
	imovf $f1, $r3
	fsub $f1, $f1, $f2
	return
ITOF_NEGATIVE_MAIN:
	sub $r3, $r0, $r3
	call ITOF_MAIN
	fsub $f1, $f0, $f1
	return

# * int_of_float
min_caml_int_of_float:
	fblt $f1, $f0, FTOI_NEGATIVE_MAIN			# if (0.0 <= $f1) goto FTOI_MAIN
	nop
	nop
FTOI_POSITIVE_MAIN:
	# ここを有効にするとocaml仕様になる
	#LORELEY call min_caml_floor
	# $f2 <- 8388608.0(0x4b000000)
	fmvhi $f2, 19200
	fmvlo $f2, 0
	# $r4 <- 0x4b000000
	addi $r4, $r0, 19200
	slli $r4, $r4, 16
	fblt $f1, $f2, FTOI_SMALL		# if (MAGICF <= $f1) goto FTOI_BIG
	nop
	nop
	# $r5 <- 0x00800000
	addi $r5, $r0, 128
	slli $r5, $r5, 16
	mov $r3, $r0
FTOI_LOOP:
	fsub $f1, $f1, $f2
	add $r3, $r3, $r5
	fblt $f1, $f2, FTOI_RET
	nop
	nop
	j FTOI_LOOP
FTOI_RET:
	fadd $f1, $f1, $f2
	fmovi $r5, $f1
	sub $r5, $r5, $r4
	add $r3, $r5, $r3
	return
FTOI_SMALL:
	fadd $f1, $f1, $f2
	fmovi $r3, $f1
	sub $r3, $r3, $r4
	return
FTOI_NEGATIVE_MAIN:
	fsub $f1, $f0, $f1
	call FTOI_POSITIVE_MAIN
	sub $r3, $r0, $r3
	return

# * truncate
min_caml_truncate:
	j min_caml_int_of_float

# ビッグエンディアン
min_caml_read_int:
	add $r3, $r0, $r0
	# 24 - 31
	inputb $r4
	add $r3, $r3, $r4
	slli $r3, $r3, 8
	# 16 - 23
	inputb $r4
	add $r3, $r3, $r4
	slli $r3, $r3, 8
	# 8 - 15
	inputb $r4
	add $r3, $r3, $r4
	slli $r3, $r3, 8
	# 0 - 7
	inputb $r4
	add $r3, $r3, $r4
	return

min_caml_read_float:
	call	min_caml_read_int
	imovf 	$f1, $r3
	return

#----------------------------------------------------------------------
#
# lib_asm.s
#
#----------------------------------------------------------------------


min_caml_sqrt:
	fsqrt $f1, $f1
	return

min_caml_xor:
	xor $r3, $r3, $r4
	return

min_caml_print_newline:
	addi $r3, $r0, 10
	outputb $r3
	return

min_caml_print_char:
	outputb $r3
	return

min_caml_input_char:
	inputb $r3
	return

min_caml_read_char:
	inputb $r3
	return



#----------------------------------------------------------------------
#
# 以上追加分
#
#----------------------------------------------------------------------


# algorithm: remez5, 0_log2
# in: $f1, out: $f1
min_caml_exp:
	fsti $f1, $r1, 0
	# $f4 <- 1.4426950216293335 (1/log(2))
	# fset $f4, 3fb8aa3b
	fmvhi $f5, 16312
	fmvlo $f5, 43579
	fmul $f1, $f1, $f5
	subi $r1, $r1, 2
	call min_caml_floor
	addi $r1, $r1, 2
	fsti $f1, $r1, -1
	subi $r1, $r1, 2
	call min_caml_int_of_float
	addi $r1, $r1, 2
	# px = $f1, x = $f2, C1 = f4, C2 = f5
	fldi $f2, $r1, 0
	fldi $f1, $r1, -1

	fmvhi $f6, 0
	fmvlo $f6, 0
	fblt $f6, $f2, exp_skip
	nop
	nop
	# a - 1 if a < 0
	fmvhi $f6, 16256
	fmvlo $f6, 0
	fsub $f1, $f1, $f6
	subi $r3, $r3, 1
exp_skip:
	fmvhi $f4, 16177
	fmvlo $f4, 29184
	fmvhi $f5, 13759
	fmvlo $f5, 48782
	fmul $f3, $f4, $f1
	fsub $f2, $f2, $f3
	fmul $f3, $f5, $f1
	fsub $f2, $f2, $f3
	# x = $f2, a = $f3
	fmvhi $f3, 15426
	fmvlo $f3, 12737
	fmul $f3, $f3, $f2

	fmvhi $f4, 15646
	fmvlo $f4, 44824
	fadd $f3, $f3, $f4
	fmul $f3, $f3, $f2

	fmvhi $f4, 15915
	fmvlo $f4, 51130
	fadd $f3, $f3, $f4
	fmul $f3, $f3, $f2

	fmvhi $f4, 16127
	fmvlo $f4, 59474
	fadd $f3, $f3, $f4
	fmul $f3, $f3, $f2

	fmvhi $f4, 16256
	fmvlo $f4, 92
	fadd $f3, $f3, $f4
	fmul $f3, $f3, $f2

	fmvhi $f4, 16255
	fmvlo $f4, 65534
	fadd $f3, $f3, $f4

	addi $r3, $r3, 127
	slli $r3, $r3, 23
	imovf $f4, $r3
	fble $f0, $f4, exp_cont
	nop
	nop
	fsub $f4, $f0, $f4
exp_cont:
	fmul $f1, $f3, $f4
	return

# algorithm: remez5, 1_e
# in: $f1, out: $f1
min_caml_log:
	# 0
	fblt $f1, $f0, LOG_END	# if ($f1 < 0) return itself, otherwise this loop will not stop
	nop
	nop

	# 1
	fmvhi $f6, 16256
	fmvlo $f6, 0

	# e
	fmvhi $f7, 16429
	fmvlo $f7, 63572

	# 1/e
	fmvhi $f8, 16060
	fmvlo $f8, 23218

	# y
	addi $r3, $r0, 0

	fble $f6, $f1, LOG_LOOP_LT_1_END
	nop
	nop

LOG_LOOP_LT_1:
	subi $r3, $r3, 1
	fmul $f1, $f1, $f7
	fblt $f1, $f6, LOG_LOOP_LT_1
	nop
	nop

LOG_LOOP_LT_1_END:
	fblt $f1, $f7, LOG_LOOP_GT_E_END
	nop
	nop

LOG_LOOP_GT_E:
	addi $r3, $r3, 1
	fmul $f1, $f1, $f8
	fblt $f7, $f1, LOG_LOOP_GT_E
	nop
	nop

LOG_LOOP_GT_E_END:

	# x = $f1, a = $f3
	fmvhi $f3, 15438
	fmvlo $f3, 18672
	fmul $f3, $f3, $f1

	fmvhi $f4, 48658
	fmvlo $f4, 58995
	fadd $f3, $f3, $f4
	fmul $f3, $f3, $f1

	fmvhi $f4, 16174
	fmvlo $f4, 35574
	fadd $f3, $f3, $f4
	fmul $f3, $f3, $f1

	fmvhi $f4, 49123
	fmvlo $f4, 52990
	fadd $f3, $f3, $f4
	fmul $f3, $f3, $f1

	fmvhi $f4, 16449
	fmvlo $f4, 23445
	fadd $f3, $f3, $f4
	fmul $f3, $f3, $f1

	fmvhi $f4, 49125
	fmvlo $f4, 27378
	fadd $f3, $f3, $f4

	fsti $f3, $r1, 0
	subi $r1, $r1, 1
	# y = $r3 to $f1
	call min_caml_float_of_int
	addi $r1, $r1, 1
	fldi $f2, $r1, 0
	fadd $f1, $f1, $f2
LOG_END:
	return

min_caml_sinh:
	call min_caml_exp
	# $f1 = e^x
	finv $f2, $f1
	# e^x - e^(-x)
	fsub $f2, $f1, $f2
	# * 0.5
	fmvhi $f4, 16128
	fmvlo $f4, 0
	fmul $f1, $f2, $f4
	return

min_caml_cosh:
	call min_caml_exp
	# $f1 = e^x
	finv $f2, $f1
	# e^x - e^(-x)
	fadd $f2, $f1, $f2
	# * 0.5
	fmvhi $f4, 16128
	fmvlo $f4, 0
	fmul $f1, $f2, $f4
	return


min_caml_lsr:
	ble	$r4, $r0, lsr_return
	nop
	nop
	srai	$r3, $r3, 1
	subi	$r4, $r4, 1
	j	min_caml_lsr
lsr_return:
	return


#mul_sub for min_caml_mul
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

#min_caml_mul
min_caml_mul:
	ble	$r0, $r4, MUL_GE_ZERO
	nop
	nop
	sub	$r3, $r0, $r3
	sub	$r4, $r0, $r4
MUL_GE_ZERO:
	j	mul_sub


#min_caml_div_binary_search
min_caml_div_binary_search:
	sti	$r3, $r1, 0
	add	$r3, $r5, $r6
	srai	$r3, $r3, 1
	sti	$r4, $r1, -1
	sti	$r3, $r1, -2
	sti	$r5, $r1, -3
	sti	$r6, $r1, -4
	subi	$r1, $r1, 5
	call	min_caml_mul
	addi	$r1, $r1, 5
	ldi	$r7, $r1, -3
	ldi	$r9, $r1, -4
	sub	$r5, $r9, $r7
	ble	$r5, $r30, div_ble_taken.1899
	nop
	nop
	ldi	$r6, $r1, 0
	blt	$r3, $r6, div_blt_taken.1900
	nop
	nop
	beq	$r3, $r6, div_beq_taken.1901
	nop
	nop
	ldi	$r8, $r1, -2
	add	$r3, $r7, $r8
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -5
	subi	$r1, $r1, 6
	call	min_caml_mul
	addi	$r1, $r1, 6
	ldi	$r8, $r1, -3
	ldi	$r9, $r1, -2
	sub	$r6, $r9, $r8
	ble	$r6, $r30, div_ble_taken.1919
	nop
	nop
	ldi	$r5, $r1, 0
	blt	$r3, $r5, div_blt_taken.1904
	nop
	nop
	beq	$r3, $r5, div_beq_taken.1905
	nop
	nop
	ldi	$r7, $r1, -5
	add	$r3, $r8, $r7
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -6
	subi	$r1, $r1, 7
	call	min_caml_mul
	addi	$r1, $r1, 7
	ldi	$r9, $r1, -3
	ldi	$r8, $r1, -5
	sub	$r6, $r8, $r9
	ble	$r6, $r30, div_ble_taken.1907
	nop
	nop
	ldi	$r5, $r1, 0
	blt	$r3, $r5, div_blt_taken.1908
	nop
	nop
	beq	$r3, $r5, div_beq_taken.1909
	nop
	nop
	ldi	$r7, $r1, -6
	add	$r3, $r9, $r7
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -7
	subi	$r1, $r1, 8
	call	min_caml_mul
	addi	$r1, $r1, 8
	ldi	$r5, $r1, -3
	ldi	$r9, $r1, -6
	sub	$r8, $r9, $r5
	ble	$r8, $r30, div_ble_taken.1911
	nop
	nop
	ldi	$r7, $r1, 0
	blt	$r3, $r7, div_blt_taken.1912
	nop
	nop
	beq	$r3, $r7, div_beq_taken.1913
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -7
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_beq_taken.1913:
	ldi	$r6, $r1, -7
	addi	$r3, $r6, 0
	return
div_blt_taken.1912:
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -7
	addi	$r5, $r6, 0
	addi	$r3, $r7, 0
	addi	$r6, $r9, 0
	j	min_caml_div_binary_search
div_ble_taken.1911:
	addi	$r3, $r5, 0
	return
div_beq_taken.1909:
	ldi	$r7, $r1, -6
	addi	$r3, $r7, 0
	return
div_blt_taken.1908:
	ldi	$r7, $r1, -6
	add	$r3, $r7, $r8
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -8
	subi	$r1, $r1, 9
	call	min_caml_mul
	addi	$r1, $r1, 9
	ldi	$r5, $r1, -6
	ldi	$r9, $r1, -5
	sub	$r8, $r9, $r5
	ble	$r8, $r30, div_ble_taken.1911
	nop
	nop
	ldi	$r7, $r1, 0
	blt	$r3, $r7, div_blt_taken.1916
	nop
	nop
	beq	$r3, $r7, div_beq_taken.1917
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -8
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_beq_taken.1917:
	ldi	$r6, $r1, -8
	addi	$r3, $r6, 0
	return
div_blt_taken.1916:
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -8
	addi	$r5, $r6, 0
	addi	$r3, $r7, 0
	addi	$r6, $r9, 0
	j	min_caml_div_binary_search
div_ble_taken.1907:
	addi	$r3, $r9, 0
	return
div_beq_taken.1905:
	ldi	$r7, $r1, -5
	addi	$r3, $r7, 0
	return
div_blt_taken.1904:
	ldi	$r7, $r1, -5
	add	$r3, $r7, $r9
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -9
	subi	$r1, $r1, 10
	call	min_caml_mul
	addi	$r1, $r1, 10
	ldi	$r8, $r1, -5
	ldi	$r9, $r1, -2
	sub	$r6, $r9, $r8
	ble	$r6, $r30, div_ble_taken.1919
	nop
	nop
	ldi	$r5, $r1, 0
	blt	$r3, $r5, div_blt_taken.1920
	nop
	nop
	beq	$r3, $r5, div_beq_taken.1921
	nop
	nop
	ldi	$r7, $r1, -9
	add	$r3, $r8, $r7
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -10
	subi	$r1, $r1, 11
	call	min_caml_mul
	addi	$r1, $r1, 11
	ldi	$r5, $r1, -5
	ldi	$r9, $r1, -9
	sub	$r8, $r9, $r5
	ble	$r8, $r30, div_ble_taken.1911
	nop
	nop
	ldi	$r7, $r1, 0
	blt	$r3, $r7, div_blt_taken.1924
	nop
	nop
	beq	$r3, $r7, div_beq_taken.1925
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -10
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_beq_taken.1925:
	ldi	$r6, $r1, -10
	addi	$r3, $r6, 0
	return
div_blt_taken.1924:
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -10
	addi	$r5, $r6, 0
	addi	$r3, $r7, 0
	addi	$r6, $r9, 0
	j	min_caml_div_binary_search
div_beq_taken.1921:
	ldi	$r7, $r1, -9
	addi	$r3, $r7, 0
	return
div_blt_taken.1920:
	ldi	$r7, $r1, -9
	add	$r3, $r7, $r9
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -11
	subi	$r1, $r1, 12
	call	min_caml_mul
	addi	$r1, $r1, 12
	ldi	$r5, $r1, -9
	ldi	$r9, $r1, -2
	sub	$r8, $r9, $r5
	ble	$r8, $r30, div_ble_taken.1911
	nop
	nop
	ldi	$r7, $r1, 0
	blt	$r3, $r7, div_blt_taken.1928
	nop
	nop
	beq	$r3, $r7, div_beq_taken.1929
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -11
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_beq_taken.1929:
	ldi	$r6, $r1, -11
	addi	$r3, $r6, 0
	return
div_blt_taken.1928:
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -11
	addi	$r5, $r6, 0
	addi	$r3, $r7, 0
	addi	$r6, $r9, 0
	j	min_caml_div_binary_search
div_ble_taken.1919:
	addi	$r3, $r8, 0
	return
div_beq_taken.1901:
	ldi	$r8, $r1, -2
	addi	$r3, $r8, 0
	return
div_blt_taken.1900:
	ldi	$r8, $r1, -2
	add	$r3, $r8, $r9
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -12
	subi	$r1, $r1, 13
	call	min_caml_mul
	addi	$r1, $r1, 13
	ldi	$r8, $r1, -2
	ldi	$r9, $r1, -4
	sub	$r6, $r9, $r8
	ble	$r6, $r30, div_ble_taken.1919
	nop
	nop
	ldi	$r5, $r1, 0
	blt	$r3, $r5, div_blt_taken.1932
	nop
	nop
	beq	$r3, $r5, div_beq_taken.1933
	nop
	nop
	ldi	$r7, $r1, -12
	add	$r3, $r8, $r7
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -13
	subi	$r1, $r1, 14
	call	min_caml_mul
	addi	$r1, $r1, 14
	ldi	$r9, $r1, -2
	ldi	$r8, $r1, -12
	sub	$r6, $r8, $r9
	ble	$r6, $r30, div_ble_taken.1907
	nop
	nop
	ldi	$r5, $r1, 0
	blt	$r3, $r5, div_blt_taken.1936
	nop
	nop
	beq	$r3, $r5, div_beq_taken.1937
	nop
	nop
	ldi	$r7, $r1, -13
	add	$r3, $r9, $r7
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -14
	subi	$r1, $r1, 15
	call	min_caml_mul
	addi	$r1, $r1, 15
	ldi	$r5, $r1, -2
	ldi	$r9, $r1, -13
	sub	$r8, $r9, $r5
	ble	$r8, $r30, div_ble_taken.1911
	nop
	nop
	ldi	$r7, $r1, 0
	blt	$r3, $r7, div_blt_taken.1940
	nop
	nop
	beq	$r3, $r7, div_beq_taken.1941
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -14
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_beq_taken.1941:
	ldi	$r6, $r1, -14
	addi	$r3, $r6, 0
	return
div_blt_taken.1940:
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -14
	addi	$r5, $r6, 0
	addi	$r3, $r7, 0
	addi	$r6, $r9, 0
	j	min_caml_div_binary_search
div_beq_taken.1937:
	ldi	$r7, $r1, -13
	addi	$r3, $r7, 0
	return
div_blt_taken.1936:
	ldi	$r7, $r1, -13
	add	$r3, $r7, $r8
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -15
	subi	$r1, $r1, 16
	call	min_caml_mul
	addi	$r1, $r1, 16
	ldi	$r5, $r1, -13
	ldi	$r9, $r1, -12
	sub	$r8, $r9, $r5
	ble	$r8, $r30, div_ble_taken.1911
	nop
	nop
	ldi	$r7, $r1, 0
	blt	$r3, $r7, div_blt_taken.1944
	nop
	nop
	beq	$r3, $r7, div_beq_taken.1945
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -15
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_beq_taken.1945:
	ldi	$r6, $r1, -15
	addi	$r3, $r6, 0
	return
div_blt_taken.1944:
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -15
	addi	$r5, $r6, 0
	addi	$r3, $r7, 0
	addi	$r6, $r9, 0
	j	min_caml_div_binary_search
div_beq_taken.1933:
	ldi	$r7, $r1, -12
	addi	$r3, $r7, 0
	return
div_blt_taken.1932:
	ldi	$r7, $r1, -12
	add	$r3, $r7, $r9
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -16
	subi	$r1, $r1, 17
	call	min_caml_mul
	addi	$r1, $r1, 17
	ldi	$r8, $r1, -12
	ldi	$r9, $r1, -4
	sub	$r6, $r9, $r8
	ble	$r6, $r30, div_ble_taken.1919
	nop
	nop
	ldi	$r5, $r1, 0
	blt	$r3, $r5, div_blt_taken.1948
	nop
	nop
	beq	$r3, $r5, div_beq_taken.1949
	nop
	nop
	ldi	$r7, $r1, -16
	add	$r3, $r8, $r7
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -17
	subi	$r1, $r1, 18
	call	min_caml_mul
	addi	$r1, $r1, 18
	ldi	$r5, $r1, -12
	ldi	$r9, $r1, -16
	sub	$r8, $r9, $r5
	ble	$r8, $r30, div_ble_taken.1911
	nop
	nop
	ldi	$r7, $r1, 0
	blt	$r3, $r7, div_blt_taken.1952
	nop
	nop
	beq	$r3, $r7, div_beq_taken.1953
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -17
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_beq_taken.1953:
	ldi	$r6, $r1, -17
	addi	$r3, $r6, 0
	return
div_blt_taken.1952:
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -17
	addi	$r5, $r6, 0
	addi	$r3, $r7, 0
	addi	$r6, $r9, 0
	j	min_caml_div_binary_search
div_beq_taken.1949:
	ldi	$r7, $r1, -16
	addi	$r3, $r7, 0
	return
div_blt_taken.1948:
	ldi	$r7, $r1, -16
	add	$r3, $r7, $r9
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -18
	subi	$r1, $r1, 19
	call	min_caml_mul
	addi	$r1, $r1, 19
	ldi	$r5, $r1, -16
	ldi	$r9, $r1, -4
	sub	$r8, $r9, $r5
	ble	$r8, $r30, div_ble_taken.1911
	nop
	nop
	ldi	$r7, $r1, 0
	blt	$r3, $r7, div_blt_taken.1956
	nop
	nop
	beq	$r3, $r7, div_beq_taken.1957
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -18
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_beq_taken.1957:
	ldi	$r6, $r1, -18
	addi	$r3, $r6, 0
	return
div_blt_taken.1956:
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -18
	addi	$r5, $r6, 0
	addi	$r3, $r7, 0
	addi	$r6, $r9, 0
	j	min_caml_div_binary_search
div_ble_taken.1899:
	addi	$r3, $r7, 0
	return


#div_sub for min_caml_div
div_sub:
	sti	$r3, $r1, 0
	slli	$r3, $r4, 1
	sti	$r4, $r1, -1
	sti	$r5, $r1, -2
	addi	$r4, $r5, 0
	subi	$r1, $r1, 3
	call	min_caml_mul
	addi	$r1, $r1, 3
	ldi	$r5, $r1, 0
	ble	$r3, $r5, div_ble_taken.1959
	nop
	nop
	ldi	$r7, $r1, -2
	slli	$r3, $r7, 1
	sti	$r3, $r1, -3
	add	$r3, $r7, $r3
	srai	$r3, $r3, 1
	ldi	$r6, $r1, -1
	sti	$r3, $r1, -4
	addi	$r4, $r6, 0
	subi	$r1, $r1, 5
	call	min_caml_mul
	addi	$r1, $r1, 5
	ldi	$r9, $r1, -2
	ldi	$r5, $r1, -3
	sub	$r7, $r5, $r9
	ble	$r7, $r30, div_ble_taken.1907
	nop
	nop
	ldi	$r6, $r1, 0
	blt	$r3, $r6, div_blt_taken.1962
	nop
	nop
	beq	$r3, $r6, div_beq_taken.1963
	nop
	nop
	ldi	$r8, $r1, -4
	add	$r3, $r9, $r8
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -5
	subi	$r1, $r1, 6
	call	min_caml_mul
	addi	$r1, $r1, 6
	ldi	$r9, $r1, -2
	ldi	$r8, $r1, -4
	sub	$r6, $r8, $r9
	ble	$r6, $r30, div_ble_taken.1907
	nop
	nop
	ldi	$r5, $r1, 0
	blt	$r3, $r5, div_blt_taken.1966
	nop
	nop
	beq	$r3, $r5, div_beq_taken.1967
	nop
	nop
	ldi	$r7, $r1, -5
	add	$r3, $r9, $r7
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -6
	subi	$r1, $r1, 7
	call	min_caml_mul
	addi	$r1, $r1, 7
	ldi	$r5, $r1, -2
	ldi	$r9, $r1, -5
	sub	$r8, $r9, $r5
	ble	$r8, $r30, div_ble_taken.1911
	nop
	nop
	ldi	$r7, $r1, 0
	blt	$r3, $r7, div_blt_taken.1970
	nop
	nop
	beq	$r3, $r7, div_beq_taken.1971
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -6
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_beq_taken.1971:
	ldi	$r6, $r1, -6
	addi	$r3, $r6, 0
	return
div_blt_taken.1970:
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -6
	addi	$r5, $r6, 0
	addi	$r3, $r7, 0
	addi	$r6, $r9, 0
	j	min_caml_div_binary_search
div_beq_taken.1967:
	ldi	$r7, $r1, -5
	addi	$r3, $r7, 0
	return
div_blt_taken.1966:
	ldi	$r7, $r1, -5
	add	$r3, $r7, $r8
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -7
	subi	$r1, $r1, 8
	call	min_caml_mul
	addi	$r1, $r1, 8
	ldi	$r5, $r1, -5
	ldi	$r9, $r1, -4
	sub	$r8, $r9, $r5
	ble	$r8, $r30, div_ble_taken.1911
	nop
	nop
	ldi	$r7, $r1, 0
	blt	$r3, $r7, div_blt_taken.1974
	nop
	nop
	beq	$r3, $r7, div_beq_taken.1975
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -7
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_beq_taken.1975:
	ldi	$r6, $r1, -7
	addi	$r3, $r6, 0
	return
div_blt_taken.1974:
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -7
	addi	$r5, $r6, 0
	addi	$r3, $r7, 0
	addi	$r6, $r9, 0
	j	min_caml_div_binary_search
div_beq_taken.1963:
	ldi	$r8, $r1, -4
	addi	$r3, $r8, 0
	return
div_blt_taken.1962:
	ldi	$r8, $r1, -4
	add	$r3, $r8, $r5
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -8
	subi	$r1, $r1, 9
	call	min_caml_mul
	addi	$r1, $r1, 9
	ldi	$r9, $r1, -4
	ldi	$r5, $r1, -3
	sub	$r7, $r5, $r9
	ble	$r7, $r30, div_ble_taken.1907
	nop
	nop
	ldi	$r6, $r1, 0
	blt	$r3, $r6, div_blt_taken.1978
	nop
	nop
	beq	$r3, $r6, div_beq_taken.1979
	nop
	nop
	ldi	$r8, $r1, -8
	add	$r3, $r9, $r8
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -9
	subi	$r1, $r1, 10
	call	min_caml_mul
	addi	$r1, $r1, 10
	ldi	$r5, $r1, -4
	ldi	$r9, $r1, -8
	sub	$r8, $r9, $r5
	ble	$r8, $r30, div_ble_taken.1911
	nop
	nop
	ldi	$r7, $r1, 0
	blt	$r3, $r7, div_blt_taken.1982
	nop
	nop
	beq	$r3, $r7, div_beq_taken.1983
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -9
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_beq_taken.1983:
	ldi	$r6, $r1, -9
	addi	$r3, $r6, 0
	return
div_blt_taken.1982:
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -9
	addi	$r5, $r6, 0
	addi	$r3, $r7, 0
	addi	$r6, $r9, 0
	j	min_caml_div_binary_search
div_beq_taken.1979:
	ldi	$r8, $r1, -8
	addi	$r3, $r8, 0
	return
div_blt_taken.1978:
	ldi	$r8, $r1, -8
	add	$r3, $r8, $r5
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -10
	subi	$r1, $r1, 11
	call	min_caml_mul
	addi	$r1, $r1, 11
	ldi	$r9, $r1, -8
	ldi	$r6, $r1, -3
	sub	$r8, $r6, $r9
	ble	$r8, $r30, div_ble_taken.1907
	nop
	nop
	ldi	$r7, $r1, 0
	blt	$r3, $r7, div_blt_taken.1986
	nop
	nop
	beq	$r3, $r7, div_beq_taken.1987
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r5, $r1, -10
	addi	$r6, $r5, 0
	addi	$r3, $r7, 0
	addi	$r5, $r9, 0
	j	min_caml_div_binary_search
div_beq_taken.1987:
	ldi	$r5, $r1, -10
	addi	$r3, $r5, 0
	return
div_blt_taken.1986:
	ldi	$r4, $r1, -1
	ldi	$r5, $r1, -10
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_ble_taken.1959:
	ldi	$r7, $r1, -2
	slli	$r4, $r7, 1
	ldi	$r6, $r1, -1
	slli	$r3, $r6, 1
	sti	$r4, $r1, -11
	subi	$r1, $r1, 12
	call	min_caml_mul
	addi	$r1, $r1, 12
	ldi	$r6, $r1, 0
	ble	$r3, $r6, div_ble_taken.1989
	nop
	nop
	ldi	$r5, $r1, -11
	slli	$r3, $r5, 1
	sti	$r3, $r1, -12
	add	$r3, $r5, $r3
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -13
	subi	$r1, $r1, 14
	call	min_caml_mul
	addi	$r1, $r1, 14
	ldi	$r5, $r1, -11
	ldi	$r7, $r1, -12
	sub	$r8, $r7, $r5
	ble	$r8, $r30, div_ble_taken.1911
	nop
	nop
	ldi	$r6, $r1, 0
	blt	$r3, $r6, div_blt_taken.1992
	nop
	nop
	beq	$r3, $r6, div_beq_taken.1993
	nop
	nop
	ldi	$r9, $r1, -13
	add	$r3, $r5, $r9
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -14
	subi	$r1, $r1, 15
	call	min_caml_mul
	addi	$r1, $r1, 15
	ldi	$r5, $r1, -11
	ldi	$r9, $r1, -13
	sub	$r8, $r9, $r5
	ble	$r8, $r30, div_ble_taken.1911
	nop
	nop
	ldi	$r7, $r1, 0
	blt	$r3, $r7, div_blt_taken.1996
	nop
	nop
	beq	$r3, $r7, div_beq_taken.1997
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -14
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_beq_taken.1997:
	ldi	$r6, $r1, -14
	addi	$r3, $r6, 0
	return
div_blt_taken.1996:
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -14
	addi	$r5, $r6, 0
	addi	$r3, $r7, 0
	addi	$r6, $r9, 0
	j	min_caml_div_binary_search
div_beq_taken.1993:
	ldi	$r9, $r1, -13
	addi	$r3, $r9, 0
	return
div_blt_taken.1992:
	ldi	$r9, $r1, -13
	add	$r3, $r9, $r7
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -15
	subi	$r1, $r1, 16
	call	min_caml_mul
	addi	$r1, $r1, 16
	ldi	$r9, $r1, -13
	ldi	$r6, $r1, -12
	sub	$r8, $r6, $r9
	ble	$r8, $r30, div_ble_taken.1907
	nop
	nop
	ldi	$r7, $r1, 0
	blt	$r3, $r7, div_blt_taken.2000
	nop
	nop
	beq	$r3, $r7, div_beq_taken.2001
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r5, $r1, -15
	addi	$r6, $r5, 0
	addi	$r3, $r7, 0
	addi	$r5, $r9, 0
	j	min_caml_div_binary_search
div_beq_taken.2001:
	ldi	$r5, $r1, -15
	addi	$r3, $r5, 0
	return
div_blt_taken.2000:
	ldi	$r4, $r1, -1
	ldi	$r5, $r1, -15
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_ble_taken.1989:
	ldi	$r5, $r1, -11
	slli	$r5, $r5, 1
	ldi	$r4, $r1, -1
	slli	$r3, $r4, 1
	sti	$r5, $r1, -16
	addi	$r4, $r5, 0
	subi	$r1, $r1, 17
	call	min_caml_mul
	addi	$r1, $r1, 17
	ldi	$r5, $r1, 0
	ble	$r3, $r5, div_ble_taken.2003
	nop
	nop
	ldi	$r6, $r1, -16
	slli	$r3, $r6, 1
	sti	$r3, $r1, -17
	add	$r3, $r6, $r3
	srai	$r3, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r3, $r1, -18
	subi	$r1, $r1, 19
	call	min_caml_mul
	addi	$r1, $r1, 19
	ldi	$r5, $r1, -16
	ldi	$r6, $r1, -17
	sub	$r8, $r6, $r5
	ble	$r8, $r30, div_ble_taken.1911
	nop
	nop
	ldi	$r7, $r1, 0
	blt	$r3, $r7, div_blt_taken.2006
	nop
	nop
	beq	$r3, $r7, div_beq_taken.2007
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r9, $r1, -18
	addi	$r6, $r9, 0
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_beq_taken.2007:
	ldi	$r9, $r1, -18
	addi	$r3, $r9, 0
	return
div_blt_taken.2006:
	ldi	$r4, $r1, -1
	ldi	$r9, $r1, -18
	addi	$r5, $r9, 0
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_ble_taken.2003:
	ldi	$r6, $r1, -16
	slli	$r5, $r6, 1
	ldi	$r4, $r1, -1
	slli	$r3, $r4, 1
	sti	$r5, $r1, -19
	addi	$r4, $r5, 0
	subi	$r1, $r1, 20
	call	min_caml_mul
	addi	$r1, $r1, 20
	ldi	$r7, $r1, 0
	ble	$r3, $r7, div_ble_taken.2009
	nop
	nop
	ldi	$r5, $r1, -19
	slli	$r6, $r5, 1
	ldi	$r4, $r1, -1
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_ble_taken.2009:
	ldi	$r5, $r1, -19
	slli	$r5, $r5, 1
	ldi	$r4, $r1, -1
	addi	$r3, $r7, 0
	j	div_sub


# $r3: devidee $r4: devider
min_caml_div:
	beq $r4, $r0, zero_div
	nop
	nop
	# when a is larger than 0x40000000, it is possible to overflow
	# round answer
	# 0x3fff ffff
	addi $r5, $r0, 16384
	slli $r5, $r5, 16
	blt $r3, $r5, start_div
	nop
	nop
	srai $r3, $r3, 1
	srai $r4, $r4, 1
start_div:
	sti	$r3, $r1, 0
	sti	$r4, $r1, -1
	add	$r3, $r3, $r0
	add	$r4, $r4, $r0
	blt	$r0, $r3, skip_invert_devidee
	nop
	nop
	sub $r3, $r0, $r3
skip_invert_devidee:
	blt $r0, $r4, skip_invert_devider
	nop
	nop
	sub $r4, $r0, $r4
skip_invert_devider:
	blt	$r3, $r4, zero_div
	nop
	nop
	addi	$r5, $r0, 1
	subi	$r1, $r1, 2
	call div_sub
	addi	$r1, $r1, 2
	ldi	$r5, $r1, 0
	ldi	$r6, $r1, -1
	# fix sign
	blt $r5, $r0, negative_devidee
	nop
	nop
	# positive_devidee
	blt $r6, $r0, ans_invert
	nop
	nop
	j ans_direct
negative_devidee:
	blt $r6, $r0, ans_direct
	nop
	nop
	j ans_invert
ans_invert:
	sub	$r3, $r0, $r3
ans_direct:
	return
# also used when a < b
zero_div:
	addi $r3, $r0, 0
	return

# display 命令を呼び直す
# display y x char
min_caml_display:
	# 80 = 64 + 16
	slli $r6, $r3, 6
	slli $r7, $r3, 4
	add $r6, $r6, $r7
	add $r6, $r6, $r4
	display $r6, $r5
	return

min_caml_readkbd:
	readkey $r3
	return

min_caml_clear_display:
	debug 8
	addi $r3, $r0, 0
	addi $r4, $r0, 2400
clear_display_start:
	display $r3, $r0
	addi $r3, $r3, 1
	blt $r3, $r4, clear_display_start
	nop
	nop
	return

min_caml_draw:
	display $r3, $r3
	return
