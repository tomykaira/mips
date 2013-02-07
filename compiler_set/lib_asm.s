
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

# * array_init
min_caml_array_init:
	add	$r4, $r3, $r4
ARRAY_INIT_LOOP:
	blt	$r3, $r4, ARRAY_INIT_CONTINUE
	nop
	nop
	return
ARRAY_INIT_CONTINUE:
	sti	$r5, $r3, 0
	addi	$r3, $r3, 1
	j	ARRAY_INIT_LOOP

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

# * float_array_init
min_caml_float_array_init:
	add	$r4, $r3, $r4
FLOAT_ARRAY_INIT_LOOP:
	blt	$r3, $r4, FLOAT_ARRAY_INIT_CONTINUE
	nop
	nop
	return
FLOAT_ARRAY_INIT_CONTINUE:	
	fsti	$f1, $r3, 0
	addi	$r3, $r3, 1
	j	FLOAT_ARRAY_INIT_LOOP

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

# * int_tuple_array_init
min_caml_int_tuple_array_init:
	ble	$r6, $r4, INT_TUPLE_ARRAY_RETURN
	nop
	nop
	sti	$r3, $r4, 0
	add	$r4, $r4, $r5
	j	min_caml_int_tuple_array_init
INT_TUPLE_ARRAY_INIT_RETURN:
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

# * float_tuple_array_init
min_caml_float_tuple_array_init:
	ble	$r5, $r3, FLOAT_TUPLE_ARRAY_RETURN
	nop
	nop
	fsti	$f1, $r3, 0
	add	$r3, $r3, $r4
	j	min_caml_float_tuple_array_init
FLOAT_TUPLE_ARRAY_INIT_RETURN:
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
	
	#nanか判定?
	#if $f1 > 2^31, return 0
	fmvhi $f6, 20352
	fblt $f1, $f6, FTOI_CONT
	nop
	nop
	fadd $f1, $f0, $f0
	return
FTOI_CONT:	
	
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

	# deal with very large/small number
	# $f6 <- 2^31
	fmvhi $f6, 20352
	fble $f6, $f1, exp_pos_overflow
	nop
	nop
	# $f6 <- -2^31
	fmvhi $f6, 53120
	fble $f1, $f6, exp_neg_overflow
	nop
	nop
	
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
exp_pos_overflow:
	addi $r3, $r0, 255
	slli $r3, $r3, 23
	imovf $f1, $r3
	return
exp_neg_overflow:
	imovf $f1, $r0
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
	beq	$r4, $r0, div_beq_taken.6783
	nop
	nop
	srai	$r5, $r4, 1
	slli	$r6, $r5, 1
	sub	$r4, $r4, $r6
	beq	$r4, $r0, div_beq_taken.6777
	nop
	nop
	slli	$r6, $r3, 1
	beq	$r5, $r0, div_return.7189
	nop
	nop
	srai	$r4, $r5, 1
	sti	$r3, $r1, 0
	slli	$r3, $r4, 1
	sub	$r3, $r5, $r3
	beq	$r3, $r0, div_beq_taken.6779
	nop
	nop
	slli	$r3, $r6, 1
	sti	$r6, $r1, -1
	subi	$r1, $r1, 2
	call	mul_sub
	addi	$r1, $r1, 2
	ldi	$r6, $r1, -1
	add	$r27, $r3, $r6
	j	div_beq_cont.6780
div_beq_taken.6779:
	slli	$r3, $r6, 1
	subi	$r1, $r1, 2
	call	mul_sub
	addi	$r1, $r1, 2
	addi	$r27, $r3, 0
div_beq_cont.6780:
	ldi	$r3, $r1, 0
	add	$r3, $r27, $r3
	return
div_return.7189:
	return
div_beq_taken.6777:
	slli	$r3, $r3, 1
	beq	$r5, $r0, div_beq_taken.6783
	nop
	nop
	srai	$r4, $r5, 1
	slli	$r6, $r4, 1
	sub	$r5, $r5, $r6
	beq	$r5, $r0, div_beq_taken.6784
	nop
	nop
	sti	$r3, $r1, -2
	slli	$r3, $r3, 1
	subi	$r1, $r1, 3
	call	mul_sub
	addi	$r1, $r1, 3
	ldi	$r4, $r1, -2
	add	$r3, $r3, $r4
	return
div_beq_taken.6784:
	slli	$r3, $r3, 1
	j	mul_sub
div_beq_taken.6783:
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
	blt	$r4, $r0, div_blt_taken.6786
	nop
	nop
	subi	$r1, $r1, 5
	call	mul_sub
	addi	$r1, $r1, 5
	addi	$r27, $r3, 0
	j	div_blt_cont.6787
div_blt_taken.6786:
	sub	$r3, $r0, $r3
	sub	$r4, $r0, $r4
	subi	$r1, $r1, 5
	call	mul_sub
	addi	$r1, $r1, 5
	addi	$r27, $r3, 0
div_blt_cont.6787:
	ldi	$r3, $r1, -3
	ldi	$r8, $r1, -4
	sub	$r4, $r8, $r3
	ble	$r4, $r30, div_return.7189
	nop
	nop
	ldi	$r5, $r1, 0
	blt	$r27, $r5, div_blt_taken.6791
	nop
	nop
	beq	$r27, $r5, div_beq_taken.6792
	nop
	nop
	ldi	$r7, $r1, -2
	add	$r3, $r3, $r7
	srai	$r5, $r3, 1
	ldi	$r6, $r1, -1
	sti	$r5, $r1, -5
	blt	$r6, $r0, div_blt_taken.6793
	nop
	nop
	beq	$r6, $r0, div_beq_taken.6795
	nop
	nop
	srai	$r4, $r6, 1
	slli	$r3, $r4, 1
	sub	$r3, $r6, $r3
	beq	$r3, $r0, div_beq_taken.6797
	nop
	nop
	slli	$r3, $r5, 1
	subi	$r1, $r1, 6
	call	mul_sub
	addi	$r1, $r1, 6
	ldi	$r6, $r1, -5
	add	$r27, $r3, $r6
	j	div_blt_cont.6794
div_beq_taken.6797:
	slli	$r3, $r5, 1
	subi	$r1, $r1, 6
	call	mul_sub
	addi	$r1, $r1, 6
	addi	$r27, $r3, 0
	j	div_blt_cont.6794
div_beq_taken.6795:
	addi	$r27, $r0, 0
	j	div_blt_cont.6794
div_blt_taken.6793:
	sub	$r3, $r0, $r5
	sub	$r5, $r0, $r6
	beq	$r5, $r0, div_beq_taken.6795
	nop
	nop
	srai	$r4, $r5, 1
	slli	$r6, $r4, 1
	sub	$r5, $r5, $r6
	beq	$r5, $r0, div_beq_taken.6803
	nop
	nop
	sti	$r3, $r1, -6
	slli	$r3, $r3, 1
	subi	$r1, $r1, 7
	call	mul_sub
	addi	$r1, $r1, 7
	ldi	$r8, $r1, -6
	add	$r27, $r3, $r8
	j	div_blt_cont.6794
div_beq_taken.6803:
	slli	$r3, $r3, 1
	subi	$r1, $r1, 7
	call	mul_sub
	addi	$r1, $r1, 7
	addi	$r27, $r3, 0
div_blt_cont.6794:
	ldi	$r5, $r1, -3
	ldi	$r8, $r1, -2
	sub	$r7, $r8, $r5
	ble	$r7, $r30, div_ble_taken.6807
	nop
	nop
	ldi	$r3, $r1, 0
	blt	$r27, $r3, div_blt_taken.6808
	nop
	nop
	beq	$r27, $r3, div_beq_taken.6809
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -5
	j	min_caml_div_binary_search
div_beq_taken.6809:
	ldi	$r6, $r1, -5
	addi	$r3, $r6, 0
	return
div_blt_taken.6808:
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -5
	addi	$r5, $r6, 0
	addi	$r6, $r8, 0
	j	min_caml_div_binary_search
div_ble_taken.6807:
	addi	$r3, $r5, 0
	return
div_beq_taken.6792:
	ldi	$r7, $r1, -2
	addi	$r3, $r7, 0
	return
div_blt_taken.6791:
	ldi	$r7, $r1, -2
	add	$r3, $r7, $r8
	srai	$r5, $r3, 1
	ldi	$r6, $r1, -1
	sti	$r5, $r1, -7
	blt	$r6, $r0, div_blt_taken.6810
	nop
	nop
	beq	$r6, $r0, div_beq_taken.6812
	nop
	nop
	srai	$r4, $r6, 1
	slli	$r3, $r4, 1
	sub	$r3, $r6, $r3
	beq	$r3, $r0, div_beq_taken.6814
	nop
	nop
	slli	$r3, $r5, 1
	subi	$r1, $r1, 8
	call	mul_sub
	addi	$r1, $r1, 8
	ldi	$r5, $r1, -7
	add	$r27, $r3, $r5
	j	div_blt_cont.6811
div_beq_taken.6814:
	slli	$r3, $r5, 1
	subi	$r1, $r1, 8
	call	mul_sub
	addi	$r1, $r1, 8
	addi	$r27, $r3, 0
	j	div_blt_cont.6811
div_beq_taken.6812:
	addi	$r27, $r0, 0
	j	div_blt_cont.6811
div_blt_taken.6810:
	sub	$r3, $r0, $r5
	sub	$r5, $r0, $r6
	beq	$r5, $r0, div_beq_taken.6812
	nop
	nop
	srai	$r4, $r5, 1
	slli	$r6, $r4, 1
	sub	$r5, $r5, $r6
	beq	$r5, $r0, div_beq_taken.6820
	nop
	nop
	sti	$r3, $r1, -8
	slli	$r3, $r3, 1
	subi	$r1, $r1, 9
	call	mul_sub
	addi	$r1, $r1, 9
	ldi	$r10, $r1, -8
	add	$r27, $r3, $r10
	j	div_blt_cont.6811
div_beq_taken.6820:
	slli	$r3, $r3, 1
	subi	$r1, $r1, 9
	call	mul_sub
	addi	$r1, $r1, 9
	addi	$r27, $r3, 0
div_blt_cont.6811:
	ldi	$r5, $r1, -2
	ldi	$r8, $r1, -4
	sub	$r7, $r8, $r5
	ble	$r7, $r30, div_ble_taken.6807
	nop
	nop
	ldi	$r3, $r1, 0
	blt	$r27, $r3, div_blt_taken.6825
	nop
	nop
	beq	$r27, $r3, div_beq_taken.6826
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -7
	j	min_caml_div_binary_search
div_beq_taken.6826:
	ldi	$r6, $r1, -7
	addi	$r3, $r6, 0
	return
div_blt_taken.6825:
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -7
	addi	$r5, $r6, 0
	addi	$r6, $r8, 0
	j	min_caml_div_binary_search

	



#div_sub for min_caml_div
div_sub:
	sti	$r3, $r1, 0
	slli	$r3, $r4, 1
	sti	$r4, $r1, -1
	addi	$r4, $r0, 0
	sti	$r4, $r1, -2
	sti	$r3, $r1, -3
	sti	$r5, $r1, -4
	blt	$r5, $r0, div_blt_taken.6827
	nop
	nop
	addi	$r4, $r5, 0
	subi	$r1, $r1, 5
	call	mul_sub
	addi	$r1, $r1, 5
	addi	$r27, $r3, 0
	j	div_blt_cont.6828
div_blt_taken.6827:
	sub	$r3, $r0, $r3
	sub	$r4, $r0, $r5
	subi	$r1, $r1, 5
	call	mul_sub
	addi	$r1, $r1, 5
	addi	$r27, $r3, 0
div_blt_cont.6828:
	ldi	$r7, $r1, 0
	ble	$r27, $r7, div_ble_taken.6831
	nop
	nop
	ldi	$r8, $r1, -4
	slli	$r3, $r8, 1
	sti	$r3, $r1, -5
	add	$r3, $r8, $r3
	srai	$r6, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r6, $r1, -6
	blt	$r4, $r0, div_blt_taken.6832
	nop
	nop
	beq	$r4, $r0, div_beq_taken.6834
	nop
	nop
	srai	$r5, $r4, 1
	slli	$r3, $r5, 1
	sub	$r3, $r4, $r3
	beq	$r3, $r0, div_beq_taken.6836
	nop
	nop
	slli	$r3, $r6, 1
	addi	$r4, $r5, 0
	subi	$r1, $r1, 7
	call	mul_sub
	addi	$r1, $r1, 7
	ldi	$r5, $r1, -6
	add	$r27, $r3, $r5
	j	div_blt_cont.6833
div_beq_taken.6836:
	slli	$r3, $r6, 1
	addi	$r4, $r5, 0
	subi	$r1, $r1, 7
	call	mul_sub
	addi	$r1, $r1, 7
	addi	$r27, $r3, 0
	j	div_blt_cont.6833
div_beq_taken.6834:
	addi	$r27, $r0, 0
	j	div_blt_cont.6833
div_blt_taken.6832:
	sub	$r3, $r0, $r6
	sub	$r5, $r0, $r4
	beq	$r5, $r0, div_beq_taken.6834
	nop
	nop
	srai	$r4, $r5, 1
	slli	$r6, $r4, 1
	sub	$r5, $r5, $r6
	beq	$r5, $r0, div_beq_taken.6842
	nop
	nop
	sti	$r3, $r1, -7
	slli	$r3, $r3, 1
	subi	$r1, $r1, 8
	call	mul_sub
	addi	$r1, $r1, 8
	ldi	$r9, $r1, -7
	add	$r27, $r3, $r9
	j	div_blt_cont.6833
div_beq_taken.6842:
	slli	$r3, $r3, 1
	subi	$r1, $r1, 8
	call	mul_sub
	addi	$r1, $r1, 8
	addi	$r27, $r3, 0
div_blt_cont.6833:
	ldi	$r8, $r1, -4
	ldi	$r6, $r1, -5
	sub	$r7, $r6, $r8
	ble	$r7, $r30, div_ble_taken.6846
	nop
	nop
	ldi	$r3, $r1, 0
	blt	$r27, $r3, div_blt_taken.6847
	nop
	nop
	beq	$r27, $r3, div_beq_taken.6848
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r5, $r1, -6
	addi	$r6, $r5, 0
	addi	$r5, $r8, 0
	j	min_caml_div_binary_search
div_beq_taken.6848:
	ldi	$r5, $r1, -6
	addi	$r3, $r5, 0
	return
div_blt_taken.6847:
	ldi	$r4, $r1, -1
	ldi	$r5, $r1, -6
	j	min_caml_div_binary_search
div_ble_taken.6846:
	addi	$r3, $r8, 0
	return
div_ble_taken.6831:
	ldi	$r8, $r1, -4
	slli	$r5, $r8, 1
	blt	$r5, $r0, div_blt_taken.6849
	nop
	nop
	beq	$r5, $r0, div_beq_taken.6850
	nop
	nop
	srai	$r4, $r5, 1
	slli	$r6, $r4, 1
	sti	$r5, $r1, -8
	sub	$r5, $r5, $r6
	beq	$r5, $r0, div_beq_taken.6851
	nop
	nop
	ldi	$r3, $r1, -3
	slli	$r3, $r3, 1
	subi	$r1, $r1, 9
	call	mul_sub
	addi	$r1, $r1, 9
	ldi	$r6, $r1, -3
	add	$r27, $r3, $r6
	j	div_beq_cont.6852
div_beq_taken.6851:
	ldi	$r3, $r1, -3
	slli	$r3, $r3, 1
	subi	$r1, $r1, 9
	call	mul_sub
	addi	$r1, $r1, 9
	addi	$r27, $r3, 0
div_beq_cont.6852:
	ldi	$r7, $r1, 0
	ble	$r27, $r7, div_ble_taken.6855
	nop
	nop
	ldi	$r5, $r1, -8
	slli	$r6, $r5, 1
	ldi	$r4, $r1, -1
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_ble_taken.6855:
	ldi	$r5, $r1, -8
	slli	$r4, $r5, 1
	sti	$r4, $r1, -9
	blt	$r4, $r0, div_blt_taken.6856
	nop
	nop
	ldi	$r3, $r1, -3
	subi	$r1, $r1, 10
	call	mul_sub
	addi	$r1, $r1, 10
	addi	$r27, $r3, 0
	j	div_blt_cont.6857
div_blt_taken.6856:
	ldi	$r3, $r1, -3
	sub	$r3, $r0, $r3
	sub	$r4, $r0, $r4
	subi	$r1, $r1, 10
	call	mul_sub
	addi	$r1, $r1, 10
	addi	$r27, $r3, 0
div_blt_cont.6857:
	ldi	$r3, $r1, 0
	ble	$r27, $r3, div_ble_taken.6860
	nop
	nop
	ldi	$r8, $r1, -9
	slli	$r3, $r8, 1
	sti	$r3, $r1, -10
	add	$r3, $r8, $r3
	srai	$r6, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r6, $r1, -11
	blt	$r4, $r0, div_blt_taken.6861
	nop
	nop
	beq	$r4, $r0, div_beq_taken.6863
	nop
	nop
	srai	$r5, $r4, 1
	slli	$r3, $r5, 1
	sub	$r3, $r4, $r3
	beq	$r3, $r0, div_beq_taken.6865
	nop
	nop
	slli	$r3, $r6, 1
	addi	$r4, $r5, 0
	subi	$r1, $r1, 12
	call	mul_sub
	addi	$r1, $r1, 12
	ldi	$r5, $r1, -11
	add	$r27, $r3, $r5
	j	div_blt_cont.6862
div_beq_taken.6865:
	slli	$r3, $r6, 1
	addi	$r4, $r5, 0
	subi	$r1, $r1, 12
	call	mul_sub
	addi	$r1, $r1, 12
	addi	$r27, $r3, 0
	j	div_blt_cont.6862
div_beq_taken.6863:
	addi	$r27, $r0, 0
	j	div_blt_cont.6862
div_blt_taken.6861:
	sub	$r3, $r0, $r6
	sub	$r5, $r0, $r4
	beq	$r5, $r0, div_beq_taken.6863
	nop
	nop
	srai	$r4, $r5, 1
	slli	$r6, $r4, 1
	sub	$r5, $r5, $r6
	beq	$r5, $r0, div_beq_taken.6871
	nop
	nop
	sti	$r3, $r1, -12
	slli	$r3, $r3, 1
	subi	$r1, $r1, 13
	call	mul_sub
	addi	$r1, $r1, 13
	ldi	$r9, $r1, -12
	add	$r27, $r3, $r9
	j	div_blt_cont.6862
div_beq_taken.6871:
	slli	$r3, $r3, 1
	subi	$r1, $r1, 13
	call	mul_sub
	addi	$r1, $r1, 13
	addi	$r27, $r3, 0
div_blt_cont.6862:
	ldi	$r5, $r1, -9
	ldi	$r6, $r1, -10
	sub	$r7, $r6, $r5
	ble	$r7, $r30, div_ble_taken.6807
	nop
	nop
	ldi	$r3, $r1, 0
	blt	$r27, $r3, div_blt_taken.6876
	nop
	nop
	beq	$r27, $r3, div_beq_taken.6877
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r8, $r1, -11
	addi	$r6, $r8, 0
	j	min_caml_div_binary_search
div_beq_taken.6877:
	ldi	$r8, $r1, -11
	addi	$r3, $r8, 0
	return
div_blt_taken.6876:
	ldi	$r4, $r1, -1
	ldi	$r8, $r1, -11
	addi	$r5, $r8, 0
	j	min_caml_div_binary_search
div_ble_taken.6860:
	ldi	$r8, $r1, -9
	slli	$r5, $r8, 1
	blt	$r5, $r0, div_blt_taken.6878
	nop
	nop
	beq	$r5, $r0, div_beq_taken.6879
	nop
	nop
	srai	$r4, $r5, 1
	slli	$r3, $r4, 1
	sub	$r3, $r5, $r3
	sti	$r5, $r1, -13
	beq	$r3, $r0, div_beq_taken.6880
	nop
	nop
	ldi	$r7, $r1, -3
	slli	$r3, $r7, 1
	subi	$r1, $r1, 14
	call	mul_sub
	addi	$r1, $r1, 14
	ldi	$r6, $r1, -3
	add	$r27, $r3, $r6
	j	div_beq_cont.6881
div_beq_taken.6880:
	ldi	$r7, $r1, -3
	slli	$r3, $r7, 1
	subi	$r1, $r1, 14
	call	mul_sub
	addi	$r1, $r1, 14
	addi	$r27, $r3, 0
div_beq_cont.6881:
	ldi	$r3, $r1, 0
	ble	$r27, $r3, div_ble_taken.6884
	nop
	nop
	ldi	$r5, $r1, -13
	slli	$r6, $r5, 1
	ldi	$r4, $r1, -1
	j	min_caml_div_binary_search
div_ble_taken.6884:
	ldi	$r5, $r1, -13
	slli	$r5, $r5, 1
	ldi	$r4, $r1, -1
	j	div_sub
div_beq_taken.6879:
	ble	$r0, $r3, div_ble_taken.6885
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -2
	addi	$r5, $r6, 0
	j	min_caml_div_binary_search
div_ble_taken.6885:
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -2
	addi	$r5, $r6, 0
	j	div_sub
div_blt_taken.6878:
	ldi	$r7, $r1, -3
	sub	$r6, $r0, $r7
	sub	$r7, $r0, $r5
	beq	$r7, $r0, div_beq_taken.6886
	nop
	nop
	srai	$r4, $r7, 1
	slli	$r3, $r4, 1
	sub	$r3, $r7, $r3
	sti	$r5, $r1, -13
	beq	$r3, $r0, div_beq_taken.6887
	nop
	nop
	slli	$r3, $r6, 1
	sti	$r6, $r1, -14
	subi	$r1, $r1, 15
	call	mul_sub
	addi	$r1, $r1, 15
	ldi	$r6, $r1, -14
	add	$r27, $r3, $r6
	j	div_beq_cont.6888
div_beq_taken.6887:
	slli	$r3, $r6, 1
	subi	$r1, $r1, 15
	call	mul_sub
	addi	$r1, $r1, 15
	addi	$r27, $r3, 0
div_beq_cont.6888:
	ldi	$r3, $r1, 0
	ble	$r27, $r3, div_ble_taken.6891
	nop
	nop
	ldi	$r5, $r1, -13
	slli	$r6, $r5, 1
	ldi	$r4, $r1, -1
	j	min_caml_div_binary_search
div_ble_taken.6891:
	ldi	$r5, $r1, -13
	slli	$r5, $r5, 1
	ldi	$r4, $r1, -1
	j	div_sub
div_beq_taken.6886:
	ble	$r0, $r3, div_ble_taken.6892
	nop
	nop
	slli	$r6, $r5, 1
	ldi	$r4, $r1, -1
	j	min_caml_div_binary_search
div_ble_taken.6892:
	slli	$r5, $r5, 1
	ldi	$r4, $r1, -1
	j	div_sub
div_beq_taken.6850:
	ble	$r0, $r7, div_ble_taken.6893
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -2
	addi	$r5, $r6, 0
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_ble_taken.6893:
	ldi	$r3, $r1, -3
	ldi	$r6, $r1, -2
	addi	$r4, $r6, 0
	subi	$r1, $r1, 15
	call	mul_sub
	addi	$r1, $r1, 15
	ldi	$r7, $r1, 0
	ble	$r3, $r7, div_ble_taken.6895
	nop
	nop
	ldi	$r4, $r1, -1
	blt	$r4, $r0, div_blt_taken.6896
	nop
	nop
	beq	$r4, $r0, div_beq_taken.6783
	nop
	nop
	addi	$r3, $r0, 0
	return
div_blt_taken.6896:
	sub	$r3, $r0, $r4
	beq	$r3, $r0, div_beq_taken.6783
	nop
	nop
	addi	$r3, $r0, 0
	return
div_ble_taken.6895:
	addi	$r5, $r0, 0
	ble	$r0, $r7, div_ble_taken.6899
	nop
	nop
	ldi	$r4, $r1, -1
	addi	$r6, $r5, 0
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_ble_taken.6899:
	ldi	$r4, $r1, -1
	addi	$r3, $r7, 0
	j	div_sub
div_blt_taken.6849:
	ldi	$r3, $r1, -3
	sub	$r6, $r0, $r3
	sub	$r8, $r0, $r5
	beq	$r8, $r0, div_beq_taken.6900
	nop
	nop
	srai	$r4, $r8, 1
	sti	$r5, $r1, -8
	slli	$r5, $r4, 1
	sub	$r5, $r8, $r5
	sti	$r6, $r1, -15
	beq	$r5, $r0, div_beq_taken.6901
	nop
	nop
	slli	$r3, $r6, 1
	subi	$r1, $r1, 16
	call	mul_sub
	addi	$r1, $r1, 16
	ldi	$r8, $r1, -15
	add	$r27, $r3, $r8
	j	div_beq_cont.6902
div_beq_taken.6901:
	slli	$r3, $r6, 1
	subi	$r1, $r1, 16
	call	mul_sub
	addi	$r1, $r1, 16
	addi	$r27, $r3, 0
div_beq_cont.6902:
	ldi	$r7, $r1, 0
	ble	$r27, $r7, div_ble_taken.6905
	nop
	nop
	ldi	$r5, $r1, -8
	slli	$r6, $r5, 1
	ldi	$r4, $r1, -1
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_ble_taken.6905:
	ldi	$r5, $r1, -8
	slli	$r4, $r5, 1
	sti	$r4, $r1, -16
	blt	$r4, $r0, div_blt_taken.6906
	nop
	nop
	ldi	$r3, $r1, -3
	subi	$r1, $r1, 17
	call	mul_sub
	addi	$r1, $r1, 17
	addi	$r27, $r3, 0
	j	div_blt_cont.6907
div_blt_taken.6906:
	sub	$r4, $r0, $r4
	ldi	$r6, $r1, -15
	addi	$r3, $r6, 0
	subi	$r1, $r1, 17
	call	mul_sub
	addi	$r1, $r1, 17
	addi	$r27, $r3, 0
div_blt_cont.6907:
	ldi	$r3, $r1, 0
	ble	$r27, $r3, div_ble_taken.6910
	nop
	nop
	ldi	$r9, $r1, -16
	slli	$r3, $r9, 1
	sti	$r3, $r1, -17
	add	$r3, $r9, $r3
	srai	$r6, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r6, $r1, -18
	blt	$r4, $r0, div_blt_taken.6911
	nop
	nop
	beq	$r4, $r0, div_beq_taken.6913
	nop
	nop
	srai	$r5, $r4, 1
	slli	$r3, $r5, 1
	sub	$r3, $r4, $r3
	beq	$r3, $r0, div_beq_taken.6915
	nop
	nop
	slli	$r3, $r6, 1
	addi	$r4, $r5, 0
	subi	$r1, $r1, 19
	call	mul_sub
	addi	$r1, $r1, 19
	ldi	$r5, $r1, -18
	add	$r27, $r3, $r5
	j	div_blt_cont.6912
div_beq_taken.6915:
	slli	$r3, $r6, 1
	addi	$r4, $r5, 0
	subi	$r1, $r1, 19
	call	mul_sub
	addi	$r1, $r1, 19
	addi	$r27, $r3, 0
	j	div_blt_cont.6912
div_beq_taken.6913:
	addi	$r27, $r0, 0
	j	div_blt_cont.6912
div_blt_taken.6911:
	sub	$r3, $r0, $r6
	sub	$r5, $r0, $r4
	beq	$r5, $r0, div_beq_taken.6913
	nop
	nop
	srai	$r4, $r5, 1
	slli	$r6, $r4, 1
	sub	$r5, $r5, $r6
	beq	$r5, $r0, div_beq_taken.6921
	nop
	nop
	sti	$r3, $r1, -19
	slli	$r3, $r3, 1
	subi	$r1, $r1, 20
	call	mul_sub
	addi	$r1, $r1, 20
	ldi	$r8, $r1, -19
	add	$r27, $r3, $r8
	j	div_blt_cont.6912
div_beq_taken.6921:
	slli	$r3, $r3, 1
	subi	$r1, $r1, 20
	call	mul_sub
	addi	$r1, $r1, 20
	addi	$r27, $r3, 0
div_blt_cont.6912:
	ldi	$r5, $r1, -16
	ldi	$r6, $r1, -17
	sub	$r7, $r6, $r5
	ble	$r7, $r30, div_ble_taken.6807
	nop
	nop
	ldi	$r3, $r1, 0
	blt	$r27, $r3, div_blt_taken.6926
	nop
	nop
	beq	$r27, $r3, div_beq_taken.6927
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r8, $r1, -18
	addi	$r6, $r8, 0
	j	min_caml_div_binary_search
div_beq_taken.6927:
	ldi	$r8, $r1, -18
	addi	$r3, $r8, 0
	return
div_blt_taken.6926:
	ldi	$r4, $r1, -1
	ldi	$r8, $r1, -18
	addi	$r5, $r8, 0
	j	min_caml_div_binary_search
div_ble_taken.6910:
	ldi	$r9, $r1, -16
	slli	$r5, $r9, 1
	blt	$r5, $r0, div_blt_taken.6928
	nop
	nop
	beq	$r5, $r0, div_beq_taken.6929
	nop
	nop
	srai	$r4, $r5, 1
	slli	$r3, $r4, 1
	sub	$r3, $r5, $r3
	sti	$r5, $r1, -20
	beq	$r3, $r0, div_beq_taken.6930
	nop
	nop
	ldi	$r7, $r1, -3
	slli	$r3, $r7, 1
	subi	$r1, $r1, 21
	call	mul_sub
	addi	$r1, $r1, 21
	ldi	$r6, $r1, -3
	add	$r27, $r3, $r6
	j	div_beq_cont.6931
div_beq_taken.6930:
	ldi	$r7, $r1, -3
	slli	$r3, $r7, 1
	subi	$r1, $r1, 21
	call	mul_sub
	addi	$r1, $r1, 21
	addi	$r27, $r3, 0
div_beq_cont.6931:
	ldi	$r3, $r1, 0
	ble	$r27, $r3, div_ble_taken.6934
	nop
	nop
	ldi	$r5, $r1, -20
	slli	$r6, $r5, 1
	ldi	$r4, $r1, -1
	j	min_caml_div_binary_search
div_ble_taken.6934:
	ldi	$r5, $r1, -20
	slli	$r5, $r5, 1
	ldi	$r4, $r1, -1
	j	div_sub
div_beq_taken.6929:
	ble	$r0, $r3, div_ble_taken.6935
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -2
	addi	$r5, $r6, 0
	j	min_caml_div_binary_search
div_ble_taken.6935:
	ldi	$r4, $r1, -1
	ldi	$r6, $r1, -2
	addi	$r5, $r6, 0
	j	div_sub
div_blt_taken.6928:
	sub	$r6, $r0, $r5
	beq	$r6, $r0, div_beq_taken.6936
	nop
	nop
	srai	$r4, $r6, 1
	slli	$r3, $r4, 1
	sub	$r3, $r6, $r3
	sti	$r5, $r1, -20
	beq	$r3, $r0, div_beq_taken.6937
	nop
	nop
	ldi	$r8, $r1, -15
	slli	$r3, $r8, 1
	subi	$r1, $r1, 21
	call	mul_sub
	addi	$r1, $r1, 21
	ldi	$r6, $r1, -15
	add	$r27, $r3, $r6
	j	div_beq_cont.6938
div_beq_taken.6937:
	ldi	$r8, $r1, -15
	slli	$r3, $r8, 1
	subi	$r1, $r1, 21
	call	mul_sub
	addi	$r1, $r1, 21
	addi	$r27, $r3, 0
div_beq_cont.6938:
	ldi	$r3, $r1, 0
	ble	$r27, $r3, div_ble_taken.6941
	nop
	nop
	ldi	$r5, $r1, -20
	slli	$r6, $r5, 1
	ldi	$r4, $r1, -1
	j	min_caml_div_binary_search
div_ble_taken.6941:
	ldi	$r5, $r1, -20
	slli	$r5, $r5, 1
	ldi	$r4, $r1, -1
	j	div_sub
div_beq_taken.6936:
	ble	$r0, $r3, div_ble_taken.6942
	nop
	nop
	slli	$r6, $r5, 1
	ldi	$r4, $r1, -1
	j	min_caml_div_binary_search
div_ble_taken.6942:
	slli	$r5, $r5, 1
	ldi	$r4, $r1, -1
	j	div_sub
div_beq_taken.6900:
	ble	$r0, $r7, div_ble_taken.6943
	nop
	nop
	slli	$r6, $r5, 1
	ldi	$r4, $r1, -1
	addi	$r3, $r7, 0
	j	min_caml_div_binary_search
div_ble_taken.6943:
	slli	$r4, $r5, 1
	sti	$r6, $r1, -15
	sti	$r4, $r1, -21
	blt	$r4, $r0, div_blt_taken.6944
	nop
	nop
	subi	$r1, $r1, 22
	call	mul_sub
	addi	$r1, $r1, 22
	addi	$r27, $r3, 0
	j	div_blt_cont.6945
div_blt_taken.6944:
	sub	$r4, $r0, $r4
	addi	$r3, $r6, 0
	subi	$r1, $r1, 22
	call	mul_sub
	addi	$r1, $r1, 22
	addi	$r27, $r3, 0
div_blt_cont.6945:
	ldi	$r3, $r1, 0
	ble	$r27, $r3, div_ble_taken.6948
	nop
	nop
	ldi	$r8, $r1, -21
	slli	$r3, $r8, 1
	sti	$r3, $r1, -22
	add	$r3, $r8, $r3
	srai	$r6, $r3, 1
	ldi	$r4, $r1, -1
	sti	$r6, $r1, -23
	blt	$r4, $r0, div_blt_taken.6949
	nop
	nop
	beq	$r4, $r0, div_beq_taken.6951
	nop
	nop
	srai	$r5, $r4, 1
	slli	$r3, $r5, 1
	sub	$r3, $r4, $r3
	beq	$r3, $r0, div_beq_taken.6953
	nop
	nop
	slli	$r3, $r6, 1
	addi	$r4, $r5, 0
	subi	$r1, $r1, 24
	call	mul_sub
	addi	$r1, $r1, 24
	ldi	$r5, $r1, -23
	add	$r27, $r3, $r5
	j	div_blt_cont.6950
div_beq_taken.6953:
	slli	$r3, $r6, 1
	addi	$r4, $r5, 0
	subi	$r1, $r1, 24
	call	mul_sub
	addi	$r1, $r1, 24
	addi	$r27, $r3, 0
	j	div_blt_cont.6950
div_beq_taken.6951:
	addi	$r27, $r0, 0
	j	div_blt_cont.6950
div_blt_taken.6949:
	sub	$r3, $r0, $r6
	sub	$r5, $r0, $r4
	beq	$r5, $r0, div_beq_taken.6951
	nop
	nop
	srai	$r4, $r5, 1
	slli	$r6, $r4, 1
	sub	$r5, $r5, $r6
	beq	$r5, $r0, div_beq_taken.6959
	nop
	nop
	sti	$r3, $r1, -24
	slli	$r3, $r3, 1
	subi	$r1, $r1, 25
	call	mul_sub
	addi	$r1, $r1, 25
	ldi	$r9, $r1, -24
	add	$r27, $r3, $r9
	j	div_blt_cont.6950
div_beq_taken.6959:
	slli	$r3, $r3, 1
	subi	$r1, $r1, 25
	call	mul_sub
	addi	$r1, $r1, 25
	addi	$r27, $r3, 0
div_blt_cont.6950:
	ldi	$r5, $r1, -21
	ldi	$r6, $r1, -22
	sub	$r7, $r6, $r5
	ble	$r7, $r30, div_ble_taken.6807
	nop
	nop
	ldi	$r3, $r1, 0
	blt	$r27, $r3, div_blt_taken.6964
	nop
	nop
	beq	$r27, $r3, div_beq_taken.6965
	nop
	nop
	ldi	$r4, $r1, -1
	ldi	$r8, $r1, -23
	addi	$r6, $r8, 0
	j	min_caml_div_binary_search
div_beq_taken.6965:
	ldi	$r8, $r1, -23
	addi	$r3, $r8, 0
	return
div_blt_taken.6964:
	ldi	$r4, $r1, -1
	ldi	$r8, $r1, -23
	addi	$r5, $r8, 0
	j	min_caml_div_binary_search
div_ble_taken.6948:
	ldi	$r8, $r1, -21
	slli	$r8, $r8, 1
	blt	$r8, $r0, div_blt_taken.6966
	nop
	nop
	beq	$r8, $r0, div_beq_taken.6967
	nop
	nop
	srai	$r4, $r8, 1
	slli	$r3, $r4, 1
	sub	$r3, $r8, $r3
	sti	$r8, $r1, -25
	beq	$r3, $r0, div_beq_taken.6968
	nop
	nop
	ldi	$r6, $r1, -3
	slli	$r3, $r6, 1
	subi	$r1, $r1, 26
	call	mul_sub
	addi	$r1, $r1, 26
	ldi	$r6, $r1, -3
	add	$r27, $r3, $r6
	j	div_beq_cont.6969
div_beq_taken.6968:
	ldi	$r6, $r1, -3
	slli	$r3, $r6, 1
	subi	$r1, $r1, 26
	call	mul_sub
	addi	$r1, $r1, 26
	addi	$r27, $r3, 0
div_beq_cont.6969:
	ldi	$r3, $r1, 0
	ble	$r27, $r3, div_ble_taken.6972
	nop
	nop
	ldi	$r5, $r1, -25
	slli	$r6, $r5, 1
	ldi	$r4, $r1, -1
	j	min_caml_div_binary_search
div_ble_taken.6972:
	ldi	$r5, $r1, -25
	slli	$r5, $r5, 1
	ldi	$r4, $r1, -1
	j	div_sub
div_beq_taken.6967:
	ldi	$r4, $r1, -1
	ldi	$r5, $r1, -2
	j	div_sub
div_blt_taken.6966:
	sub	$r5, $r0, $r8
	beq	$r5, $r0, div_beq_taken.6973
	nop
	nop
	srai	$r4, $r5, 1
	slli	$r3, $r4, 1
	sub	$r3, $r5, $r3
	sti	$r8, $r1, -25
	beq	$r3, $r0, div_beq_taken.6974
	nop
	nop
	ldi	$r7, $r1, -15
	slli	$r3, $r7, 1
	subi	$r1, $r1, 26
	call	mul_sub
	addi	$r1, $r1, 26
	ldi	$r6, $r1, -15
	add	$r27, $r3, $r6
	j	div_beq_cont.6975
div_beq_taken.6974:
	ldi	$r7, $r1, -15
	slli	$r3, $r7, 1
	subi	$r1, $r1, 26
	call	mul_sub
	addi	$r1, $r1, 26
	addi	$r27, $r3, 0
div_beq_cont.6975:
	ldi	$r3, $r1, 0
	ble	$r27, $r3, div_ble_taken.6978
	nop
	nop
	ldi	$r5, $r1, -25
	slli	$r6, $r5, 1
	ldi	$r4, $r1, -1
	j	min_caml_div_binary_search
div_ble_taken.6978:
	ldi	$r5, $r1, -25
	slli	$r5, $r5, 1
	ldi	$r4, $r1, -1
	j	div_sub
div_beq_taken.6973:
	slli	$r5, $r8, 1
	ldi	$r4, $r1, -1
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


min_caml_land_sub:
	slli $r5, $r5, 31
	addi $r6, $r0, 30
	addi $r12, $r0, 0
land_sub_loop:
	blt $r6, $r0, land_sub_return
	addi $r7, $r3, 0
	srai $r3, $r3, 1
	srai $r12, $r12, 1
	slli $r8, $r3, 1
	sub $r7, $r7, $r8
	beq $r7, $r0, land_sub_skip
	addi $r9, $r4, 0
	srai $r4, $r4, 1
	slli $r10, $r4, 1
	sub $r9, $r9, $r10
	beq $r9, $r0, land_sub_skip
	nop
	nop
	slli $r11, $r30, 30
	add $r12, $r12, $r11
land_sub_skip:
	subi $r6, $r6, 1
	j land_sub_loop
land_sub_return:
	xor $r3, $r5, $r12
	return

