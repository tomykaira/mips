
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
	return
CREATE_FLOAT_ARRAY_CONTINUE:
	fsti $f0, $r2, 0
	addi $r2, $r2, 1
	j CREATE_FLOAT_ARRAY_LOOP

	
# * floor		$f0 + MAGICF - MAGICF
min_caml_floor:
	fmov $f1, $f0
	# $f4 <- 0.0
	# fset $f4, 0.0
	fmvhi $f4, 0
	fmvlo $f4, 0
	fblt $f0, $f4, FLOOR_NEGATIVE	# if ($f4 <= $f0) goto FLOOR_PISITIVE
FLOOR_POSITIVE:
	# $f2 <- 8388608.0(0x4b000000)
	fmvhi $f2, 19200
	fmvlo $f2, 0
	fblt $f2, $f0, FLOOR_POSITIVE_RET
FLOOR_POSITIVE_MAIN:
	fmov $f1, $f0
	fadd $f0, $f0, $f2
	fsti $f0, $r1, 0
	ldi $r4, $r1, 0
	fsub $f0, $f0, $f2
	fsti $f0, $r1, 0
	ldi $r4, $r1, 0
	fblt $f1, $f0, FLOOR_POSITIVE_RET
	return
FLOOR_POSITIVE_RET:
	# $f3 <- 1.0
	# fset $f3, 1.0
	fmvhi $f3, 16256
	fmvlo $f3, 0
	fsub $f0, $f0, $f3
	return
FLOOR_NEGATIVE:
	fneg $f0, $f0
	# $f2 <- 8388608.0(0x4b000000)
	fmvhi $f2, 19200
	fmvlo $f2, 0
	fblt $f2, $f0, FLOOR_NEGATIVE_RET
FLOOR_NEGATIVE_MAIN:
	fadd $f0, $f0, $f2
	fsub $f0, $f0, $f2
	fneg $f1, $f1
	fblt $f0, $f1, FLOOR_NEGATIVE_PRE_RET
	j FLOOR_NEGATIVE_RET
FLOOR_NEGATIVE_PRE_RET:
	fadd $f0, $f0, $f2
	# $f3 <- 1.0
	# fset $f3, 1.0
	fmvhi $f3, 16256
	fmvlo $f3, 0
	fadd $f0, $f0, $f3
	fsub $f0, $f0, $f2
FLOOR_NEGATIVE_RET:
	fneg $f0, $f0
	return
	
min_caml_ceil:
	fneg $f0, $f0
	call min_caml_floor
	fneg $f0, $f0
	return

# * float_of_int
min_caml_float_of_int:
	blt $r3, $r0, ITOF_NEGATIVE_MAIN		# if ($r0 <= $r3) goto ITOF_MAIN
ITOF_MAIN:
	# $f1 <- 8388608.0(0x4b000000)
	fmvhi $f1, 19200
	fmvlo $f1, 0
	# $r4 <- 0x4b000000
	mvhi $r4, 19200
	mvlo $r4, 0
	# $r5 <- 0x00800000
	mvhi $r5, 128
	mvlo $r5, 0
	blt $r3, $r5, ITOF_SMALL
ITOF_BIG:
	# $f2 <- 0.0
	# fset $f2, 0.0
	fmvhi $f2, 0
	fmvlo $f2, 0
ITOF_LOOP:
	sub $r3, $r3, $r5
	fadd $f2, $f2, $f1
	blt $r3, $r5, ITOF_RET
	j ITOF_LOOP
ITOF_RET:
	add $r3, $r3, $r4
	sti $r3, $r1, 0
	fldi $f0, $r1, 0
	fsub $f0, $f0, $f1
	fadd $f0, $f0, $f2
	return
ITOF_SMALL:
	add $r3, $r3, $r4
	sti $r3, $r1, 0
	fldi $f0, $r1, 0
	fsub $f0, $f0, $f1
	return
ITOF_NEGATIVE_MAIN:
	sub $r3, $r0, $r3
	call ITOF_MAIN
	fneg $f0, $f0
	return

# * int_of_float
min_caml_int_of_float:
	# $f1 <- 0.0
	# fset $f1, 0.0
	fmvhi $f1, 0
	fmvlo $f1, 0
	fblt $f0, $f1, FTOI_NEGATIVE_MAIN			# if (0.0 <= $f0) goto FTOI_MAIN
FTOI_POSITIVE_MAIN:
	# call min_caml_floor # is it needed??
	# $f2 <- 8388608.0(0x4b000000)
	fmvhi $f2, 19200
	fmvlo $f2, 0
	# $r4 <- 0x4b000000
	mvhi $r4, 19200
	mvlo $r4, 0
	fblt $f0, $f2, FTOI_SMALL		# if (MAGICF <= $f0) goto FTOI_BIG
	# $r5 <- 0x00800000
	mvhi $r5, 128
	mvlo $r5, 0
	mov $r3, $r0
FTOI_LOOP:
	fsub $f0, $f0, $f2
	add $r3, $r3, $r5
	fblt $f0, $f2, FTOI_RET
	j FTOI_LOOP
FTOI_RET:
	fadd $f0, $f0, $f2
	fmovi $r5, $f0
	sub $r5, $r5, $r4
	add $r3, $r5, $r3
	return
FTOI_SMALL:
	fadd $f0, $f0, $f2
	fmovi $r3, $f0
	sub $r3, $r3, $r4
	return
FTOI_NEGATIVE_MAIN:
	fneg $f0, $f0
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
	call min_caml_read_int
	sti $r3, $r1, 0
	fldi $f0, $r1, 0
	return

#----------------------------------------------------------------------
#
# lib_asm.s
#
#----------------------------------------------------------------------


min_caml_sqrt:
	fsqrt $f0, $f0
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
# in: $f0, out: $f0
min_caml_exp:
  fsti $f0, $r1, 0
	# $f4 <- 1.4426950216293335 (1/log(2))
	# fset $f4, 3fb8aa3b
	fmvhi $f4, 16312
	fmvlo $f4, 43579
	fmul $f0, $f0, $f4
	addi $r1, $r1, 2
	call min_caml_floor
	subi $r1, $r1, 2
	fsti $f0, $r1, 1
	addi $r1, $r1, 2
	call min_caml_int_of_float
	subi $r1, $r1, 2
	# px = $f0, x = $f1, C1 = f3, C2 = f4
	fldi $f1, $r1, 0
	fldi $f0, $r1, 1
	fmvhi $f3, 16177
	fmvlo $f3, 29184
	fmvhi $f4, 13759
	fmvlo $f4, 48782
	fmul $f2, $f3, $f0
	fsub $f1, $f1, $f2
	fmul $f2, $f4, $f0
	fsub $f1, $f1, $f2
	# x = $f1, a = $f2
	fmvhi $f2, 15426
	fmvlo $f2, 12737
	fmul $f2, $f2, $f1

	fmvhi $f3, 15646
	fmvlo $f3, 44824
	fadd $f2, $f2, $f3
	fmul $f2, $f2, $f1

	fmvhi $f3, 15915
	fmvlo $f3, 51130
	fadd $f2, $f2, $f3
	fmul $f2, $f2, $f1

	fmvhi $f3, 16127
	fmvlo $f3, 59474
	fadd $f2, $f2, $f3
	fmul $f2, $f2, $f1

	fmvhi $f3, 16256
	fmvlo $f3, 92
	fadd $f2, $f2, $f3
	fmul $f2, $f2, $f1

	fmvhi $f3, 16255
	fmvlo $f3, 65534
	fadd $f2, $f2, $f3

	addi $r3, $r3, 127
	andi $r3, $r3, 255
	slli $r3, $r3, 23
	imovf $f3, $r3

	fmul $f0, $f2, $f3
	return

# algorithm: remez5, 1_e
# in: $f0, out: $f0
min_caml_log:
	# 0
	fmvhi $f4, 0
	fmvlo $f4, 0
	fblt $f0, $f4, LOG_END	# if ($f0 < 0) return itself, otherwise this loop will not stop

	# 1
	fmvhi $f5, 16256
	fmvlo $f5, 0

	# e
	fmvhi $f6, 16429
	fmvlo $f6, 63572

	# 1/e
	fmvhi $f7, 16060
	fmvlo $f7, 23218

	# y
	addi $r3, $r0, 0

	fble $f5, $f0, LOG_LOOP_LT_1_END

LOG_LOOP_LT_1:
	subi $r3, $r3, 1
	fmul $f0, $f0, $f6
	fblt $f0, $f5, LOG_LOOP_LT_1

LOG_LOOP_LT_1_END:
	fblt $f0, $f6, LOG_LOOP_GT_E_END

LOG_LOOP_GT_E:
	addi $r3, $r3, 1
	fmul $f0, $f0, $f7
	fblt $f6, $f0, LOG_LOOP_GT_E

LOG_LOOP_GT_E_END:
	
	# x = $f0, a = $f2
	fmvhi $f2, 15438
	fmvlo $f2, 18672
	fmul $f2, $f2, $f0

	fmvhi $f3, 48658
	fmvlo $f3, 58995
	fadd $f2, $f2, $f3
	fmul $f2, $f2, $f0

	fmvhi $f3, 16174
	fmvlo $f3, 35574
	fadd $f2, $f2, $f3
	fmul $f2, $f2, $f0

	fmvhi $f3, 49123
	fmvlo $f3, 52990
	fadd $f2, $f2, $f3
	fmul $f2, $f2, $f0

	fmvhi $f3, 16449
	fmvlo $f3, 23445
	fadd $f2, $f2, $f3
	fmul $f2, $f2, $f0

	fmvhi $f3, 49125
	fmvlo $f3, 27378
	fadd $f2, $f2, $f3

	fsti $f2, $r1, 0
	addi $r1, $r1, 1
	# y = $r3 to $f0
	call min_caml_float_of_int
	subi $r1, $r1, 1
	fldi $f1, $r1, 0
	fadd $f0, $f0, $f1
LOG_END:
	return

min_caml_land:
	and $r3, $r3, $r4
	return

min_caml_sinh:
	call min_caml_exp
	# $f0 = e^x
	finv $f1, $f0
	# e^x - e^(-x)
	fsub $f1, $f0, $f1
	# * 0.5
	fmvhi $f3, 16128
	fmvlo $f3, 0
	fmul $f0, $f1, $f3
	return

min_caml_cosh:
	call min_caml_exp
	# $f0 = e^x
	finv $f1, $f0
	# e^x - e^(-x)
	fadd $f1, $f0, $f1
	# * 0.5
	fmvhi $f3, 16128
	fmvlo $f3, 0
	fmul $f0, $f1, $f3
	return
