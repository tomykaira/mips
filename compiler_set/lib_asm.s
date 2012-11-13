
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
	subi $r1, $r1, 2
	call min_caml_floor
	addi $r1, $r1, 2
	fsti $f0, $r1, -1
	subi $r1, $r1, 2
	call min_caml_int_of_float
	addi $r1, $r1, 2
	# px = $f0, x = $f1, C1 = f3, C2 = f4
	fldi $f1, $r1, 0
	fldi $f0, $r1, -1

	fmvhi $f5, 0
	fmvlo $f5, 0
	fblt $f5, $f1, exp_skip
	# a - 1 if a < 0
	fmvhi $f5, 16256
	fmvlo $f5, 0
	fsub $f0, $f0, $f5
	subi $r3, $r3, 1
exp_skip:
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
	subi $r1, $r1, 1
	# y = $r3 to $f0
	call min_caml_float_of_int
	addi $r1, $r1, 1
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

min_caml_mul:
	mul $r3, $r3, $r4
	return

# copied from generated asm
# print_int uses div_binary_search
min_caml_div_binary_search:
	add	$r7, $r5, $r6
	srai	$r7, $r7, 1
	mul	$r8, $r7, $r4
	sub	$r9, $r6, $r5
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3062
	blt	$r8, $r3, blt_taken.3063
	beq	$r8, $r3, beq_taken.3064
	add	$r6, $r5, $r7
	srai	$r6, $r6, 1
	mul	$r8, $r6, $r4
	sub	$r9, $r7, $r5
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3065
	blt	$r8, $r3, blt_taken.3066
	beq	$r8, $r3, beq_taken.3067
	add	$r7, $r5, $r6
	srai	$r7, $r7, 1
	mul	$r8, $r7, $r4
	sub	$r9, $r6, $r5
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3068
	blt	$r8, $r3, blt_taken.3069
	beq	$r8, $r3, beq_taken.3070
	add	$r6, $r5, $r7
	srai	$r6, $r6, 1
	mul	$r8, $r6, $r4
	sub	$r9, $r7, $r5
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3071
	blt	$r8, $r3, blt_taken.3072
	beq	$r8, $r3, beq_taken.3073
	j	min_caml_div_binary_search
beq_taken.3073:
	addi	$r3, $r6, 0
	return
blt_taken.3072:
	addi	$r5, $r6, 0
	addi	$r6, $r7, 0
	j	min_caml_div_binary_search
ble_taken.3071:
	addi	$r3, $r5, 0
	return
beq_taken.3070:
	addi	$r3, $r7, 0
	return
blt_taken.3069:
	add	$r5, $r7, $r6
	srai	$r5, $r5, 1
	mul	$r8, $r5, $r4
	sub	$r9, $r6, $r7
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3074
	blt	$r8, $r3, blt_taken.3075
	beq	$r8, $r3, beq_taken.3076
	addi	$r6, $r5, 0
	addi	$r5, $r7, 0
	j	min_caml_div_binary_search
beq_taken.3076:
	addi	$r3, $r5, 0
	return
blt_taken.3075:
	j	min_caml_div_binary_search
ble_taken.3074:
	addi	$r3, $r7, 0
	return
ble_taken.3068:
	addi	$r3, $r5, 0
	return
beq_taken.3067:
	addi	$r3, $r6, 0
	return
blt_taken.3066:
	add	$r5, $r6, $r7
	srai	$r5, $r5, 1
	mul	$r8, $r5, $r4
	sub	$r9, $r7, $r6
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3077
	blt	$r8, $r3, blt_taken.3078
	beq	$r8, $r3, beq_taken.3079
	add	$r7, $r6, $r5
	srai	$r7, $r7, 1
	mul	$r8, $r7, $r4
	sub	$r9, $r5, $r6
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3080
	blt	$r8, $r3, blt_taken.3081
	beq	$r8, $r3, beq_taken.3082
	addi	$r5, $r6, 0
	addi	$r6, $r7, 0
	j	min_caml_div_binary_search
beq_taken.3082:
	addi	$r3, $r7, 0
	return
blt_taken.3081:
	addi	$r6, $r5, 0
	addi	$r5, $r7, 0
	j	min_caml_div_binary_search
ble_taken.3080:
	addi	$r3, $r6, 0
	return
beq_taken.3079:
	addi	$r3, $r5, 0
	return
blt_taken.3078:
	add	$r6, $r5, $r7
	srai	$r6, $r6, 1
	mul	$r8, $r6, $r4
	sub	$r9, $r7, $r5
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3083
	blt	$r8, $r3, blt_taken.3084
	beq	$r8, $r3, beq_taken.3085
	j	min_caml_div_binary_search
beq_taken.3085:
	addi	$r3, $r6, 0
	return
blt_taken.3084:
	addi	$r5, $r6, 0
	addi	$r6, $r7, 0
	j	min_caml_div_binary_search
ble_taken.3083:
	addi	$r3, $r5, 0
	return
ble_taken.3077:
	addi	$r3, $r6, 0
	return
ble_taken.3065:
	addi	$r3, $r5, 0
	return
beq_taken.3064:
	addi	$r3, $r7, 0
	return
blt_taken.3063:
	add	$r5, $r7, $r6
	srai	$r5, $r5, 1
	mul	$r8, $r5, $r4
	sub	$r9, $r6, $r7
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3086
	blt	$r8, $r3, blt_taken.3087
	beq	$r8, $r3, beq_taken.3088
	add	$r6, $r7, $r5
	srai	$r6, $r6, 1
	mul	$r8, $r6, $r4
	sub	$r9, $r5, $r7
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3089
	blt	$r8, $r3, blt_taken.3090
	beq	$r8, $r3, beq_taken.3091
	add	$r5, $r7, $r6
	srai	$r5, $r5, 1
	mul	$r8, $r5, $r4
	sub	$r9, $r6, $r7
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3092
	blt	$r8, $r3, blt_taken.3093
	beq	$r8, $r3, beq_taken.3094
	addi	$r6, $r5, 0
	addi	$r5, $r7, 0
	j	min_caml_div_binary_search
beq_taken.3094:
	addi	$r3, $r5, 0
	return
blt_taken.3093:
	j	min_caml_div_binary_search
ble_taken.3092:
	addi	$r3, $r7, 0
	return
beq_taken.3091:
	addi	$r3, $r6, 0
	return
blt_taken.3090:
	add	$r7, $r6, $r5
	srai	$r7, $r7, 1
	mul	$r8, $r7, $r4
	sub	$r9, $r5, $r6
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3095
	blt	$r8, $r3, blt_taken.3096
	beq	$r8, $r3, beq_taken.3097
	addi	$r5, $r6, 0
	addi	$r6, $r7, 0
	j	min_caml_div_binary_search
beq_taken.3097:
	addi	$r3, $r7, 0
	return
blt_taken.3096:
	addi	$r6, $r5, 0
	addi	$r5, $r7, 0
	j	min_caml_div_binary_search
ble_taken.3095:
	addi	$r3, $r6, 0
	return
ble_taken.3089:
	addi	$r3, $r7, 0
	return
beq_taken.3088:
	addi	$r3, $r5, 0
	return
blt_taken.3087:
	add	$r7, $r5, $r6
	srai	$r7, $r7, 1
	mul	$r8, $r7, $r4
	sub	$r9, $r6, $r5
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3098
	blt	$r8, $r3, blt_taken.3099
	beq	$r8, $r3, beq_taken.3100
	add	$r6, $r5, $r7
	srai	$r6, $r6, 1
	mul	$r8, $r6, $r4
	sub	$r9, $r7, $r5
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3101
	blt	$r8, $r3, blt_taken.3102
	beq	$r8, $r3, beq_taken.3103
	j	min_caml_div_binary_search
beq_taken.3103:
	addi	$r3, $r6, 0
	return
blt_taken.3102:
	addi	$r5, $r6, 0
	addi	$r6, $r7, 0
	j	min_caml_div_binary_search
ble_taken.3101:
	addi	$r3, $r5, 0
	return
beq_taken.3100:
	addi	$r3, $r7, 0
	return
blt_taken.3099:
	add	$r5, $r7, $r6
	srai	$r5, $r5, 1
	mul	$r8, $r5, $r4
	sub	$r9, $r6, $r7
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3104
	blt	$r8, $r3, blt_taken.3105
	beq	$r8, $r3, beq_taken.3106
	addi	$r6, $r5, 0
	addi	$r5, $r7, 0
	j	min_caml_div_binary_search
beq_taken.3106:
	addi	$r3, $r5, 0
	return
blt_taken.3105:
	j	min_caml_div_binary_search
ble_taken.3104:
	addi	$r3, $r7, 0
	return
ble_taken.3098:
	addi	$r3, $r5, 0
	return
ble_taken.3086:
	addi	$r3, $r7, 0
	return
ble_taken.3062:
	addi	$r3, $r5, 0
	return
print_int.457:
	blt	$r3, $r0, blt_taken.3107
	mvlo	$r4, 57600
	mvhi	$r4, 1525
	blt	$r4, $r3, blt_taken.3108
	beq	$r4, $r3, beq_taken.3110
	addi	$r4, $r0, 0
	j	beq_cont.3111
beq_taken.3110:
	addi	$r4, $r0, 1
beq_cont.3111:
	j	blt_cont.3109
blt_taken.3108:
	mvlo	$r4, 49664
	mvhi	$r4, 3051
	blt	$r4, $r3, blt_taken.3112
	beq	$r4, $r3, beq_taken.3114
	addi	$r4, $r0, 1
	j	beq_cont.3115
beq_taken.3114:
	addi	$r4, $r0, 2
beq_cont.3115:
	j	blt_cont.3113
blt_taken.3112:
	addi	$r4, $r0, 2
blt_cont.3113:
blt_cont.3109:
	mvlo	$r5, 57600
	mvhi	$r5, 1525
	mul	$r5, $r4, $r5
	sub	$r3, $r3, $r5
	sti	$r3, $r1, 0
	ble	$r4, $r0, ble_taken.3116
	addi	$r5, $r0, 48
	add	$r4, $r5, $r4
	addi	$r3, $r4, 0
	subi	$r1, $r1, 2
	call	min_caml_print_char
	addi	$r1, $r1, 2
	addi	$r3, $r0, 1
	j	ble_cont.3117
ble_taken.3116:
	addi	$r3, $r0, 0
ble_cont.3117:
	mvlo	$r4, 38528
	mvhi	$r4, 152
	addi	$r5, $r0, 0
	addi	$r6, $r0, 10
	addi	$r7, $r0, 5
	mvlo	$r8, 61568
	mvhi	$r8, 762
	ldi	$r9, $r1, 0
	sti	$r3, $r1, -1
	blt	$r8, $r9, blt_taken.3118
	beq	$r8, $r9, beq_taken.3120
	addi	$r6, $r0, 2
	mvlo	$r8, 11520
	mvhi	$r8, 305
	blt	$r8, $r9, blt_taken.3122
	beq	$r8, $r9, beq_taken.3124
	addi	$r7, $r0, 1
	mvlo	$r8, 38528
	mvhi	$r8, 152
	blt	$r8, $r9, blt_taken.3126
	beq	$r8, $r9, beq_taken.3128
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 3
	call	min_caml_div_binary_search
	addi	$r1, $r1, 3
	j	beq_cont.3129
beq_taken.3128:
	addi	$r3, $r0, 1
beq_cont.3129:
	j	blt_cont.3127
blt_taken.3126:
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 3
	call	min_caml_div_binary_search
	addi	$r1, $r1, 3
blt_cont.3127:
	j	beq_cont.3125
beq_taken.3124:
	addi	$r3, $r0, 2
beq_cont.3125:
	j	blt_cont.3123
blt_taken.3122:
	addi	$r5, $r0, 3
	mvlo	$r8, 50048
	mvhi	$r8, 457
	blt	$r8, $r9, blt_taken.3130
	beq	$r8, $r9, beq_taken.3132
	addi	$r3, $r9, 0
	addi	$r28, $r6, 0
	addi	$r6, $r5, 0
	addi	$r5, $r28, 0
	subi	$r1, $r1, 3
	call	min_caml_div_binary_search
	addi	$r1, $r1, 3
	j	beq_cont.3133
beq_taken.3132:
	addi	$r3, $r0, 3
beq_cont.3133:
	j	blt_cont.3131
blt_taken.3130:
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 3
	call	min_caml_div_binary_search
	addi	$r1, $r1, 3
blt_cont.3131:
blt_cont.3123:
	j	beq_cont.3121
beq_taken.3120:
	addi	$r3, $r0, 5
beq_cont.3121:
	j	blt_cont.3119
blt_taken.3118:
	addi	$r5, $r0, 7
	mvlo	$r8, 7552
	mvhi	$r8, 1068
	blt	$r8, $r9, blt_taken.3134
	beq	$r8, $r9, beq_taken.3136
	addi	$r6, $r0, 6
	mvlo	$r8, 34560
	mvhi	$r8, 915
	blt	$r8, $r9, blt_taken.3138
	beq	$r8, $r9, beq_taken.3140
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 3
	call	min_caml_div_binary_search
	addi	$r1, $r1, 3
	j	beq_cont.3141
beq_taken.3140:
	addi	$r3, $r0, 6
beq_cont.3141:
	j	blt_cont.3139
blt_taken.3138:
	addi	$r3, $r9, 0
	addi	$r28, $r6, 0
	addi	$r6, $r5, 0
	addi	$r5, $r28, 0
	subi	$r1, $r1, 3
	call	min_caml_div_binary_search
	addi	$r1, $r1, 3
blt_cont.3139:
	j	beq_cont.3137
beq_taken.3136:
	addi	$r3, $r0, 7
beq_cont.3137:
	j	blt_cont.3135
blt_taken.3134:
	addi	$r7, $r0, 8
	mvlo	$r8, 46080
	mvhi	$r8, 1220
	blt	$r8, $r9, blt_taken.3142
	beq	$r8, $r9, beq_taken.3144
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 3
	call	min_caml_div_binary_search
	addi	$r1, $r1, 3
	j	beq_cont.3145
beq_taken.3144:
	addi	$r3, $r0, 8
beq_cont.3145:
	j	blt_cont.3143
blt_taken.3142:
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 3
	call	min_caml_div_binary_search
	addi	$r1, $r1, 3
blt_cont.3143:
blt_cont.3135:
blt_cont.3119:
	mvlo	$r4, 38528
	mvhi	$r4, 152
	mul	$r4, $r3, $r4
	ldi	$r5, $r1, 0
	sub	$r4, $r5, $r4
	sti	$r4, $r1, -2
	ble	$r3, $r0, ble_taken.3146
	addi	$r5, $r0, 48
	add	$r3, $r5, $r3
	subi	$r1, $r1, 4
	call	min_caml_print_char
	addi	$r1, $r1, 4
	addi	$r3, $r0, 1
	j	ble_cont.3147
ble_taken.3146:
	ldi	$r5, $r1, -1
	beq	$r5, $r0, beq_taken.3148
	addi	$r5, $r0, 48
	add	$r3, $r5, $r3
	subi	$r1, $r1, 4
	call	min_caml_print_char
	addi	$r1, $r1, 4
	addi	$r3, $r0, 1
	j	beq_cont.3149
beq_taken.3148:
	addi	$r3, $r0, 0
beq_cont.3149:
ble_cont.3147:
	mvlo	$r4, 16960
	mvhi	$r4, 15
	addi	$r5, $r0, 0
	addi	$r6, $r0, 10
	addi	$r7, $r0, 5
	mvlo	$r8, 19264
	mvhi	$r8, 76
	ldi	$r9, $r1, -2
	sti	$r3, $r1, -3
	blt	$r8, $r9, blt_taken.3150
	beq	$r8, $r9, beq_taken.3152
	addi	$r6, $r0, 2
	mvlo	$r8, 33920
	mvhi	$r8, 30
	blt	$r8, $r9, blt_taken.3154
	beq	$r8, $r9, beq_taken.3156
	addi	$r7, $r0, 1
	mvlo	$r8, 16960
	mvhi	$r8, 15
	blt	$r8, $r9, blt_taken.3158
	beq	$r8, $r9, beq_taken.3160
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 5
	call	min_caml_div_binary_search
	addi	$r1, $r1, 5
	j	beq_cont.3161
beq_taken.3160:
	addi	$r3, $r0, 1
beq_cont.3161:
	j	blt_cont.3159
blt_taken.3158:
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 5
	call	min_caml_div_binary_search
	addi	$r1, $r1, 5
blt_cont.3159:
	j	beq_cont.3157
beq_taken.3156:
	addi	$r3, $r0, 2
beq_cont.3157:
	j	blt_cont.3155
blt_taken.3154:
	addi	$r5, $r0, 3
	mvlo	$r8, 50880
	mvhi	$r8, 45
	blt	$r8, $r9, blt_taken.3162
	beq	$r8, $r9, beq_taken.3164
	addi	$r3, $r9, 0
	addi	$r28, $r6, 0
	addi	$r6, $r5, 0
	addi	$r5, $r28, 0
	subi	$r1, $r1, 5
	call	min_caml_div_binary_search
	addi	$r1, $r1, 5
	j	beq_cont.3165
beq_taken.3164:
	addi	$r3, $r0, 3
beq_cont.3165:
	j	blt_cont.3163
blt_taken.3162:
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 5
	call	min_caml_div_binary_search
	addi	$r1, $r1, 5
blt_cont.3163:
blt_cont.3155:
	j	beq_cont.3153
beq_taken.3152:
	addi	$r3, $r0, 5
beq_cont.3153:
	j	blt_cont.3151
blt_taken.3150:
	addi	$r5, $r0, 7
	mvlo	$r8, 53184
	mvhi	$r8, 106
	blt	$r8, $r9, blt_taken.3166
	beq	$r8, $r9, beq_taken.3168
	addi	$r6, $r0, 6
	mvlo	$r8, 36224
	mvhi	$r8, 91
	blt	$r8, $r9, blt_taken.3170
	beq	$r8, $r9, beq_taken.3172
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 5
	call	min_caml_div_binary_search
	addi	$r1, $r1, 5
	j	beq_cont.3173
beq_taken.3172:
	addi	$r3, $r0, 6
beq_cont.3173:
	j	blt_cont.3171
blt_taken.3170:
	addi	$r3, $r9, 0
	addi	$r28, $r6, 0
	addi	$r6, $r5, 0
	addi	$r5, $r28, 0
	subi	$r1, $r1, 5
	call	min_caml_div_binary_search
	addi	$r1, $r1, 5
blt_cont.3171:
	j	beq_cont.3169
beq_taken.3168:
	addi	$r3, $r0, 7
beq_cont.3169:
	j	blt_cont.3167
blt_taken.3166:
	addi	$r7, $r0, 8
	mvlo	$r8, 4608
	mvhi	$r8, 122
	blt	$r8, $r9, blt_taken.3174
	beq	$r8, $r9, beq_taken.3176
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 5
	call	min_caml_div_binary_search
	addi	$r1, $r1, 5
	j	beq_cont.3177
beq_taken.3176:
	addi	$r3, $r0, 8
beq_cont.3177:
	j	blt_cont.3175
blt_taken.3174:
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 5
	call	min_caml_div_binary_search
	addi	$r1, $r1, 5
blt_cont.3175:
blt_cont.3167:
blt_cont.3151:
	mvlo	$r4, 16960
	mvhi	$r4, 15
	mul	$r4, $r3, $r4
	ldi	$r5, $r1, -2
	sub	$r4, $r5, $r4
	sti	$r4, $r1, -4
	ble	$r3, $r0, ble_taken.3178
	addi	$r5, $r0, 48
	add	$r3, $r5, $r3
	subi	$r1, $r1, 6
	call	min_caml_print_char
	addi	$r1, $r1, 6
	addi	$r3, $r0, 1
	j	ble_cont.3179
ble_taken.3178:
	ldi	$r5, $r1, -3
	beq	$r5, $r0, beq_taken.3180
	addi	$r5, $r0, 48
	add	$r3, $r5, $r3
	subi	$r1, $r1, 6
	call	min_caml_print_char
	addi	$r1, $r1, 6
	addi	$r3, $r0, 1
	j	beq_cont.3181
beq_taken.3180:
	addi	$r3, $r0, 0
beq_cont.3181:
ble_cont.3179:
	mvlo	$r4, 34464
	mvhi	$r4, 1
	addi	$r5, $r0, 0
	addi	$r6, $r0, 10
	addi	$r7, $r0, 5
	mvlo	$r8, 41248
	mvhi	$r8, 7
	ldi	$r9, $r1, -4
	sti	$r3, $r1, -5
	blt	$r8, $r9, blt_taken.3182
	beq	$r8, $r9, beq_taken.3184
	addi	$r6, $r0, 2
	mvlo	$r8, 3392
	mvhi	$r8, 3
	blt	$r8, $r9, blt_taken.3186
	beq	$r8, $r9, beq_taken.3188
	addi	$r7, $r0, 1
	mvlo	$r8, 34464
	mvhi	$r8, 1
	blt	$r8, $r9, blt_taken.3190
	beq	$r8, $r9, beq_taken.3192
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 7
	call	min_caml_div_binary_search
	addi	$r1, $r1, 7
	j	beq_cont.3193
beq_taken.3192:
	addi	$r3, $r0, 1
beq_cont.3193:
	j	blt_cont.3191
blt_taken.3190:
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 7
	call	min_caml_div_binary_search
	addi	$r1, $r1, 7
blt_cont.3191:
	j	beq_cont.3189
beq_taken.3188:
	addi	$r3, $r0, 2
beq_cont.3189:
	j	blt_cont.3187
blt_taken.3186:
	addi	$r5, $r0, 3
	mvlo	$r8, 37856
	mvhi	$r8, 4
	blt	$r8, $r9, blt_taken.3194
	beq	$r8, $r9, beq_taken.3196
	addi	$r3, $r9, 0
	addi	$r28, $r6, 0
	addi	$r6, $r5, 0
	addi	$r5, $r28, 0
	subi	$r1, $r1, 7
	call	min_caml_div_binary_search
	addi	$r1, $r1, 7
	j	beq_cont.3197
beq_taken.3196:
	addi	$r3, $r0, 3
beq_cont.3197:
	j	blt_cont.3195
blt_taken.3194:
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 7
	call	min_caml_div_binary_search
	addi	$r1, $r1, 7
blt_cont.3195:
blt_cont.3187:
	j	beq_cont.3185
beq_taken.3184:
	addi	$r3, $r0, 5
beq_cont.3185:
	j	blt_cont.3183
blt_taken.3182:
	addi	$r5, $r0, 7
	mvlo	$r8, 44640
	mvhi	$r8, 10
	blt	$r8, $r9, blt_taken.3198
	beq	$r8, $r9, beq_taken.3200
	addi	$r6, $r0, 6
	mvlo	$r8, 10176
	mvhi	$r8, 9
	blt	$r8, $r9, blt_taken.3202
	beq	$r8, $r9, beq_taken.3204
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 7
	call	min_caml_div_binary_search
	addi	$r1, $r1, 7
	j	beq_cont.3205
beq_taken.3204:
	addi	$r3, $r0, 6
beq_cont.3205:
	j	blt_cont.3203
blt_taken.3202:
	addi	$r3, $r9, 0
	addi	$r28, $r6, 0
	addi	$r6, $r5, 0
	addi	$r5, $r28, 0
	subi	$r1, $r1, 7
	call	min_caml_div_binary_search
	addi	$r1, $r1, 7
blt_cont.3203:
	j	beq_cont.3201
beq_taken.3200:
	addi	$r3, $r0, 7
beq_cont.3201:
	j	blt_cont.3199
blt_taken.3198:
	addi	$r7, $r0, 8
	mvlo	$r8, 13568
	mvhi	$r8, 12
	blt	$r8, $r9, blt_taken.3206
	beq	$r8, $r9, beq_taken.3208
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 7
	call	min_caml_div_binary_search
	addi	$r1, $r1, 7
	j	beq_cont.3209
beq_taken.3208:
	addi	$r3, $r0, 8
beq_cont.3209:
	j	blt_cont.3207
blt_taken.3206:
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 7
	call	min_caml_div_binary_search
	addi	$r1, $r1, 7
blt_cont.3207:
blt_cont.3199:
blt_cont.3183:
	mvlo	$r4, 34464
	mvhi	$r4, 1
	mul	$r4, $r3, $r4
	ldi	$r5, $r1, -4
	sub	$r4, $r5, $r4
	sti	$r4, $r1, -6
	ble	$r3, $r0, ble_taken.3210
	addi	$r5, $r0, 48
	add	$r3, $r5, $r3
	subi	$r1, $r1, 8
	call	min_caml_print_char
	addi	$r1, $r1, 8
	addi	$r3, $r0, 1
	j	ble_cont.3211
ble_taken.3210:
	ldi	$r5, $r1, -5
	beq	$r5, $r0, beq_taken.3212
	addi	$r5, $r0, 48
	add	$r3, $r5, $r3
	subi	$r1, $r1, 8
	call	min_caml_print_char
	addi	$r1, $r1, 8
	addi	$r3, $r0, 1
	j	beq_cont.3213
beq_taken.3212:
	addi	$r3, $r0, 0
beq_cont.3213:
ble_cont.3211:
	addi	$r4, $r0, 10000
	addi	$r5, $r0, 0
	addi	$r6, $r0, 10
	addi	$r7, $r0, 5
	mvlo	$r8, 50000
	mvhi	$r8, 0
	ldi	$r9, $r1, -6
	sti	$r3, $r1, -7
	blt	$r8, $r9, blt_taken.3214
	beq	$r8, $r9, beq_taken.3216
	addi	$r6, $r0, 2
	addi	$r8, $r0, 20000
	blt	$r8, $r9, blt_taken.3218
	beq	$r8, $r9, beq_taken.3220
	addi	$r7, $r0, 1
	addi	$r8, $r0, 10000
	blt	$r8, $r9, blt_taken.3222
	beq	$r8, $r9, beq_taken.3224
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 9
	call	min_caml_div_binary_search
	addi	$r1, $r1, 9
	j	beq_cont.3225
beq_taken.3224:
	addi	$r3, $r0, 1
beq_cont.3225:
	j	blt_cont.3223
blt_taken.3222:
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 9
	call	min_caml_div_binary_search
	addi	$r1, $r1, 9
blt_cont.3223:
	j	beq_cont.3221
beq_taken.3220:
	addi	$r3, $r0, 2
beq_cont.3221:
	j	blt_cont.3219
blt_taken.3218:
	addi	$r5, $r0, 3
	addi	$r8, $r0, 30000
	blt	$r8, $r9, blt_taken.3226
	beq	$r8, $r9, beq_taken.3228
	addi	$r3, $r9, 0
	addi	$r28, $r6, 0
	addi	$r6, $r5, 0
	addi	$r5, $r28, 0
	subi	$r1, $r1, 9
	call	min_caml_div_binary_search
	addi	$r1, $r1, 9
	j	beq_cont.3229
beq_taken.3228:
	addi	$r3, $r0, 3
beq_cont.3229:
	j	blt_cont.3227
blt_taken.3226:
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 9
	call	min_caml_div_binary_search
	addi	$r1, $r1, 9
blt_cont.3227:
blt_cont.3219:
	j	beq_cont.3217
beq_taken.3216:
	addi	$r3, $r0, 5
beq_cont.3217:
	j	blt_cont.3215
blt_taken.3214:
	addi	$r5, $r0, 7
	mvlo	$r8, 4464
	mvhi	$r8, 1
	blt	$r8, $r9, blt_taken.3230
	beq	$r8, $r9, beq_taken.3232
	addi	$r6, $r0, 6
	mvlo	$r8, 60000
	mvhi	$r8, 0
	blt	$r8, $r9, blt_taken.3234
	beq	$r8, $r9, beq_taken.3236
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 9
	call	min_caml_div_binary_search
	addi	$r1, $r1, 9
	j	beq_cont.3237
beq_taken.3236:
	addi	$r3, $r0, 6
beq_cont.3237:
	j	blt_cont.3235
blt_taken.3234:
	addi	$r3, $r9, 0
	addi	$r28, $r6, 0
	addi	$r6, $r5, 0
	addi	$r5, $r28, 0
	subi	$r1, $r1, 9
	call	min_caml_div_binary_search
	addi	$r1, $r1, 9
blt_cont.3235:
	j	beq_cont.3233
beq_taken.3232:
	addi	$r3, $r0, 7
beq_cont.3233:
	j	blt_cont.3231
blt_taken.3230:
	addi	$r7, $r0, 8
	mvlo	$r8, 14464
	mvhi	$r8, 1
	blt	$r8, $r9, blt_taken.3238
	beq	$r8, $r9, beq_taken.3240
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 9
	call	min_caml_div_binary_search
	addi	$r1, $r1, 9
	j	beq_cont.3241
beq_taken.3240:
	addi	$r3, $r0, 8
beq_cont.3241:
	j	blt_cont.3239
blt_taken.3238:
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 9
	call	min_caml_div_binary_search
	addi	$r1, $r1, 9
blt_cont.3239:
blt_cont.3231:
blt_cont.3215:
	muli	$r4, $r3, 10000
	ldi	$r5, $r1, -6
	sub	$r4, $r5, $r4
	sti	$r4, $r1, -8
	ble	$r3, $r0, ble_taken.3242
	addi	$r5, $r0, 48
	add	$r3, $r5, $r3
	subi	$r1, $r1, 10
	call	min_caml_print_char
	addi	$r1, $r1, 10
	addi	$r3, $r0, 1
	j	ble_cont.3243
ble_taken.3242:
	ldi	$r5, $r1, -7
	beq	$r5, $r0, beq_taken.3244
	addi	$r5, $r0, 48
	add	$r3, $r5, $r3
	subi	$r1, $r1, 10
	call	min_caml_print_char
	addi	$r1, $r1, 10
	addi	$r3, $r0, 1
	j	beq_cont.3245
beq_taken.3244:
	addi	$r3, $r0, 0
beq_cont.3245:
ble_cont.3243:
	addi	$r4, $r0, 1000
	addi	$r5, $r0, 0
	addi	$r6, $r0, 10
	addi	$r7, $r0, 5
	addi	$r8, $r0, 5000
	ldi	$r9, $r1, -8
	sti	$r3, $r1, -9
	blt	$r8, $r9, blt_taken.3246
	beq	$r8, $r9, beq_taken.3248
	addi	$r6, $r0, 2
	addi	$r8, $r0, 2000
	blt	$r8, $r9, blt_taken.3250
	beq	$r8, $r9, beq_taken.3252
	addi	$r7, $r0, 1
	addi	$r8, $r0, 1000
	blt	$r8, $r9, blt_taken.3254
	beq	$r8, $r9, beq_taken.3256
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 11
	call	min_caml_div_binary_search
	addi	$r1, $r1, 11
	j	beq_cont.3257
beq_taken.3256:
	addi	$r3, $r0, 1
beq_cont.3257:
	j	blt_cont.3255
blt_taken.3254:
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 11
	call	min_caml_div_binary_search
	addi	$r1, $r1, 11
blt_cont.3255:
	j	beq_cont.3253
beq_taken.3252:
	addi	$r3, $r0, 2
beq_cont.3253:
	j	blt_cont.3251
blt_taken.3250:
	addi	$r5, $r0, 3
	addi	$r8, $r0, 3000
	blt	$r8, $r9, blt_taken.3258
	beq	$r8, $r9, beq_taken.3260
	addi	$r3, $r9, 0
	addi	$r28, $r6, 0
	addi	$r6, $r5, 0
	addi	$r5, $r28, 0
	subi	$r1, $r1, 11
	call	min_caml_div_binary_search
	addi	$r1, $r1, 11
	j	beq_cont.3261
beq_taken.3260:
	addi	$r3, $r0, 3
beq_cont.3261:
	j	blt_cont.3259
blt_taken.3258:
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 11
	call	min_caml_div_binary_search
	addi	$r1, $r1, 11
blt_cont.3259:
blt_cont.3251:
	j	beq_cont.3249
beq_taken.3248:
	addi	$r3, $r0, 5
beq_cont.3249:
	j	blt_cont.3247
blt_taken.3246:
	addi	$r5, $r0, 7
	addi	$r8, $r0, 7000
	blt	$r8, $r9, blt_taken.3262
	beq	$r8, $r9, beq_taken.3264
	addi	$r6, $r0, 6
	addi	$r8, $r0, 6000
	blt	$r8, $r9, blt_taken.3266
	beq	$r8, $r9, beq_taken.3268
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 11
	call	min_caml_div_binary_search
	addi	$r1, $r1, 11
	j	beq_cont.3269
beq_taken.3268:
	addi	$r3, $r0, 6
beq_cont.3269:
	j	blt_cont.3267
blt_taken.3266:
	addi	$r3, $r9, 0
	addi	$r28, $r6, 0
	addi	$r6, $r5, 0
	addi	$r5, $r28, 0
	subi	$r1, $r1, 11
	call	min_caml_div_binary_search
	addi	$r1, $r1, 11
blt_cont.3267:
	j	beq_cont.3265
beq_taken.3264:
	addi	$r3, $r0, 7
beq_cont.3265:
	j	blt_cont.3263
blt_taken.3262:
	addi	$r7, $r0, 8
	addi	$r8, $r0, 8000
	blt	$r8, $r9, blt_taken.3270
	beq	$r8, $r9, beq_taken.3272
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 11
	call	min_caml_div_binary_search
	addi	$r1, $r1, 11
	j	beq_cont.3273
beq_taken.3272:
	addi	$r3, $r0, 8
beq_cont.3273:
	j	blt_cont.3271
blt_taken.3270:
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 11
	call	min_caml_div_binary_search
	addi	$r1, $r1, 11
blt_cont.3271:
blt_cont.3263:
blt_cont.3247:
	muli	$r4, $r3, 1000
	ldi	$r5, $r1, -8
	sub	$r4, $r5, $r4
	sti	$r4, $r1, -10
	ble	$r3, $r0, ble_taken.3274
	addi	$r5, $r0, 48
	add	$r3, $r5, $r3
	subi	$r1, $r1, 12
	call	min_caml_print_char
	addi	$r1, $r1, 12
	addi	$r3, $r0, 1
	j	ble_cont.3275
ble_taken.3274:
	ldi	$r5, $r1, -9
	beq	$r5, $r0, beq_taken.3276
	addi	$r5, $r0, 48
	add	$r3, $r5, $r3
	subi	$r1, $r1, 12
	call	min_caml_print_char
	addi	$r1, $r1, 12
	addi	$r3, $r0, 1
	j	beq_cont.3277
beq_taken.3276:
	addi	$r3, $r0, 0
beq_cont.3277:
ble_cont.3275:
	addi	$r4, $r0, 100
	addi	$r5, $r0, 0
	addi	$r6, $r0, 10
	addi	$r7, $r0, 5
	addi	$r8, $r0, 500
	ldi	$r9, $r1, -10
	sti	$r3, $r1, -11
	blt	$r8, $r9, blt_taken.3278
	beq	$r8, $r9, beq_taken.3280
	addi	$r6, $r0, 2
	addi	$r8, $r0, 200
	blt	$r8, $r9, blt_taken.3282
	beq	$r8, $r9, beq_taken.3284
	addi	$r7, $r0, 1
	addi	$r8, $r0, 100
	blt	$r8, $r9, blt_taken.3286
	beq	$r8, $r9, beq_taken.3288
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 13
	call	min_caml_div_binary_search
	addi	$r1, $r1, 13
	j	beq_cont.3289
beq_taken.3288:
	addi	$r3, $r0, 1
beq_cont.3289:
	j	blt_cont.3287
blt_taken.3286:
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 13
	call	min_caml_div_binary_search
	addi	$r1, $r1, 13
blt_cont.3287:
	j	beq_cont.3285
beq_taken.3284:
	addi	$r3, $r0, 2
beq_cont.3285:
	j	blt_cont.3283
blt_taken.3282:
	addi	$r5, $r0, 3
	addi	$r8, $r0, 300
	blt	$r8, $r9, blt_taken.3290
	beq	$r8, $r9, beq_taken.3292
	addi	$r3, $r9, 0
	addi	$r28, $r6, 0
	addi	$r6, $r5, 0
	addi	$r5, $r28, 0
	subi	$r1, $r1, 13
	call	min_caml_div_binary_search
	addi	$r1, $r1, 13
	j	beq_cont.3293
beq_taken.3292:
	addi	$r3, $r0, 3
beq_cont.3293:
	j	blt_cont.3291
blt_taken.3290:
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 13
	call	min_caml_div_binary_search
	addi	$r1, $r1, 13
blt_cont.3291:
blt_cont.3283:
	j	beq_cont.3281
beq_taken.3280:
	addi	$r3, $r0, 5
beq_cont.3281:
	j	blt_cont.3279
blt_taken.3278:
	addi	$r5, $r0, 7
	addi	$r8, $r0, 700
	blt	$r8, $r9, blt_taken.3294
	beq	$r8, $r9, beq_taken.3296
	addi	$r6, $r0, 6
	addi	$r8, $r0, 600
	blt	$r8, $r9, blt_taken.3298
	beq	$r8, $r9, beq_taken.3300
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 13
	call	min_caml_div_binary_search
	addi	$r1, $r1, 13
	j	beq_cont.3301
beq_taken.3300:
	addi	$r3, $r0, 6
beq_cont.3301:
	j	blt_cont.3299
blt_taken.3298:
	addi	$r3, $r9, 0
	addi	$r28, $r6, 0
	addi	$r6, $r5, 0
	addi	$r5, $r28, 0
	subi	$r1, $r1, 13
	call	min_caml_div_binary_search
	addi	$r1, $r1, 13
blt_cont.3299:
	j	beq_cont.3297
beq_taken.3296:
	addi	$r3, $r0, 7
beq_cont.3297:
	j	blt_cont.3295
blt_taken.3294:
	addi	$r7, $r0, 8
	addi	$r8, $r0, 800
	blt	$r8, $r9, blt_taken.3302
	beq	$r8, $r9, beq_taken.3304
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 13
	call	min_caml_div_binary_search
	addi	$r1, $r1, 13
	j	beq_cont.3305
beq_taken.3304:
	addi	$r3, $r0, 8
beq_cont.3305:
	j	blt_cont.3303
blt_taken.3302:
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 13
	call	min_caml_div_binary_search
	addi	$r1, $r1, 13
blt_cont.3303:
blt_cont.3295:
blt_cont.3279:
	muli	$r4, $r3, 100
	ldi	$r5, $r1, -10
	sub	$r4, $r5, $r4
	sti	$r4, $r1, -12
	ble	$r3, $r0, ble_taken.3306
	addi	$r5, $r0, 48
	add	$r3, $r5, $r3
	subi	$r1, $r1, 14
	call	min_caml_print_char
	addi	$r1, $r1, 14
	addi	$r3, $r0, 1
	j	ble_cont.3307
ble_taken.3306:
	ldi	$r5, $r1, -11
	beq	$r5, $r0, beq_taken.3308
	addi	$r5, $r0, 48
	add	$r3, $r5, $r3
	subi	$r1, $r1, 14
	call	min_caml_print_char
	addi	$r1, $r1, 14
	addi	$r3, $r0, 1
	j	beq_cont.3309
beq_taken.3308:
	addi	$r3, $r0, 0
beq_cont.3309:
ble_cont.3307:
	addi	$r4, $r0, 10
	addi	$r5, $r0, 0
	addi	$r6, $r0, 10
	addi	$r7, $r0, 5
	addi	$r8, $r0, 50
	ldi	$r9, $r1, -12
	sti	$r3, $r1, -13
	blt	$r8, $r9, blt_taken.3310
	beq	$r8, $r9, beq_taken.3312
	addi	$r6, $r0, 2
	addi	$r8, $r0, 20
	blt	$r8, $r9, blt_taken.3314
	beq	$r8, $r9, beq_taken.3316
	addi	$r7, $r0, 1
	addi	$r8, $r0, 10
	blt	$r8, $r9, blt_taken.3318
	beq	$r8, $r9, beq_taken.3320
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 15
	call	min_caml_div_binary_search
	addi	$r1, $r1, 15
	j	beq_cont.3321
beq_taken.3320:
	addi	$r3, $r0, 1
beq_cont.3321:
	j	blt_cont.3319
blt_taken.3318:
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 15
	call	min_caml_div_binary_search
	addi	$r1, $r1, 15
blt_cont.3319:
	j	beq_cont.3317
beq_taken.3316:
	addi	$r3, $r0, 2
beq_cont.3317:
	j	blt_cont.3315
blt_taken.3314:
	addi	$r5, $r0, 3
	addi	$r8, $r0, 30
	blt	$r8, $r9, blt_taken.3322
	beq	$r8, $r9, beq_taken.3324
	addi	$r3, $r9, 0
	addi	$r28, $r6, 0
	addi	$r6, $r5, 0
	addi	$r5, $r28, 0
	subi	$r1, $r1, 15
	call	min_caml_div_binary_search
	addi	$r1, $r1, 15
	j	beq_cont.3325
beq_taken.3324:
	addi	$r3, $r0, 3
beq_cont.3325:
	j	blt_cont.3323
blt_taken.3322:
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 15
	call	min_caml_div_binary_search
	addi	$r1, $r1, 15
blt_cont.3323:
blt_cont.3315:
	j	beq_cont.3313
beq_taken.3312:
	addi	$r3, $r0, 5
beq_cont.3313:
	j	blt_cont.3311
blt_taken.3310:
	addi	$r5, $r0, 7
	addi	$r8, $r0, 70
	blt	$r8, $r9, blt_taken.3326
	beq	$r8, $r9, beq_taken.3328
	addi	$r6, $r0, 6
	addi	$r8, $r0, 60
	blt	$r8, $r9, blt_taken.3330
	beq	$r8, $r9, beq_taken.3332
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 15
	call	min_caml_div_binary_search
	addi	$r1, $r1, 15
	j	beq_cont.3333
beq_taken.3332:
	addi	$r3, $r0, 6
beq_cont.3333:
	j	blt_cont.3331
blt_taken.3330:
	addi	$r3, $r9, 0
	addi	$r28, $r6, 0
	addi	$r6, $r5, 0
	addi	$r5, $r28, 0
	subi	$r1, $r1, 15
	call	min_caml_div_binary_search
	addi	$r1, $r1, 15
blt_cont.3331:
	j	beq_cont.3329
beq_taken.3328:
	addi	$r3, $r0, 7
beq_cont.3329:
	j	blt_cont.3327
blt_taken.3326:
	addi	$r7, $r0, 8
	addi	$r8, $r0, 80
	blt	$r8, $r9, blt_taken.3334
	beq	$r8, $r9, beq_taken.3336
	addi	$r6, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 15
	call	min_caml_div_binary_search
	addi	$r1, $r1, 15
	j	beq_cont.3337
beq_taken.3336:
	addi	$r3, $r0, 8
beq_cont.3337:
	j	blt_cont.3335
blt_taken.3334:
	addi	$r5, $r7, 0
	addi	$r3, $r9, 0
	subi	$r1, $r1, 15
	call	min_caml_div_binary_search
	addi	$r1, $r1, 15
blt_cont.3335:
blt_cont.3327:
blt_cont.3311:
	muli	$r4, $r3, 10
	ldi	$r5, $r1, -12
	sub	$r4, $r5, $r4
	sti	$r4, $r1, -14
	ble	$r3, $r0, ble_taken.3338
	addi	$r5, $r0, 48
	add	$r3, $r5, $r3
	subi	$r1, $r1, 16
	call	min_caml_print_char
	addi	$r1, $r1, 16
	addi	$r3, $r0, 1
	j	ble_cont.3339
ble_taken.3338:
	ldi	$r5, $r1, -13
	beq	$r5, $r0, beq_taken.3340
	addi	$r5, $r0, 48
	add	$r3, $r5, $r3
	subi	$r1, $r1, 16
	call	min_caml_print_char
	addi	$r1, $r1, 16
	addi	$r3, $r0, 1
	j	beq_cont.3341
beq_taken.3340:
	addi	$r3, $r0, 0
beq_cont.3341:
ble_cont.3339:
	addi	$r3, $r0, 48
	ldi	$r4, $r1, -14
	add	$r3, $r3, $r4
	j	min_caml_print_char
blt_taken.3107:
	addi	$r4, $r0, 45
	sti	$r3, $r1, -15
	addi	$r3, $r4, 0
	subi	$r1, $r1, 17
	call	min_caml_print_char
	addi	$r1, $r1, 17
	ldi	$r3, $r1, -15
	muli	$r3, $r3, -1
	j	print_int.457
mul_sub.462:
	beq	$r3, $r0, beq_taken.3342
	beq	$r4, $r0, beq_taken.3344
	addi	$r5, $r0, 0
	j	beq_cont.3345
beq_taken.3344:
	addi	$r5, $r0, 1
beq_cont.3345:
	j	beq_cont.3343
beq_taken.3342:
	addi	$r5, $r0, 1
beq_cont.3343:
	beq	$r5, $r0, beq_taken.3346
	addi	$r3, $r0, 0
	return
beq_taken.3346:
	srai	$r5, $r4, 1
	muli	$r5, $r5, 2
	sub	$r5, $r4, $r5
	beq	$r5, $r0, beq_taken.3347
	muli	$r5, $r3, 2
	srai	$r4, $r4, 1
	beq	$r5, $r0, beq_taken.3348
	beq	$r4, $r0, beq_taken.3350
	addi	$r6, $r0, 0
	j	beq_cont.3351
beq_taken.3350:
	addi	$r6, $r0, 1
beq_cont.3351:
	j	beq_cont.3349
beq_taken.3348:
	addi	$r6, $r0, 1
beq_cont.3349:
	sti	$r3, $r1, 0
	beq	$r6, $r0, beq_taken.3352
	addi	$r3, $r0, 0
	j	beq_cont.3353
beq_taken.3352:
	srai	$r6, $r4, 1
	muli	$r6, $r6, 2
	sub	$r6, $r4, $r6
	beq	$r6, $r0, beq_taken.3354
	muli	$r6, $r5, 2
	srai	$r4, $r4, 1
	sti	$r5, $r1, -1
	addi	$r3, $r6, 0
	subi	$r1, $r1, 3
	call	mul_sub.462
	addi	$r1, $r1, 3
	ldi	$r4, $r1, -1
	add	$r3, $r3, $r4
	j	beq_cont.3355
beq_taken.3354:
	muli	$r5, $r5, 2
	srai	$r4, $r4, 1
	addi	$r3, $r5, 0
	subi	$r1, $r1, 3
	call	mul_sub.462
	addi	$r1, $r1, 3
beq_cont.3355:
beq_cont.3353:
	ldi	$r4, $r1, 0
	add	$r3, $r3, $r4
	return
beq_taken.3347:
	muli	$r3, $r3, 2
	srai	$r4, $r4, 1
	beq	$r3, $r0, beq_taken.3356
	beq	$r4, $r0, beq_taken.3358
	addi	$r5, $r0, 0
	j	beq_cont.3359
beq_taken.3358:
	addi	$r5, $r0, 1
beq_cont.3359:
	j	beq_cont.3357
beq_taken.3356:
	addi	$r5, $r0, 1
beq_cont.3357:
	beq	$r5, $r0, beq_taken.3360
	addi	$r3, $r0, 0
	return
beq_taken.3360:
	srai	$r5, $r4, 1
	muli	$r5, $r5, 2
	sub	$r5, $r4, $r5
	beq	$r5, $r0, beq_taken.3361
	muli	$r5, $r3, 2
	srai	$r4, $r4, 1
	sti	$r3, $r1, -2
	addi	$r3, $r5, 0
	subi	$r1, $r1, 4
	call	mul_sub.462
	addi	$r1, $r1, 4
	ldi	$r4, $r1, -2
	add	$r3, $r3, $r4
	return
beq_taken.3361:
	muli	$r3, $r3, 2
	srai	$r4, $r4, 1
	j	mul_sub.462
mul.465:
	blt	$r4, $r0, blt_taken.3362
	beq	$r3, $r0, beq_taken.3363
	beq	$r4, $r0, beq_taken.3365
	addi	$r5, $r0, 0
	j	beq_cont.3366
beq_taken.3365:
	addi	$r5, $r0, 1
beq_cont.3366:
	j	beq_cont.3364
beq_taken.3363:
	addi	$r5, $r0, 1
beq_cont.3364:
	beq	$r5, $r0, beq_taken.3367
	addi	$r3, $r0, 0
	return
beq_taken.3367:
	srai	$r5, $r4, 1
	muli	$r5, $r5, 2
	sub	$r5, $r4, $r5
	beq	$r5, $r0, beq_taken.3368
	muli	$r5, $r3, 2
	srai	$r4, $r4, 1
	sti	$r3, $r1, 0
	addi	$r3, $r5, 0
	subi	$r1, $r1, 2
	call	mul_sub.462
	addi	$r1, $r1, 2
	ldi	$r4, $r1, 0
	add	$r3, $r3, $r4
	return
beq_taken.3368:
	muli	$r3, $r3, 2
	srai	$r4, $r4, 1
	j	mul_sub.462
blt_taken.3362:
	muli	$r3, $r3, -1
	muli	$r4, $r4, -1
	beq	$r3, $r0, beq_taken.3369
	beq	$r4, $r0, beq_taken.3371
	addi	$r5, $r0, 0
	j	beq_cont.3372
beq_taken.3371:
	addi	$r5, $r0, 1
beq_cont.3372:
	j	beq_cont.3370
beq_taken.3369:
	addi	$r5, $r0, 1
beq_cont.3370:
	beq	$r5, $r0, beq_taken.3373
	addi	$r3, $r0, 0
	return
beq_taken.3373:
	srai	$r5, $r4, 1
	muli	$r5, $r5, 2
	sub	$r5, $r4, $r5
	beq	$r5, $r0, beq_taken.3374
	muli	$r5, $r3, 2
	srai	$r4, $r4, 1
	sti	$r3, $r1, -1
	addi	$r3, $r5, 0
	subi	$r1, $r1, 3
	call	mul_sub.462
	addi	$r1, $r1, 3
	ldi	$r4, $r1, -1
	add	$r3, $r3, $r4
	return
beq_taken.3374:
	muli	$r3, $r3, 2
	srai	$r4, $r4, 1
	j	mul_sub.462
div_binary_search.468:
	add	$r7, $r5, $r6
	srai	$r7, $r7, 1
	mul	$r8, $r7, $r4
	sub	$r9, $r6, $r5
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3375
	blt	$r8, $r3, blt_taken.3376
	beq	$r8, $r3, beq_taken.3377
	add	$r6, $r5, $r7
	srai	$r6, $r6, 1
	mul	$r8, $r6, $r4
	sub	$r9, $r7, $r5
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3378
	blt	$r8, $r3, blt_taken.3379
	beq	$r8, $r3, beq_taken.3380
	add	$r7, $r5, $r6
	srai	$r7, $r7, 1
	mul	$r8, $r7, $r4
	sub	$r9, $r6, $r5
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3381
	blt	$r8, $r3, blt_taken.3382
	beq	$r8, $r3, beq_taken.3383
	add	$r6, $r5, $r7
	srai	$r6, $r6, 1
	mul	$r8, $r6, $r4
	sub	$r9, $r7, $r5
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3384
	blt	$r8, $r3, blt_taken.3385
	beq	$r8, $r3, beq_taken.3386
	j	div_binary_search.468
beq_taken.3386:
	addi	$r3, $r6, 0
	return
blt_taken.3385:
	addi	$r5, $r6, 0
	addi	$r6, $r7, 0
	j	div_binary_search.468
ble_taken.3384:
	addi	$r3, $r5, 0
	return
beq_taken.3383:
	addi	$r3, $r7, 0
	return
blt_taken.3382:
	add	$r5, $r7, $r6
	srai	$r5, $r5, 1
	mul	$r8, $r5, $r4
	sub	$r9, $r6, $r7
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3387
	blt	$r8, $r3, blt_taken.3388
	beq	$r8, $r3, beq_taken.3389
	addi	$r6, $r5, 0
	addi	$r5, $r7, 0
	j	div_binary_search.468
beq_taken.3389:
	addi	$r3, $r5, 0
	return
blt_taken.3388:
	j	div_binary_search.468
ble_taken.3387:
	addi	$r3, $r7, 0
	return
ble_taken.3381:
	addi	$r3, $r5, 0
	return
beq_taken.3380:
	addi	$r3, $r6, 0
	return
blt_taken.3379:
	add	$r5, $r6, $r7
	srai	$r5, $r5, 1
	mul	$r8, $r5, $r4
	sub	$r9, $r7, $r6
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3390
	blt	$r8, $r3, blt_taken.3391
	beq	$r8, $r3, beq_taken.3392
	add	$r7, $r6, $r5
	srai	$r7, $r7, 1
	mul	$r8, $r7, $r4
	sub	$r9, $r5, $r6
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3393
	blt	$r8, $r3, blt_taken.3394
	beq	$r8, $r3, beq_taken.3395
	addi	$r5, $r6, 0
	addi	$r6, $r7, 0
	j	div_binary_search.468
beq_taken.3395:
	addi	$r3, $r7, 0
	return
blt_taken.3394:
	addi	$r6, $r5, 0
	addi	$r5, $r7, 0
	j	div_binary_search.468
ble_taken.3393:
	addi	$r3, $r6, 0
	return
beq_taken.3392:
	addi	$r3, $r5, 0
	return
blt_taken.3391:
	add	$r6, $r5, $r7
	srai	$r6, $r6, 1
	mul	$r8, $r6, $r4
	sub	$r9, $r7, $r5
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3396
	blt	$r8, $r3, blt_taken.3397
	beq	$r8, $r3, beq_taken.3398
	j	div_binary_search.468
beq_taken.3398:
	addi	$r3, $r6, 0
	return
blt_taken.3397:
	addi	$r5, $r6, 0
	addi	$r6, $r7, 0
	j	div_binary_search.468
ble_taken.3396:
	addi	$r3, $r5, 0
	return
ble_taken.3390:
	addi	$r3, $r6, 0
	return
ble_taken.3378:
	addi	$r3, $r5, 0
	return
beq_taken.3377:
	addi	$r3, $r7, 0
	return
blt_taken.3376:
	add	$r5, $r7, $r6
	srai	$r5, $r5, 1
	mul	$r8, $r5, $r4
	sub	$r9, $r6, $r7
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3399
	blt	$r8, $r3, blt_taken.3400
	beq	$r8, $r3, beq_taken.3401
	add	$r6, $r7, $r5
	srai	$r6, $r6, 1
	mul	$r8, $r6, $r4
	sub	$r9, $r5, $r7
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3402
	blt	$r8, $r3, blt_taken.3403
	beq	$r8, $r3, beq_taken.3404
	add	$r5, $r7, $r6
	srai	$r5, $r5, 1
	mul	$r8, $r5, $r4
	sub	$r9, $r6, $r7
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3405
	blt	$r8, $r3, blt_taken.3406
	beq	$r8, $r3, beq_taken.3407
	addi	$r6, $r5, 0
	addi	$r5, $r7, 0
	j	div_binary_search.468
beq_taken.3407:
	addi	$r3, $r5, 0
	return
blt_taken.3406:
	j	div_binary_search.468
ble_taken.3405:
	addi	$r3, $r7, 0
	return
beq_taken.3404:
	addi	$r3, $r6, 0
	return
blt_taken.3403:
	add	$r7, $r6, $r5
	srai	$r7, $r7, 1
	mul	$r8, $r7, $r4
	sub	$r9, $r5, $r6
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3408
	blt	$r8, $r3, blt_taken.3409
	beq	$r8, $r3, beq_taken.3410
	addi	$r5, $r6, 0
	addi	$r6, $r7, 0
	j	div_binary_search.468
beq_taken.3410:
	addi	$r3, $r7, 0
	return
blt_taken.3409:
	addi	$r6, $r5, 0
	addi	$r5, $r7, 0
	j	div_binary_search.468
ble_taken.3408:
	addi	$r3, $r6, 0
	return
ble_taken.3402:
	addi	$r3, $r7, 0
	return
beq_taken.3401:
	addi	$r3, $r5, 0
	return
blt_taken.3400:
	add	$r7, $r5, $r6
	srai	$r7, $r7, 1
	mul	$r8, $r7, $r4
	sub	$r9, $r6, $r5
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3411
	blt	$r8, $r3, blt_taken.3412
	beq	$r8, $r3, beq_taken.3413
	add	$r6, $r5, $r7
	srai	$r6, $r6, 1
	mul	$r8, $r6, $r4
	sub	$r9, $r7, $r5
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3414
	blt	$r8, $r3, blt_taken.3415
	beq	$r8, $r3, beq_taken.3416
	j	div_binary_search.468
beq_taken.3416:
	addi	$r3, $r6, 0
	return
blt_taken.3415:
	addi	$r5, $r6, 0
	addi	$r6, $r7, 0
	j	div_binary_search.468
ble_taken.3414:
	addi	$r3, $r5, 0
	return
beq_taken.3413:
	addi	$r3, $r7, 0
	return
blt_taken.3412:
	add	$r5, $r7, $r6
	srai	$r5, $r5, 1
	mul	$r8, $r5, $r4
	sub	$r9, $r6, $r7
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3417
	blt	$r8, $r3, blt_taken.3418
	beq	$r8, $r3, beq_taken.3419
	addi	$r6, $r5, 0
	addi	$r5, $r7, 0
	j	div_binary_search.468
beq_taken.3419:
	addi	$r3, $r5, 0
	return
blt_taken.3418:
	j	div_binary_search.468
ble_taken.3417:
	addi	$r3, $r7, 0
	return
ble_taken.3411:
	addi	$r3, $r5, 0
	return
ble_taken.3399:
	addi	$r3, $r7, 0
	return
ble_taken.3375:
	addi	$r3, $r5, 0
	return
div_sub:
	muli	$r6, $r4, 2
	sti	$r4, $r1, 0
	sti	$r5, $r1, -1
	sti	$r3, $r1, -2
	blt	$r5, $r0, blt_taken.3420
	addi	$r4, $r5, 0
	addi	$r3, $r6, 0
	subi	$r1, $r1, 4
	call	mul_sub.462
	addi	$r1, $r1, 4
	j	blt_cont.3421
blt_taken.3420:
	muli	$r6, $r6, -1
	muli	$r7, $r5, -1
	addi	$r4, $r7, 0
	addi	$r3, $r6, 0
	subi	$r1, $r1, 4
	call	mul_sub.462
	addi	$r1, $r1, 4
blt_cont.3421:
	ldi	$r4, $r1, -2
	ble	$r3, $r4, ble_taken.3422
	ldi	$r3, $r1, -1
	muli	$r6, $r3, 2
	add	$r5, $r3, $r6
	srai	$r5, $r5, 1
	ldi	$r7, $r1, 0
	mul	$r8, $r5, $r7
	sub	$r9, $r6, $r3
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3423
	blt	$r8, $r4, blt_taken.3424
	beq	$r8, $r4, beq_taken.3425
	add	$r6, $r3, $r5
	srai	$r6, $r6, 1
	mul	$r8, $r6, $r7
	sub	$r9, $r5, $r3
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3426
	blt	$r8, $r4, blt_taken.3427
	beq	$r8, $r4, beq_taken.3428
	add	$r5, $r3, $r6
	srai	$r5, $r5, 1
	mul	$r8, $r5, $r7
	sub	$r9, $r6, $r3
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3429
	blt	$r8, $r4, blt_taken.3430
	beq	$r8, $r4, beq_taken.3431
	addi	$r6, $r5, 0
	addi	$r5, $r3, 0
	addi	$r3, $r4, 0
	addi	$r4, $r7, 0
	j	div_binary_search.468
beq_taken.3431:
	addi	$r3, $r5, 0
	return
blt_taken.3430:
	addi	$r3, $r4, 0
	addi	$r4, $r7, 0
	j	div_binary_search.468
ble_taken.3429:
	return
beq_taken.3428:
	addi	$r3, $r6, 0
	return
blt_taken.3427:
	add	$r3, $r6, $r5
	srai	$r3, $r3, 1
	mul	$r8, $r3, $r7
	sub	$r9, $r5, $r6
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3432
	blt	$r8, $r4, blt_taken.3433
	beq	$r8, $r4, beq_taken.3434
	addi	$r5, $r6, 0
	addi	$r6, $r3, 0
	addi	$r3, $r4, 0
	addi	$r4, $r7, 0
	j	div_binary_search.468
beq_taken.3434:
	return
blt_taken.3433:
	addi	$r6, $r5, 0
	addi	$r5, $r3, 0
	addi	$r3, $r4, 0
	addi	$r4, $r7, 0
	j	div_binary_search.468
ble_taken.3432:
	addi	$r3, $r6, 0
	return
ble_taken.3426:
	return
beq_taken.3425:
	addi	$r3, $r5, 0
	return
blt_taken.3424:
	add	$r3, $r5, $r6
	srai	$r3, $r3, 1
	mul	$r8, $r3, $r7
	sub	$r9, $r6, $r5
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3435
	blt	$r8, $r4, blt_taken.3436
	beq	$r8, $r4, beq_taken.3437
	add	$r6, $r5, $r3
	srai	$r6, $r6, 1
	mul	$r8, $r6, $r7
	sub	$r9, $r3, $r5
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3438
	blt	$r8, $r4, blt_taken.3439
	beq	$r8, $r4, beq_taken.3440
	addi	$r3, $r4, 0
	addi	$r4, $r7, 0
	j	div_binary_search.468
beq_taken.3440:
	addi	$r3, $r6, 0
	return
blt_taken.3439:
	addi	$r5, $r6, 0
	addi	$r6, $r3, 0
	addi	$r3, $r4, 0
	addi	$r4, $r7, 0
	j	div_binary_search.468
ble_taken.3438:
	addi	$r3, $r5, 0
	return
beq_taken.3437:
	return
blt_taken.3436:
	add	$r5, $r3, $r6
	srai	$r5, $r5, 1
	mul	$r8, $r5, $r7
	sub	$r9, $r6, $r3
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3441
	blt	$r8, $r4, blt_taken.3442
	beq	$r8, $r4, beq_taken.3443
	addi	$r6, $r5, 0
	addi	$r5, $r3, 0
	addi	$r3, $r4, 0
	addi	$r4, $r7, 0
	j	div_binary_search.468
beq_taken.3443:
	addi	$r3, $r5, 0
	return
blt_taken.3442:
	addi	$r3, $r4, 0
	addi	$r4, $r7, 0
	j	div_binary_search.468
ble_taken.3441:
	return
ble_taken.3435:
	addi	$r3, $r5, 0
	return
ble_taken.3423:
	return
ble_taken.3422:
	ldi	$r3, $r1, -1
	muli	$r3, $r3, 2
	ldi	$r5, $r1, 0
	muli	$r6, $r5, 2
	sti	$r3, $r1, -3
	addi	$r4, $r3, 0
	addi	$r3, $r6, 0
	subi	$r1, $r1, 5
	call	mul.465
	addi	$r1, $r1, 5
	ldi	$r4, $r1, -2
	ble	$r3, $r4, ble_taken.3444
	ldi	$r3, $r1, -3
	muli	$r6, $r3, 2
	add	$r5, $r3, $r6
	srai	$r5, $r5, 1
	ldi	$r7, $r1, 0
	mul	$r8, $r5, $r7
	sub	$r9, $r6, $r3
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3445
	blt	$r8, $r4, blt_taken.3446
	beq	$r8, $r4, beq_taken.3447
	add	$r6, $r3, $r5
	srai	$r6, $r6, 1
	mul	$r8, $r6, $r7
	sub	$r9, $r5, $r3
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3448
	blt	$r8, $r4, blt_taken.3449
	beq	$r8, $r4, beq_taken.3450
	addi	$r5, $r3, 0
	addi	$r3, $r4, 0
	addi	$r4, $r7, 0
	j	div_binary_search.468
beq_taken.3450:
	addi	$r3, $r6, 0
	return
blt_taken.3449:
	addi	$r3, $r4, 0
	addi	$r4, $r7, 0
	addi	$r28, $r6, 0
	addi	$r6, $r5, 0
	addi	$r5, $r28, 0
	j	div_binary_search.468
ble_taken.3448:
	return
beq_taken.3447:
	addi	$r3, $r5, 0
	return
blt_taken.3446:
	add	$r3, $r5, $r6
	srai	$r3, $r3, 1
	mul	$r8, $r3, $r7
	sub	$r9, $r6, $r5
	addi	$r10, $r0, 1
	ble	$r9, $r10, ble_taken.3451
	blt	$r8, $r4, blt_taken.3452
	beq	$r8, $r4, beq_taken.3453
	addi	$r6, $r3, 0
	addi	$r3, $r4, 0
	addi	$r4, $r7, 0
	j	div_binary_search.468
beq_taken.3453:
	return
blt_taken.3452:
	addi	$r5, $r3, 0
	addi	$r3, $r4, 0
	addi	$r4, $r7, 0
	j	div_binary_search.468
ble_taken.3451:
	addi	$r3, $r5, 0
	return
ble_taken.3445:
	return
ble_taken.3444:
	ldi	$r3, $r1, -3
	muli	$r5, $r3, 2
	ldi	$r3, $r1, 0
	addi	$r28, $r4, 0
	addi	$r4, $r3, 0
	addi	$r3, $r28, 0
	j	div_sub

# $r3: devidee $r4: devider
min_caml_div:
	beq $r4, $r0, zero_div
	# when a is larger than 0x40000000, it is possible to overflow
	# round answer
	# 0x3fff ffff
	mvhi $r5, 16384
	mvlo $r5, 0
	blt $r3, $r5, start_div
	srai $r3, $r3, 1
	srai $r4, $r4, 1
start_div:
	sti	$r3, $r1, 0
	sti	$r4, $r1, -1
	add	$r3, $r3, $r0
	add	$r4, $r4, $r0
	blt	$r0, $r3, skip_invert_devidee
	sub $r3, $r0, $r3
skip_invert_devidee:
	blt $r0, $r4, skip_invert_devider
	sub $r4, $r0, $r4
skip_invert_devider:
	blt	$r3, $r4, zero_div
	addi	$r5, $r0, 1
	subi	$r1, $r1, 2
	call div_sub
	addi	$r1, $r1, 2
	ldi	$r5, $r1, 0
	ldi	$r6, $r1, -1
	# fix sign
	blt $r5, $r0, negative_devidee
	# positive_devidee
	blt $r6, $r0, ans_invert
	j ans_direct
negative_devidee:
	blt $r6, $r0, ans_direct
	j ans_invert
ans_invert:
	sub	$r3, $r0, $r3
ans_direct:
	return
# also used when a < b
zero_div:
	addi $r3, $r0, 0
	return
