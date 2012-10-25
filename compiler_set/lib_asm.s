#----------------------------------------------------------------------
#
# lib_asm.s
#
#----------------------------------------------------------------------

# * create_array
min_caml_create_array:
	add	$r5, $r3, $r2
	addi	$r3, $r2, 0
CREATE_ARRAY_LOOP:
	blt	$r2, $r5, CREATE_ARRAY_CONTINUE
	return
CREATE_ARRAY_CONTINUE:
	sti	$r4, $r2, 0	
	addi	$r2, $r2, 1
	j CREATE_ARRAY_LOOP

# * create_float_array
min_caml_create_float_array:
	add	$r4, $r3, $r2
	addi	$r3, $r2, 0
CREATE_FLOAT_ARRAY_LOOP:
	blt	$r2, $r4, CREATE_FLOAT_ARRAY_CONTINUE
	return
CREATE_FLOAT_ARRAY_CONTINUE:
	fsti	$f0, $r2, 0
	addi	$r2, $r2, 1
	j CREATE_FLOAT_ARRAY_LOOP

	
# * floor		$f0 + MAGICF - MAGICF
min_caml_floor:
	# $f4 <- 0.0
	imovf $f4, $r0
	# if (0 <= $f0) goto FLOOR_POSITIVE
	fblt $f0, $f4, FLOOR_NEGATIVE	
FLOOR_POSITIVE:
	# $f2 <- 8388608.0 = 2^23 (0x4b000000)
	fmvhi $f2, 19200
	fmvlo $f2, 0
	fblt $f2, $f0, FLOOR_POSITIVE_RET
FLOOR_POSITIVE_MAIN:
	fmov $f1, $f0
	fadd $f0, $f0, $f2
	fsub $f0, $f0, $f2
	fblt $f1, $f0, FLOOR_POSITIVE_RET
	return
FLOOR_POSITIVE_RET:
	# $f3 <- 1.0
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
	fmov $f1, $f0
	fadd $f0, $f0, $f2
	fsub $f0, $f0, $f2
	fblt $f0, $f1, FLOOR_NEGATIVE_PRE_RET
	j FLOOR_NEGATIVE_RET
FLOOR_NEGATIVE_PRE_RET:
	fadd $f0, $f0, $f2
	# $f3 <- 1.0
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
	# if ($r0 <= $r3) goto ITOF_MAIN
	blt $r3, $r0, ITOF_NEGATIVE_MAIN		
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
	imovf $f2, $r0
ITOF_LOOP:
	sub $r3, $r3, $r5
	fadd $f2, $f2, $f1
	blt $r3, $r5, ITOF_RET
	j ITOF_LOOP
ITOF_RET:
	add $r3, $r3, $r4
	imovf $f0, $r3
	fsub $f0, $f0, $f1
	fadd $f0, $f0, $f2
	return
ITOF_SMALL:
	add $r3, $r3, $r4
	imovf $f0, $r3
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
	imovf $f1, $r0
	# if (0.0 <= $f0) goto FTOI_MAIN
	fblt $f0, $f1, FTOI_NEGATIVE_MAIN			
FTOI_POSITIVE_MAIN:
	# $f2 <- 8388608.0(0x4b000000)
	fmvhi $f2, 19200
	fmvlo $f2, 0
	# $r4 <- 0x4b000000
	mvhi $r4, 19200
	mvlo $r4, 0
	# if (MAGICF <= $f0) goto FTOI_BIG
	fblt $f0, $f2, FTOI_SMALL		
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
	
# *read_int
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
	imovf $f0, $r3
	return

#----------------------------------------------------------------------
#
# lib_asm.s
#
#----------------------------------------------------------------------

atan_sub:
	fblt	$f3, $f6, ATAN_RETURN
	fmul	$f7, $f3, $f3
	fmul	$f7, $f7, $f4
	fadd	$f5, $f5, $f1
	fadd	$f5, $f5, $f3
	fadd	$f5, $f5, $f3
	finv	$f5, $f5
	fsub	$f3, $f3, $f1
	fmul	$f5, $f5, $f7
	j	atan_sub
ATAN_RETURN:
	return
min_caml_atan:
	# $f1 <- 1.0
	fmvhi	$f1, 16256
	fmvlo	$f1, 0
	# $f2 <- -1.0
	fneg	$f2, $f1
	# $f3 <- 11.0
	fmvhi	$f3, 16688
	fmvlo	$f3, 0
	# $f5 <- 0.0
	imovf	$f5, $r0
	# $f6 <- 0.5
	fmvhi	$f6, 16128
	fmvlo	$f6, 0
	
	fblt	$f1, $f0, GTONE
	fblt	$f0, $f2, LTMONE
	# when -1.0 <= $f1 <= 1,0
	fmul	$f4, $f0, $f0
	call	atan_sub
	fadd	$f5, $f5, $f1
	finv	$f5, $f5
	fmul	$f0, $f0, $f5
	return
GTONE:
	finv	$f0, $f0
	fmul	$f4, $f0, $f0
	call	atan_sub
	fadd	$f5, $f5, $f1
	finv	$f5, $f5
	fmul	$f8, $f0, $f5
	# $f0 <- pi/2
	fmvlo	$f0, 4059
	fmvhi	$f0, 16329
	fsub	$f0, $f0, $f8
	return
LTMONE:
	finv	$f0, $f0
	fmul	$f4, $f0, $f0
	call	atan_sub
	fadd	$f5, $f5, $f1
	finv	$f5, $f5
	fmul	$f8, $f0, $f5
	# $f0 <- -pi/2
	fmvlo	$f0, 4059
	fmvhi	$f0, 49097
	fsub	$f0, $f0, $f8
	return

