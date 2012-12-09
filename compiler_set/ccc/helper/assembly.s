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
	sub	$r3, $r0, $r3
skip_invert_devidee:
	blt	$r0, $r4, skip_invert_devider
	nop
	nop
	sub	$r4, $r0, $r4
skip_invert_devider:
	blt	$r3, $r4, zero_div
	nop
	nop
	addi	$r5, $r0, 1
	subi	$r1, $r1, 2
	call	div_sub
	addi	$r1, $r1, 2
	ldi	$r5, $r1, 0
	ldi	$r6, $r1, -1
	# fix sign
	blt	$r5, $r0, negative_devidee
	nop
	nop
	# positive_devidee
	blt	$r6, $r0, ans_invert
	nop
	nop
	j ans_direct
negative_devidee:
	blt	$r6, $r0, ans_direct
	nop
	nop
	j	ans_invert
ans_invert:
	sub	$r3, $r0, $r3
ans_direct:
	return
# also used when a < b
zero_div:
	addi	$r3, $r0, 0
	return

min_caml_mod:
	sti	$r3, $r1, 0
	sti	$r4, $r1, -1
	subi	$r1, $r1, 2
	call	min_caml_div
	addi	$r1, $r1, 2
	ldi	$r4, $r1, -1
	subi	$r1, $r1, 2
	call	min_caml_mul
	addi	$r1, $r1, 2
	ldi	$r4, $r1, 0
	sub	$r3, $r4, $r3

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
	ldr	$r8, $r3, $r5
	outputb	$r8
	addi	$r5, $r5, 1
	blt	$r5, $r4, send_rs_start
	nop
	nop
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
