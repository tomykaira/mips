#EXPECTED: 1 0 0 41
	nop
	j min_caml_start
min_caml_start:
	fmvhi $f3, 16384	# 2.0
	fadd $f4, $f3, $f0 # 2.0
	fadd $f5, $f4, $f3 # 4.0
	fmul $f7, $f5, $f3 # 8.0
	nop
	nop
	nop
	fmul $f8, $f7, $f7 # 64.0
	fsqrt $f9, $f8 # 8.0
	fmovi $r3, $f9
	outputb $r3 # 01
	srai $r3, $r3, 8
	outputb $r3 # 0
	srai $r3, $r3, 8
	outputb $r3 # 0
	srai $r3, $r3, 8
	outputb $r3 # 41
	halt
