# 関係のないメモリに連続で load -> store したとき動作するか
	nop
  addi $r3, $r0, 65
	addi $r4, $r0, 66
	sti $r3, $r3, 0
	nop
	nop
# 準備完了
	ldi $r5, $r3, 0
	sti $r4, $r4, 1
	nop
	nop
	ldi $r6, $r4, 1
	outputb $r5
	outputb	$r6
# expected: AB
# sim: 'Z'B
	halt
