# min-camlのライブラリファイル
LIB_ML = lib_ml.ml

# アセンブリのライブラリファイル
LIB_ASM = lib_asm.s

OPTION = 
# 入力ファイルをテキストとして扱う
# min-rtで、test/min-rt/sld以下の.sldファイルを入力ファイルとしたいときは
# 普通はこっちにしとけば大体間違いはない
BINARY =

# 入力ファイルをバイナリとして扱う
# min-rtで、test/min-rt/sldbin以下の.slbinファイルを入力ファイルとしたいときはこっちを使う
#BINARY = -b

# インライン展開をどの程度行うか
# 値が大きいほどインライン展開がより深くなされる
# 値は250くらいがちょうど良い
# それ以上は徒にアセンブリファイルのサイズが増えるだけで命令数はほとんど減らない
INLINE = -inline 50

LINKER=groovy linker.groovy

# globals.ml, min-rt.ml があるディレクトリ
MIN_RT_DIR = test/min-rt/

LORELEY_DIR=test/Loreley

# min-rtを実行するときの入力ファイル。上で"BINARY ="としたときはこっちを使う
SLD_PATH = test/min-rt/sld/contest.sld

# min-rtを実行するときの入力ファイル。上で"BINARY = -b"としたときはこっちを使う
#SLD_PATH = test/min-rt/sldbin/contest.sldbin

# min-rtを実行するときの出力ファイル
PPM_PATH = test/min-rt/contest.ppm

LOR_PATH=$(LORELEY_DIR)/Simple/simple1.lor
LOR_PPM_PATH=$(LOR_PATH).ppm

MIN_CAML=BISECT_FILE=compiler/coverage compiler/min-caml

INPUT= -f $(LOR_PATH)

architecture:
	cd assembler; make
	cd simulator; make
	cd compiler; make br

tools:
	cd assembler; make
	cd simulator; make

architecture-clean:
	cd assembler; make clean
	cd simulator; make clean
	cd compiler; make clean

report:
	cd compiler; bisect-report -html report coverage*.out

#--------------------------------------------------------------------
# min-rtのコンパイル・実行
#--------------------------------------------------------------------

# globals.mlとmin-rt.mlをコンパイル。min-rt.sとmin-rt.binを作る
min-rt:
	cat $(LIB_ML) $(MIN_RT_DIR)globals.ml $(MIN_RT_DIR)min-rt-new.ml > __tmp__.ml
	$(MIN_CAML) $(BINARY) $(INLINE) __tmp__
	cd linker; $(LINKER) ../$(LIB_ASM) ../__tmp__.s ${abspath $(MIN_RT_DIR)min-rt.s}
	assembler/assembler $(MIN_RT_DIR)min-rt.s $(MIN_RT_DIR)min-rt.bin

# min-rt.binを実行
min-rt-run:
	make min-rt
	touch $(PPM_PATH)
	eog $(PPM_PATH) & 2> /dev/null
	make -s ./test/min-rt/min-rt.run | tail -c +2  > $(PPM_PATH)

min-rt-run_f:
	assembler/assembler $(MIN_RT_DIR)min-rt.s $(MIN_RT_DIR)min-rt.bin
	touch $(PPM_PATH)
	eog $(PPM_PATH) & 2> /dev/null
	make -s ./test/min-rt/min-rt.run | tail -c +2  > $(PPM_PATH)

# min-rt.s, min-rt.binを削除
min-rt-clean:
	rm $(MIN_RT_DIR)min-rt.s
	rm $(MIN_RT_DIR)min-rt.bin

#--------------------------------------------------------------------
# loreley のコンパイル・実行
#--------------------------------------------------------------------

# globals.mlとmin-rt.mlをコンパイル。min-rt.sとmin-rt.binを作る
lor:
	cat $(LIB_ML) $(LORELEY_DIR)/main.ml > __tmp__.ml
	$(MIN_CAML) $(OPTION) $(BINARY) $(INLINE) __tmp__
	cd linker; $(LINKER) ../$(LIB_ASM) ../__tmp__.s ${abspath $(LORELEY_DIR)/main.s}
	assembler/assembler $(LORELEY_DIR)/main.s $(LORELEY_DIR)/main.bin

# min-rt.binを実行
lor-run:
	make lor
	touch $(LOR_PPM_PATH)
	eog $(LOR_PPM_PATH) & 2> /dev/null
	make -s ./test/Loreley/main.run > $(LOR_PPM_PATH)

lor-run_f:
	touch $(LOR_PPM_PATH)
	eog $(LOR_PPM_PATH) & 2> /dev/null
	make -s ./test/Loreley/main.run > $(LOR_PPM_PATH)

# min-rt.s, min-rt.binを削除
lor-clean:
	rm -rf $(LOR_PPM_PATH) $(LORELEY_DIR)/main.s $(LORELEY_DIR)/main.bin

#--------------------------------------------------------------------
# その他ファイルのコンパイル
#--------------------------------------------------------------------

TESTS=adder and array_init cls-bug2 cls-bug cls-rec double_equal even-odd fib \
funcomp gcd join-reg join-reg2 join-stack join-stack2 \
join-stack3 let-array let-fun list list2 list-float matmul \
matmul-flat multest nest-tuple non-tail-if2 pattern \
print-char print ref ref2 shuffle spill spill2 spill3 \
sum-tail tuple-fv tuple2 tuple3 tuple-return

# tests whose result can be different because of FPU, or
# ocaml cannot process
MEMOIZE_TESTS=float fmul log-exp hyperbolic inprod-loop inprod-rec \
non-tail-if power random

# tests use input file
INPUT_TESTS=fib_input

LARGE_TESTS=hanabi mandelbrot

# not run: sum ack
.PHONY: test test-simple test-comp test-large

test: test-simple test-comp test-large
test-simple: $(TESTS:%=test/%.run_f)
test-comp: $(MEMOIZE_TESTS:%=test/%.run_cmp)
test-large: $(LARGE_TESTS:%=test/%.run_large)

# .bin, .sが存在しないときのみビルドして実行
%.run:
	make $*.bin
	simulator/simulator $(INPUT) $*.bin

# .bin, .sが存在しててもビルドした上で実行する
%.run_f:
	make $*.bin_f
	simulator/simulator $*.bin | tee /tmp/`basename $*`.out && \
		ocaml $*.ml | tee /tmp/`basename $*`.ans && \
		diff /tmp/`basename $*`.out /tmp/`basename $*`.ans

%.run_cmp:
	make $*.bin_f
	simulator/simulator $*.bin | tee /tmp/`basename $*`.out && \
		diff /tmp/`basename $*`.out $*.ans

%.ans:
	make $*.bin_f
	simulator/simulator $*.bin 2>/dev/null | tee $*.ans
	ocaml $*.ml

%.run_large:
	make $*.bin_f
	simulator/simulator $*.bin > /tmp/`basename $*`.out && \
		diff /tmp/`basename $*`.out $*.ans

# .bin, .sが存在しないときのみビルド
%.bin:
	cat $(LIB_ML) $*.ml > __tmp__.ml
	$(MIN_CAML) $(OPTION) $(BINARY) $(INLINE) __tmp__
	cd linker; $(LINKER) ../$(LIB_ASM) ../__tmp__.s ${abspath $*.s}
	assembler/assembler $*.s $*.bin

# .bin, .sが存在しててもビルドする
%.bin_f:
	cat $(LIB_ML) $*.ml > __tmp__.ml
	$(MIN_CAML) $(OPTION) $(BINARY) $(INLINE) __tmp__
	cd linker; $(LINKER) ../$(LIB_ASM) ../__tmp__.s ${abspath $*.s}
	assembler/assembler $*.s $*.bin

# .sが存在しないときのみビルド
%.s:
	cat $(LIB_ML) $*.ml > __tmp__.ml
	$(MIN_CAML) $(OPTION) $(BINARY) $(INLINE) __tmp__
	cd linker; $(LINKER) ../$(LIB_ASM) ../__tmp__.s ${abspath $*.s}

# .sが存在しててもビルド
%.s_f:
	cat $(LIB_ML) $*.ml > __tmp__.ml
	$(MIN_CAML) $(OPTION) $(BINARY) $(INLINE) __tmp__
	cd linker; $(LINKER) ../$(LIB_ASM) ../__tmp__.s ${abspath $*.s}

# MLがソースファイルのとき
%.clean_ml:
	rm $*.s
	rm $*.bin

# アセンブリがソースファイルのとき。.sを削除したらまずい
%.clean_s:
	rm $*.bin

.PHONY: cur

cur: 
	make tools
	assembler/assembler test/Loreley/main.s test/Loreley/main.bin
