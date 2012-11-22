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

INPUT=  -f $(SLD_PATH)

architecture:
	cd assembler; make
	cd simulator; make
	cd compiler; make native-code

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
#	eog $(LOR_PPM_PATH) & 2> /dev/null
	make -s ./test/Loreley/main.run > $(LOR_PPM_PATH)

lor-run_f:
	touch $(LOR_PPM_PATH)
#	eog $(LOR_PPM_PATH) & 2> /dev/null
	make -s ./test/Loreley/main.run > $(LOR_PPM_PATH)

# min-rt.s, min-rt.binを削除
lor-clean:
	rm -rf $(LOR_PPM_PATH) $(LORELEY_DIR)/main.s $(LORELEY_DIR)/main.bin

# not run: sum ack
.PHONY: test test-simple test-comp test-large

test: test-simple test-comp test-large
test-simple: $(TESTS:%=test/%.run_f)
test-comp: $(MEMOIZE_TESTS:%=test/%.run_cmp)
test-large: $(LARGE_TESTS:%=test/%.run_large)

test-core: $(TESTS:%=test/%.run_core) $(MEMOIZE_TESTS:%=test/%.run_core_cmp) $(LARGE_TESTS:%=test/%.run_core_cmp)

%.run_core: %.sim %.core
	diff $*.sim $*.core

%.run_core_cmp: %.core
	diff $*.ans $*.core

# .bin, .sが存在しないときのみビルドして実行
%.run:
	make $*.bin
	simulator/simulator -t $(INPUT) $*.bin

# .bin, .sが存在しててもビルドした上で実行する
%.run_f:
	make $*.bin_f
	simulator/simulator -t $*.bin > /tmp/`basename $*`.out && \
		ocaml $*.ml > /tmp/`basename $*`.ans && \
		diff /tmp/`basename $*`.out /tmp/`basename $*`.ans

%.run_cmp:
	make $*.bin_f
	simulator/simulator -t $*.bin > /tmp/`basename $*`.out && \
		diff /tmp/`basename $*`.out $*.ans

%.run_large:
	make $*.bin_f
	simulator/simulator -t $*.bin > /tmp/`basename $*`.out && \
		diff /tmp/`basename $*`.out $*.ans


# .bin, .sが存在しないときのみビルド
%.bin: %.s
	assembler/assembler $*.s $*.bin

# .bin, .sが存在しててもビルドする
%.bin_f:
	cat $(LIB_ML) $*.ml > __tmp__.ml
	$(MIN_CAML) $(OPTION) $(BINARY) $(INLINE) __tmp__
	cd linker; $(LINKER) ../$(LIB_ASM) ../__tmp__.s ${abspath $*.s}
	assembler/assembler $*.s $*.bin


# .sが存在しててもビルド
%.s_f:
	cat $(LIB_ML) $*.ml > __tmp__.ml
	$(MIN_CAML) $(OPTION) $(BINARY) $(INLINE) __tmp__
	cd linker; $(LINKER) ../$(LIB_ASM) ../__tmp__.s ${abspath $*.s}
