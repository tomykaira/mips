# min-rt の実行は test_runner.rb min-rt か cd test/min-rt; make min-rt-new.test で

LORELEY_DIR=test/Loreley

BINARY = -b
INLINE= -inline 50 

# min-camlのライブラリファイル
LIB_ML = lib_ml.ml

# アセンブリのライブラリファイル
LIB_ASM = lib_asm.s

LOR_PATH=$(LORELEY_DIR)/Simple/simple1.lor
LOR_PPM_PATH=$(LOR_PATH).ppm

MIN_CAML=./compiler/min-caml
LINKER=groovy linker.groovy

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

%.s: %.ml
	cat $(LIB_ML) $*.ml > __tmp__.ml
	$(MIN_CAML) $(OPTION) $(BINARY) $(INLINE) __tmp__
	cd linker; $(LINKER) ../$(LIB_ASM) ../__tmp__.s ${abspath $*.s}

min-rt-test:
	cd test/min-rt; make BIN=-x min-rt-new-bin.test; cd ../..
