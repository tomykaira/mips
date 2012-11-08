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
INLINE = --inline 50

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

architecture:
	cd assembler; make
	cd simulator; make
	cd compiler; make

tools:
	cd assembler; make
	cd simulator; make

architecture-clean:
	cd assembler; make clean
	cd simulator; make clean
	cd compiler; make clean

#--------------------------------------------------------------------
# min-rtのコンパイル・実行
#--------------------------------------------------------------------

# globals.mlとmin-rt.mlをコンパイル。min-rt.sとmin-rt.binを作る
min-rt:
	cat $(LIB_ML) $(MIN_RT_DIR)globals.ml $(MIN_RT_DIR)min-rt-new.ml > __tmp__.ml
	compiler/min-caml $(BINARY) $(INLINE) __tmp__
	cd linker; $(LINKER) ../$(LIB_ASM) ../__tmp__.s ${abspath $(MIN_RT_DIR)min-rt.s}
	assembler/assembler $(MIN_RT_DIR)min-rt.s $(MIN_RT_DIR)min-rt.bin

# min-rt.binを実行
min-rt-run:
	make min-rt
	touch $(PPM_PATH)
	eog $(PPM_PATH) & 2> /dev/null
	make -s ./test/min-rt/min-rt.run < $(SLD_PATH) > $(PPM_PATH)

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
	compiler/min-caml $(BINARY) $(INLINE) __tmp__
	cd linker; $(LINKER) ../$(LIB_ASM) ../__tmp__.s ${abspath $(LORELEY_DIR)/main.s}
	assembler/assembler $(LORELEY_DIR)/main.s $(LORELEY_DIR)/main.bin

# min-rt.binを実行
lor-run:
	make min-rt
	touch $(LOR_PPM_PATH)
	eog $(LOR_PPM_PATH) & 2> /dev/null
	make -s ./test/min-rt/min-rt.run < $(LOR_PATH) > $(LOR_PPM_PATH)

# min-rt.s, min-rt.binを削除
lor-clean:
	rm -rf $(LOR_PPM_PATH) $(LORELEY_DIR)/main.s $(LORELEY_DIR)/main.bin

#--------------------------------------------------------------------
# その他ファイルのコンパイル
#--------------------------------------------------------------------

# .bin, .sが存在しないときのみビルドして実行
%.run:
	make $*.bin
	simulator/simulator $*.bin

# .bin, .sが存在しててもビルドした上で実行する
%.run_f:
	make tools
	make $*.bin_f
	simulator/simulator $*.bin

# .bin, .sが存在しないときのみビルド
%.bin:
	cat $(LIB_ML) $*.ml > __tmp__.ml
	compiler/min-caml $(OPTION) $(BINARY) $(INLINE) __tmp__
	cd linker; $(LINKER) ../$(LIB_ASM) ../__tmp__.s ${abspath $*.s}
	assembler/assembler $*.s $*.bin

# .bin, .sが存在しててもビルドする
%.bin_f:
	cat $(LIB_ML) $*.ml > __tmp__.ml
	compiler/min-caml $(OPTION) $(BINARY) $(INLINE) __tmp__
	cd linker; $(LINKER) ../$(LIB_ASM) ../__tmp__.s ${abspath $*.s}
	assembler/assembler $*.s $*.bin

# .sが存在しないときのみビルド
%.s:
	cat $(LIB_ML) $*.ml > __tmp__.ml
	compiler/min-caml $(OPTION) $(BINARY) $(INLINE) __tmp__
	cd linker; $(LINKER) ../$(LIB_ASM) ../__tmp__.s ${abspath $*.s}

# .sが存在しててもビルド
%.s_f:
	cat $(LIB_ML) $*.ml > __tmp__.ml
	compiler/min-caml $(OPTION) $(BINARY) $(INLINE) __tmp__
	cd linker; $(LINKER) ../$(LIB_ASM) ../__tmp__.s ${abspath $*.s}

# MLがソースファイルのとき
%.clean_ml:
	rm $*.s
	rm $*.bin

# アセンブリがソースファイルのとき。.sを削除したらまずい
%.clean_s:
	rm $*.bin
