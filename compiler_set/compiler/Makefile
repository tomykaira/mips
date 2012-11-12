# Sumii's Makefile for Min-Caml (for GNU Make)
# 
# ack.mlなどのテストプログラムをtest/に用意してmake do_testを実行すると、
# min-camlとocamlでコンパイル・実行した結果を自動で比較します。

AUXLIB_DIRS = +deriving +/usr/local/lib/ocaml/3.12.1/bisect
OCAMLFLAGS = -w Aef

RESULT = min-caml
NCSUFFIX = .opt
CC = gcc
CFLAGS = -g -O2 -Wall

LIBS = nums deriving
CLIBS=

INCDIRS = $(AUXLIB_DIRS)
LIBDIRS = $(AUXLIB_DIRS)

#default: debug-code top $(RESULT) do_test
$(RESULT): debug-code top
br: bisect-byte-code
## [自分（住井）用の注]
## ・OCamlMakefileや古いGNU Makeのバグ(?)で上のような定義が必要(??)
## ・OCamlMakefileではdebug-codeとnative-codeのそれぞれで
##   .mliがコンパイルされてしまうので、両方ともdefault:の右辺に入れると
##   再make時に（.mliが変更されているので）.mlも再コンパイルされる
clean:: nobackup clean-coverage

clean-coverage:
	rm -rf coverage*
	rm -rf report

# ↓もし実装を改造したら、それに合わせて変える
SOURCES = global.ml float.c type.ml id.ml syntax.ml m.ml s.ml \
parser.mly lexer.mll typing.mli typing.ml kNormal.mli kNormal.ml \
alpha.mli alpha.ml beta.mli beta.ml assoc.mli assoc.ml \
inline.mli inline.ml constFold.mli constFold.ml elim.mli elim.ml \
closure.mli closure.ml asm.mli asm.ml virtual.mli virtual.ml \
simm.mli simm.ml regAlloc.mli regAlloc.ml emit.mli emit.ml \
main.mli main.ml

include OCamlMakefile

bisect-byte-code:		$(PRE_TARGETS)
			$(QUIET)$(MAKE) -r -f $(OCAMLMAKEFILE) $(BCRESULT) \
				REAL_RESULT="$(BCRESULT)" \
				OCAMLFLAGS="-I +bisect -pp \"camlp4o str.cma `ocamlc -where`/bisect/bisect_pp.cmo\" $(OCAMLFLAGS)" \
				OCAMLLDFLAGS="-I +bisect bisect.cma $(OCAMLLDFLAGS)" \
				make_deps=yes
