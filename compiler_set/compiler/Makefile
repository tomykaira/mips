RESULT = min-caml

$(RESULT): debug-code top
clean:: nobackup

# ↓もし実装を改造したら、それに合わせて変える
SOURCES = global.ml type.ml id.ml m.ml s.ml \
syntax.ml parser.mly lexer.mll typing.mli typing.ml kNormal.mli kNormal.ml \
alpha.mli alpha.ml beta.ml cse.ml assoc.mli assoc.ml \
inline.mli inline.ml constFold.mli constFold.ml elim.mli elim.ml\
clsElim.ml closure.mli closure.ml asm.mli asm.ml virtual.mli virtual.ml \
simm.mli simm.ml regAlloc.mli regAlloc.ml out.ml emit.mli emit.ml jumpElim.ml \
main.mli main.ml

include OCamlMakefile
