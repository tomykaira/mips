RESULT = min-caml

$(RESULT): debug-code top
clean:: nobackup

# ↓もし実装を改造したら、それに合わせて変える
SOURCES = global.ml type.ml id.ml m.ml s.ml \
syntax.ml parser.mly lexer.mll typing.mli typing.ml kNormal.mli kNormal.ml \
aNormal.ml alpha.ml beta.ml inline.ml ifThenElse.ml \
constFold.ml elim.ml cse.ml lCM.ml \
lambdaLift.ml closure.ml\
flattenTuple.ml unfoldTuple.ml embedTuple.ml elimTuple.ml assoc2.ml\
asm.ml virtual.mli virtual.ml constFold2.ml together.ml\
simm.mli simm.ml graphColor.ml out.ml emit.ml\
regAlloc.mli regAlloc.ml jumpElim.ml \
main.mli main.ml

include OCamlMakefile
