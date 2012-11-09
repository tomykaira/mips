exp: exp.s ../lib_asm.s
	cat exp.s ../lib_asm.s > __tmp__.s
	../assembler/assembler __tmp__.s __tmp__.bin
	../simulator/simulator __tmp__.bin

log: log.s ../lib_asm.s
	cat log.s ../lib_asm.s > __tmp__.s
	../assembler/assembler __tmp__.s __tmp__.bin
	../simulator/simulator __tmp__.bin

fastexp: fastexp.c
	gcc -g -std=c99 -o fastexp -O3 -fomit-frame-pointer -ffast-math -lm fastexp.c
	./fastexp

fastlog: fastlog.c
	gcc -g -std=c99 -o fastlog -O3 -fomit-frame-pointer -ffast-math -lm fastlog.c
	./fastlog
