include ../Makefile.in

HEADER = ../include/common.h fpu.h

all:simulator
simulator: simulator.o fpu.o
simulator.o: $(HEADER)
fpu.o: fpu.h

clean:
	rm *.o
	rm simulator

