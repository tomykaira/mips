include ../Makefile.in

HEADER = ../include/common.h fpu.h logger.h simulator.h

all:simulator
simulator: simulator.o fpu.o logger.o
simulator.o: $(HEADER)
fpu.o: fpu.h
logger.o: logger.h simulator.h

clean:
	rm *.o
	rm simulator
