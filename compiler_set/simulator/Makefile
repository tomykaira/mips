include ../Makefile.in

HEADER = ../include/common.h fpu.h logger.h simulator.h InputFile.h Display.h

all:simulator
simulator: simulator.o fpu.o logger.o InputFile.o Display.o SDCard.o
simulator.o: $(HEADER)
fpu.o: fpu.h
logger.o: logger.h simulator.h
InputFile.o: InputFile.h simulator.h
Display.o: Display.h simulator.h
SDCard.o: SDCard.h simulator.h

clean:
	rm *.o
	rm simulator
