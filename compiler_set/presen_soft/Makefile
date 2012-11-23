include ../test/Makefile.in

# turn off binary(-b)
OPTION := -inline 50
# turn off test(-t)
SIMULATOR := $(COMPILER_SET)/simulator/simulator

test: main.bin
	$(SIMULATOR) -f test.txt main.bin
