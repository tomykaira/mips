
testrunner: fmul.o testrunner.smi testrunner.sml
	smlsharp -o testrunner testrunner.sml fmul.o

fmul.o: fmul.c
	gcc -c -Wall -o fmul.o fmul.c

clean:
	rm -rf testrunner fmul.o
