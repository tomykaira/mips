JOIN = join.cc table.o table.h util.o util.h
FINV = myfinv.cc table.o table.h util.o util.h
FSQRT = fsqrt.cc table.o table.h util.o util.h
TEST_SOURCE = test_source.c util.o util.h
FADD = fadd.c util.o util.h
FMUL = fmul.c util.o util.h
MYFDIV = myfdiv.cc table.o table.h util.o util.h

finv: $(FINV) finv_mytest.c
	gcc -Wall -o test finv_mytest.c
	./test
	g++ -Wall -g -o $@ $(FINV)

fdiv: $(MYFDIV)
	g++ -Wall -g -o $@ $(MYFDIV)

fsqrt: $(FSQRT)
	g++ -Wall -g -o $@ $(FSQRT)

join: $(JOIN)
	g++ -Wall -g -o $@ $(JOIN)

test_source: $(TEST_SOURCE)
	g++ -Wall -g -o $@ $(TEST_SOURCE)

fadd: $(FADD)
	g++ -Wall -g -o $@ $(FADD)

fmul: $(FMUL)
	g++ -Wall -g -o $@ $(FMUL)

%.o: %.cc
	g++ -Wall -g -c -o $@ $<

rollback: inc.dat.orig const.dat.orig
	cp inc.dat.orig inc.dat
	cp const.dat.orig const.dat

test:finv_mytest.c
	gcc -Wall -o test finv_mytest.c

clean:
	rm -rf testrunner *.o
