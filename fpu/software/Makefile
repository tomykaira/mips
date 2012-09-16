JOIN = join.cc table.o table.h util.o util.h
FINV = finv_ikuta.cc table.o table.h util.o util.h
FSQRT = fsqrt.cc table.o table.h util.o util.h

finv: $(FINV)
	g++ -Wall -g -o $@ $(FINV)

fsqrt: $(FSQRT)
	g++ -Wall -g -o $@ $(FSQRT)

join: $(JOIN)
	g++ -Wall -g -o $@ $(JOIN)

%.o: %.cc
	g++ -Wall -g -c -o $@ $<

rollback: inc.dat.orig const.dat.orig
	cp inc.dat.orig inc.dat
	cp const.dat.orig const.dat

clean:
	rm -rf testrunner *.o
