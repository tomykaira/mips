testrunner: finv_ikuta.cc
	g++ -Wall -g -o testrunner finv_ikuta.cc

join: join.cc
	g++ -Wall -g -o $@ $<

rollback: inc.dat.orig const.dat.orig
	cp inc.dat.orig inc.dat
	cp const.dat.orig const.dat

clean:
	rm -rf testrunner *.o
