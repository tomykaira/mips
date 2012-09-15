testrunner: finv_ikuta.cc
	g++ -Wall -g -o testrunner finv_ikuta.cc

rollback: inc.dat.orig const.dat.orig
	cp inc.dat.orig inc.dat
	cp const.dat.orig const.dat

clean:
	rm -rf testrunner *.o
