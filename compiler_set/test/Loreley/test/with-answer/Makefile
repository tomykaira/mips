targets:
	@ls -1 *.ml

%.test: %.sim
	diff $*.ans $*.sim
include ../Makefile.in
include ../../../Makefile.in
