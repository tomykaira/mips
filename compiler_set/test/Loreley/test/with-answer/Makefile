targets:
	@ls -1 *.ml

include ../Makefile.in
include ../../../Makefile.in

%.test: %.sim
	diff $*.ans $*.sim
