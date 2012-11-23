targets:
	@ls -1 *.ml

# Prepare .ans file beforehand
%.test: %.sim
	diff $*.ans $*.sim

include ../Makefile.in
