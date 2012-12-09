targets:
	@ls -1 *.ml

TESTS=$(shell ls -1 *.ml)
test:$(TESTS:%.ml=%.test)

include ../Makefile.in
