targets:
	@ls -1 *.ml

%.input: %.ml
	ruby -e '(m = STDIN.read.match(/\(\*INPUTB(.*)\*\)/m)) && puts(m[1].split(" ").map{|x| x.to_i(16).chr}.join"")' < $*.ml > $*.input

%.sim: %.bin %.input
	$(SIMULATOR) -f $*.input $*.bin > $*.sim

%.test: %.ans %.sim
	diff $*.ans $*.sim

%.ans: %.sim
	cp $*.sim $*.ans

include ../Makefile.in
