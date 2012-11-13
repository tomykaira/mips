SIMULATOR=../../../simulator/simulator

%.test: %.ocaml %.sim
	diff $*.ocaml $*.sim

%.input: %.ml
	ruby -e '(m = STDIN.read.match(/\(\*INPUT(.*)\*\)/m)) && puts(m[1].lstrip)' < $*.ml > $*.input

%.ocaml: %.ml %.input ocaml_header.ml
	cat ocaml_header.ml common_header.ml $*.ml > /tmp/ocaml.ml
	ocaml /tmp/ocaml.ml < $*.input > $*.ocaml

%.sim: %.ml %.input
	cat common_header.ml $*.ml > $*_tmp.ml
	cd ../../..; make ${abspath $*_tmp}.bin_f
	$(SIMULATOR) -f ${abspath $*.input} $*_tmp.bin > ${abspath $*.sim}

clean: 
	rm -rf *.ocaml *.input *.sim *.bin *.s *_tmp.ml
