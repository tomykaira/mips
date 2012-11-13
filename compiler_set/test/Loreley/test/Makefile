SIMULATOR=../../../simulator/simulator

# order from easy (simple) to hard (complicated)
TEST_WITH_OCAML=read_environment read_glConf read_deConf read_cMap read_dnas \
decode_init
# read_gene: printer bug
# output_ppm: infinite loop
TEST_WITH_ANSWER=apply_func
# apply_gene_no_random shooting_sub: heisenbug

.PHONY: test test-with-ocaml test-with-answer
test: test-with-ocaml test-with-answer clean
test-with-ocaml: $(TEST_WITH_OCAML:%=%.test)
test-with-answer: $(TEST_WITH_ANSWER:%=%.cmp)

%.test: %.ocaml %.sim
	diff $*.ocaml $*.sim

%.cmp: %.sim
	diff $*.ans $*.sim

# don't use with ease, commit to git if you add or update an answer
%.ans: %.sim %.ocaml
	cat $*.sim
	diff $*.ocaml $*.sim || true
	cp $*.sim $*.ans

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
