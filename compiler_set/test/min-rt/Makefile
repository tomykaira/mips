SLD_PATH = sldbin/contest.sldbin

targets:
	@echo min-rt-new.ml

%.test: %.ans %.sim
	diff $*.ans $*.sim

%.test_core: %.ans %.core
	diff $*.ans $*.core

include ../Makefile.in

# It should not depend on %.sim, because this is not updated after sim is updated.
%.ans:
	make $*.sim
	cp $*.sim $*.ans

%.s: %.ml
	cat $(LIB_ML) globals.ml $*.ml > $(call tmp,$*).ml
	$(MIN_CAML) $(BIN) $(OPTION) $(call tmp,$*)
	$(LINKER) $(LIB_ASM) $(call tmp,$*).s ${abspath $*.s}


%.sim: %.bin
	$(SIMULATOR) $(BIN) -f $(SLD_PATH) $*.bin > $*.sim

%.ppm: %.bin
	$(SIMULATOR) $(BIN) -f $(SLD_PATH) $*.bin | tail -c +2 > output.ppm

%.core: %.bin
	$(CORE_RUNNER) $*.bin -i $(SLD_PATH) > $*.core

# use this with cserver
# ./cserver -b -B 460800 sld/contest.sld contest_new.ppm
write: min-rt-new.bin
	$(CORE_RUNNER) $< -R

