targets:
	@ls -1 *.s | grep -v "\(callback\|sequential\)"

%.exp: %.s
	ruby -e '(m = STDIN.read.match(/#EXPECTED: (.*)/)) && puts(m[1].lstrip)' < $*.s > $*.exp

%.sim_bytes: %.sim
	ruby -e 'puts File.read("$*.sim").bytes.map(&:ord).join(" ")' > $*.sim_bytes

# Prepare .ans file beforehand
%.test: %.exp %.sim_bytes
	diff $*.exp $*.sim_bytes

include ../Makefile.in

%.core: %.bin
	@echo "check the result with your own eyes"
	$(CORE_RUNNER) $*.bin -h1

ARTIFACTS=*.bin *.exp *.sim_bytes
