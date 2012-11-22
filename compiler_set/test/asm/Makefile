# files to be deleted by clean
ARTIFACTS=*.bin

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
