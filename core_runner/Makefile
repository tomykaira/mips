ASSEMBLER = ../compiler_set/assembler/assembler

all: bootloader.bin

clean:
	rm -rf *.bin

%.bin: %.s
	$(ASSEMBLER) $< $@
