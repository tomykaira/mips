OCOPT = -ccopt -O3 -w a -unsafe
#OCOPT = -ccopt -O2
OCAMLBASE=/usr/bin
OCAMLOPT=$(OCAMLBASE)/ocamlopt

LOR_PATH=Simple/simple1.lor

all: loreley

loreley: main.ml
	$(OCAMLOPT) $(OCOPT) -o loreley main.ml

include ../Makefile.in

ARTIFACTS=*.cm? *.o loreley *.sim *.s *.bin

%.s: %.ml
	cat $(LIB_ML) $*.ml > $(call tmp,$*).ml
	$(MIN_CAML) -inline 600 $(call tmp,$*)
	sed $(LIB_ASM) -e 's/#LORELEY //' > $(call tmp,lib_asm).s
	$(LINKER) $(call tmp,lib_asm).s $(call tmp,$*).s ${abspath $*.s}

# to override lor file, specify from command-line
%.sim: %.bin
	$(SIMULATOR) $< -f $(LOR_PATH) > $@
