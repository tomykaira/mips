OCOPT = -ccopt -O3 -w a -unsafe
#OCOPT = -ccopt -O2
OCAMLBASE=/usr/bin
OCAMLOPT=$(OCAMLBASE)/ocamlopt

all: loreley

loreley: main.ml
	$(OCAMLOPT) $(OCOPT) -o loreley main.ml

clean:
	rm -f *.cm? *.o loreley