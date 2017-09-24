#
# Makefile for the default server drgs and client drgc
#

.PHONY: all clean drgs #drgc

OCB_FLAGS = -use-ocamlfind -I src -I src/drgs -I src/drgc
OCB = ocamlbuild $(OCB_FLAGS)

all: bin/drgs bin/drgc # profile debug

clean:
	$(OCB) -clean
	rm -rf bin

bin/drgs: src/*.ml src/drgs/*.ml | bin
	$(OCB) drgs.native
	mv drgs.native bin/drgs

bin/drgc: src/*.ml src/drgc/*.ml | bin
	$(OCB) drgc.native
	mv drgc.native bin/drgc

bin:
	mkdir -p $@
