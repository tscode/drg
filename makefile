#
# Makefile for the default server drgs and client drgc
#

.PHONY: all clean drgs #drgc

OCB_FLAGS = -use-ocamlfind -I src
OCB = ocamlbuild $(OCB_FLAGS)

all: bin/drgs bin/drgc # profile debug

clean:
	$(OCB) -clean
	rm -rf bin

bin/drgs: src/*.ml | bin
	$(OCB) drgs.native
	mv drgs.native bin/drgs

bin/drgc: src/*.ml | bin
	$(OCB) drgc.native
	mv drgc.native bin/drgc


bin:
	mkdir -p $@
