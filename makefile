#
# Makefile for the default server drgs and client drgc
#

.PHONY: all clean drgs #drgc

OCB_FLAGS = -use-ocamlfind -I src
OCB = ocamlbuild $(OCB_FLAGS)

all: bin/drgs #drgc # profile debug

clean:
	$(OCB) -clean
	rm -rf bin

bin/drgs: src/*.ml
	mkdir -p bin
	$(OCB) drgs.native
	mv drgs.native bin/drgs
