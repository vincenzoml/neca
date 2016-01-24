
.PHONY: build test clean

build:
	ocamlbuild -use-ocamlfind -package graphics nomcell.native

test: build
	./nomcell.native

clean:
	rm -fr _build nomcell.native *~ \#* 
