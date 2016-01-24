
.PHONY: build test clean

build:
	ocamlbuild -pkg graphics nomcell.native

test: build
	./nomcell.native

clean:
	rm -fr _build nomcell.native *~ \#* 
