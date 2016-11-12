
.PHONY: build run clean

TARGET=neca

build:
	ocamlbuild -use-ocamlfind -package graphics -package str $(TARGET).native && cp -L $(TARGET).native $(TARGET)

run: build
	./$(TARGET)

clean:
	rm -fr _build $(TARGET) *~ \#* 
