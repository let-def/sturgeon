PKG=ocaml pkg/pkg.ml

all: build

.PHONY: all build clean test

build clean:
	$(PKG) $@

test:
	ocamlbuild -use-ocamlfind test/test.otarget
