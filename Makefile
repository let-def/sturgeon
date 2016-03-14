all: byte-code-library native-code-library sturgeon-connector
	for i in $(SUB); do $(MAKE) -C $$i $@; done

SOURCES =                \
	sexp.mli    sexp.ml    \
	session.mli session.ml \
	textbuf.mli textbuf.ml \
	stui.mli    stui.ml    \
	recipes.mli recipes.ml

PACKS = grenier.trope inuit
THREADS = 1

OCAMLFLAGS += -g
OCAMLLDFLAGS += -g

LIB_PACK_NAME = sturgeon
RESULT = sturgeon

LIBINSTALL_FILES = \
	sturgeon.a       \
	sturgeon.cma     \
	sturgeon.cmi     \
	sturgeon.cmo     \
	sturgeon.cmx     \
	sturgeon.cmxa    \
	sturgeon.o


-include OCamlMakefile

install: libinstall

uninstall: libuninstall

reinstall:
	-$(MAKE) uninstall
	$(MAKE) install

sturgeon-connector: sturgeon.cmxa connector/sturgeon_connector.ml
	ocamlfind opt -linkpkg -package grenier.trope,unix -o $@ $^

clean::
	rm -f connector/*.cm* connector/*.o sturgeon-connector
