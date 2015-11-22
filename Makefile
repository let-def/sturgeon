all: byte-code-library native-code-library
	for i in $(SUB); do $(MAKE) -C $$i $@; done

SOURCES =                \
	sexp.mli    sexp.ml    \
	session.mli session.ml \
	tui.mli     tui.ml     \
	stui.mli    stui.ml    \
	recipes.mli recipes.ml

PACKS = grenier.trope

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
