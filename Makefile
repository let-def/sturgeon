all: byte-code-library native-code-library
	for i in $(SUB); do $(MAKE) -C $$i $@; done

SOURCES =                    \
  sexp.mli      sexp.ml      \
  session.mli   session.ml   \
  buf_naive.mli buf_naive.ml \
  buf.mli       buf.ml       \
  ui_print.mli  ui_print.ml  \
  ui_nav.mli    ui_nav.ml    \
  ui_tree.mli   ui_tree.ml

PACKS = grenier.baltree grenier.orderme

LIB_PACK_NAME = sturgeon
RESULT = sturgeon

LIBINSTALL_FILES = \
	emacs_sexp.mli   \
	emacs_sexp.cmi   \
	emacs_sexp.cmo   \
	emacs_sexp.cmx

-include OCamlMakefile

install: libinstall

uninstall: libuninstall

reinstall:
	-$(MAKE) uninstall
	$(MAKE) install
