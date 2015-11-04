all: byte-code-library native-code-library
	for i in $(SUB); do $(MAKE) -C $$i $@; done

SOURCES = \
	emacs_sexp.mli emacs_sexp.ml \
	emacs_serge.mli emacs_serge.ml \
	emacs_hyperprint.mli emacs_hyperprint.ml \
	emacs_hypernav.mli emacs_hypernav.ml \
	emacs_htree.mli emacs_htree.ml

RESULT = emacs_serge

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

EXAMPLES = echo_server fib_server nav_server print_server tree_server

examples: $(EXAMPLES)

$(EXAMPLES): ncl
	ocamlopt -o $@ unix.cmxa emacs_serge.cmxa $@.ml
