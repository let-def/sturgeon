EXAMPLES = echo_server fib_server nav_server print_server tree_server

examples: $(EXAMPLES)

$(EXAMPLES):
	ocamlfind opt -package grenier.trope,sturgeon,unix -g -linkpkg -o $@ $@.ml

.PHONY: $(EXAMPLES)
