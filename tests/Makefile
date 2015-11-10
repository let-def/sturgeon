EXAMPLES = echo_server fib_server nav_server print_server tree_server

examples: $(EXAMPLES)

$(EXAMPLES):
	ocamlfind opt -package grenier.baltree,grenier.orderme -g -linkpkg -o $@ \
		-I .. unix.cmxa sturgeon.cmxa $@.ml

.PHONY: $(EXAMPLES)
