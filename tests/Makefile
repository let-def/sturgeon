EXAMPLES = nav_server print_server tree_server

examples: $(EXAMPLES)

clean:
	rm -f $(EXAMPLES) *.cm* *.o

$(EXAMPLES):
	ocamlfind opt -package grenier.trope,sturgeon,unix -g -linkpkg -o $@ $@.ml

.PHONY: $(EXAMPLES)
