EXAMPLES = nav_server print_server tree_server sync_buffer

examples: $(EXAMPLES)

clean:
	rm -f $(EXAMPLES) *.cm* *.o

$(EXAMPLES):
	ocamlfind opt -package grenier.trope,inuit,sturgeon,unix -g -linkpkg -o $@ $@.ml

.PHONY: $(EXAMPLES)
