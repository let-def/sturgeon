EXAMPLES = nav_server print_server tree_server media_test sync_buffer

examples: $(EXAMPLES)

clean:
	rm -f $(EXAMPLES) *.cm* *.o

$(EXAMPLES):
	ocamlfind opt -thread -package str,grenier.trope,inuit,sturgeon,unix -g -linkpkg -o $@ $@.ml

.PHONY: $(EXAMPLES)
