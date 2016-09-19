TARGETS = default clean lib-native doc tests
all: default

.PHONY: all $(TARGETS)

$(TARGETS):
	sh build $@

%:
	sh build $@

clean:
	sh build $@
	$(MAKE) -C test/landmarks clean
