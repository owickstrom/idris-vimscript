IPKG=vimscript.ipkg
IDRIS_SRCS=$(shell find lib -name '*.idr')

EXAMPLE_SRCS=$(shell find examples -name '*.idr')
EXAMPLE_TARGETS=$(EXAMPLE_SRCS:examples/%.idr=examples/%.vim)
EXAMPLE_IBCS=$(EXAMPLE_SRCS:examples/%.idr=examples/%.ibc)

.PHONY: examples

examples: $(EXAMPLE_TARGETS)

examples/%.vim: examples/%.idr | ipkg
	idris $^ -p vimscript --codegen vim -o $@

ipkg: $(IPKG) $(IDRIS_SRCS)
	idris --install $(IPKG)

clean:
	rm $(EXAMPLE_IBCS) $(EXAMPLE_TARGETS)
