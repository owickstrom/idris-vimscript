IPKG=vimscript.ipkg
IDRIS_SRCS=$(shell find lib -name '*.idr')

EXAMPLE_SRCS=$(shell find examples -name '*.idr')
EXAMPLE_TARGETS=$(EXAMPLE_SRCS:examples/%.idr=examples/%.vim)
EXAMPLE_IBCS=$(EXAMPLE_SRCS:examples/%.idr=examples/%.ibc)

.PHONY: clean examples

examples: $(EXAMPLE_TARGETS)

examples/%.vim: examples/%.idr $(IDRIS_SRCS)
	stack exec idris -- $< -i lib/ --codegen vim -o $@

clean:
	stack exec idris -- --clean vimscript.ipkg
	rm -f $(EXAMPLE_IBCS) $(EXAMPLE_TARGETS)
