EXAMPLE_SRCS=$(shell find examples -name '*.idr')
EXAMPLE_TARGETS=$(EXAMPLE_SRCS:examples/%.idr=examples/%.vim)

.PHONY: examples
examples: $(EXAMPLE_TARGETS)

examples/%.vim: examples/%.idr
	idris $< -p vimscript --codegen vim -o $@
