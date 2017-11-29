.PHONY: examples
examples: examples/misc.idr
	idris examples/misc.idr -p vimscript --codegen vim -o examples/misc.vim
