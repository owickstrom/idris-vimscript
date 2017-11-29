.PHONY: examples
examples: examples/lines.idr
	idris examples/lines.idr -p vimscript --codegen vim -o examples/lines.vim
