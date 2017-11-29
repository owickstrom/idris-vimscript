# Idris Vimscript

The good stuff.

**Please not that this is VERY experimental. The Idris Integer type, usually
represented as an arbitrary-precision integer, is represented as the Vim number
type. It's a hack, deal with it. &#x1f60e;**

## Example

``` idris
module Main

import Vimscript.FFI
import Vimscript.List
import Vimscript.Builtin

main : VIM_IO ()
main = do
  l <- line "$"              -- get the last line nr
  s <- getline (l - 1)       -- get the contents of the next-to-last line
  let sl = length s          -- get its length
  echo (substr 3 (3 + sl) s) -- print a part of it
```

## Build and Install

Install the code generator:
``` shell
stack install # or cabal install
```

Install the Vimscript library for Idris:

``` shell
idris --install vimscript.ipkg
```

## Run Examples

``` shell
make
vim examples/lines.vim
# :source %             (in Vim)
```

## License

[BSD 3 License](LICENSE). Original work by Edwin Brady.
