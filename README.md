# Idris in Vimscript!

[![](https://img.shields.io/github/issues/badges/shields/good%20first%20issue.svg?colorB=05c62c)]()

The good stuff.

**Please note that this is *very* experimental. For example, the Idris `Integer` type, usually
represented as an arbitrary-precision integer, is represented using the Vim
number type.**

**It's a hack, deal with it. &#x1f60e;**

## Example

Many examples of what is possible are present in the 
[examples/](https://github.com/owickstrom/idris-vimscript/tree/master/examples) 
directory, but this will give you an idea of the state of the art:

``` idris
module Main

import Vimscript.FFI
import Vimscript.List
import Vimscript.Builtin

main : VIM_IO ()
main = do
  l <- line "$"              -- get the last line number
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

[BSD 3 License](LICENSE). Based on original work by Edwin Brady.
