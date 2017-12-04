# Idris Vimscript!

[![](https://img.shields.io/github/issues/badges/shields/good%20first%20issue.svg?colorB=05c62c)](https://github.com/owickstrom/idris-vimscript/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22)

The good stuff.

**Please note that this is *very* experimental. For example, the Idris `Integer` type, usually
represented as an arbitrary-precision integer, is represented using the Vim
number type.**

**It's a hack, deal with it. &#x1f60e;**

Or, better still:

## Contribute!

This is a fun project, and the authors aren't expert compiler writers: we need your help! There are three languages (Idris, Haskell, and documentation!) you can contribute in, and knowledge of Vimscript is useful too. 
Specifically:

* if you know a little Idris, you can add bindings for more of the Vim API
* if you know some Haskell, you can add new or improve existing optimisations, make the generated code better, or improve performance
* if you can do Vimscript, we'd like you to help us generate better, faster, smaller code that Vimscript users can understand somewhat well
* The Idris REPL itself lets people browse documentation, so even a few comments on key functions can make users' day-to-day lives much easier when they're discovering the API.

Good first issues for new contributors are tagged [![](https://img.shields.io/github/issues/badges/shields/good%20first%20issue.svg?colorB=05c62c)](https://github.com/owickstrom/idris-vimscript/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22) on our issue tracker.

If you have an idea in mind, open a PR!

## Examples

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
