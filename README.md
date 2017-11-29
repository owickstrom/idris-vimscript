# Idris Vimscript

The good stuff.

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

``` shell
stack install # or cabal install
idris --install vimscript.ipkg
```

## License

[BSD 3 License](LICENSE). Original work by Edwin Brady.
