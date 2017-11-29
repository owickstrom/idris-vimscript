module Main

import Vimscript.FFI
import Vimscript.List
import Vimscript.Builtin

substr : Int -> Int -> String -> String
substr = prim__strSubstr

length : String -> Int
length = prim_lenString

main : VIM_IO ()
main = do
  l <- line "$"
  s <- getline (l - 1)
  echo s
  arr1 <-
    List.empty
    -- >>= List.prepend (toVim {from=Int} {to=VimInt} 333)
    -- >>= List.append (toVim {from=Int} {to=VimInt} 666)
  arr2 <-
    List.empty
    -- >>= List.prepend (toVim {from=Int} {to=VimInt} 222)
    -- >>= List.append (toVim {from=Int} {to=VimInt} 444)
  arr <- List.concat arr1 arr2
  echo (the (VimList Int) arr)
