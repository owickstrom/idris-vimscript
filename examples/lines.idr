module Main

import Vimscript.FFI
import Vimscript.List
import Vimscript.Builtin

substr : Int -> Int -> String -> String
substr = prim__strSubstr

length : String -> Int
length = prim_lenString

prefixPrint : String -> String -> VIM_IO ()
prefixPrint p s =
  echo (p ++ s)

main : VIM_IO ()
main = do
  l <- line "$"
  s <- getline (l - 1)
  let printResult = prefixPrint "Result: "
  printResult s

  arr1 <- List.empty
    >>= flip List.snoc 1
    >>= flip List.snoc 2
  arr2 <- List.empty
    >>= flip List.snoc 3
    >>= flip List.snoc 4
  arr <- List.concat arr1 arr2
  echo arr
