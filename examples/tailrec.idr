module Main

import Vimscript.FFI
import Vimscript.Builtins

loop : Int -> Int
loop n = go n 0
  where
    go : Int -> Int -> Int
    go 0 acc = acc
    go n acc = go (n - 1) (acc + n)

main : VIM_IO ()
main = echo (show (loop 100000))
