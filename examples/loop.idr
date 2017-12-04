module Main

import Vimscript.FFI
import Vimscript.Builtins

loop : Int -> VIM_IO ()
loop 0 = pure ()
loop n = do
  echo (show n)
  loop (n - 1)

main : VIM_IO ()
main = do
  echo "Hello, Vim!"
  loop 10

-- isolate the "call graph" with v/s:Idris_/d
-- then we have these:
-- function s:Idris_Main_46__123_loop_95_1_125_(loc0,loc1)
--    return s:Idris_Main_46_loop(l:loc2)
-- function s:Idris_Main_46__123_main_95_1_125_(loc0)
--    return s:Idris_Main_46_loop(l:loc1)
