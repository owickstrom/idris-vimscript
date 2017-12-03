module Main

import Vimscript.Builtins
import Vimscript.FFI
import Vimscript.List

||| Make a bunch of splits, alternating between
||| horizontal and vertical.
mkSplits : (n : Int) -> VIM_IO ()
mkSplits n = go n True
  where
    go 0 _ = pure ()
    go n c =
      (if c then split else vsplit) *> go (n - 1) (not c)

-- function! StripTrailingWhitespaces()
--     " save last search & cursor position
--     let _s=@/
--     let l = line(".")
--     let c = col(".")
--     %s/\s\+$//e
--     let @/=_s
--     call cursor(l, c)
-- endfunction
stripTrailingWhitespace : VIM_IO ()
stripTrailingWhitespace = do
  l <- line "."
  c <- col "."
  sub "%" "\\s\\+$" "" "e"
  cursor l c

main : VIM_IO ()
main = do
  echo ("Readonly flag is set to " ++ !(readOption "readonly"))
  writeOption "readonly" 1
  echo ("Readonly flag is set to " ++ !(readOption "readonly"))

