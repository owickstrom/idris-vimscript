module Main

import Vimscript.FFI
import Vimscript.List
import Vimscript.Builtin

printWith : (String -> VIM_IO ()) -> String -> VIM_IO ()
printWith f s =
  f (s ++ "!")

numbers : VimList Int
numbers =
  let l1 = cons 1 (cons 2 empty)
      l2 = snoc (snoc empty 3) 4
  in concat l1 l2

-- Construct a regular List of Strings.
idrisList : List String
idrisList = do
  hi <- ["Hi", "Hello", "Greetings"]
  who <- ["world", "Vim", "Idris"]
  pure (hi ++ ", " ++ who ++ "!")

main : VIM_IO ()
main = do
  -- You can do Vimmy stuff:
  l <- line "$"
  s <- getline (l - 1)

  -- Higher-order functions:
  printWith echo s

  -- We can echo Vim types:
  echo numbers

  -- Convert an Idris List to a VimList:
  let greetings = fromFoldable (the (List String) idrisList)

  -- Add the to end of the current buffer:
  appendLines l greetings
