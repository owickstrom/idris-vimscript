module Main

import Data.Vect

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

l3 : VimList (VimList String)
l3 = fromFoldable (map fromFoldable (the (List (List String)) [["Hi", "there"]]))

v1 : Vect 2 Int
v1 = [666] ++ [999]

l4 : VimList Int
l4 = fromFoldable v1

main : VIM_IO ()
main = do
  -- You can do Vimmy stuff:
  l <- line "$"
  s <- getline (l - 1)
  let sl = length s

  -- Higher-order functions:
  printWith echo (substr 3 (3 + sl) s)

  -- We can echo Vim types:
  echo numbers

  -- Convert an Idris List to a VimList:
  let greetings = fromFoldable (the (List String) idrisList)

  -- Add the to end of the current buffer:
  appendLines l greetings

  -- Echo nested listed:
  echo l3

  -- And what was a vector...
  echo l4

