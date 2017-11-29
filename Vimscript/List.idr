module Vimscript.List

import Vimscript.FFI

%access export

empty : {auto p : VIM_Types (VimList a)} -> VimList a
empty {a} =
  unsafePerformIO $
  foreign FFI_VIM VIM_ListEmpty (VIM_IO (VimList a))

concat : {auto p : VIM_Types (VimList a)} -> VimList a -> VimList a -> VimList a
concat {a} l1 l2 =
  unsafePerformIO $
  foreign
  FFI_VIM
  VIM_ListConcat
  (VimList a -> VimList a -> VIM_IO (VimList a))
  l1
  l2

cons : {auto p : VIM_Types a} -> {auto p' : VIM_Types (VimList a)} -> (x : a) -> VimList a -> VimList a
cons {a} x list =
  unsafePerformIO $
  foreign
  FFI_VIM
  VIM_ListCons
  (a -> VimList a -> VIM_IO (VimList a))
  x
  list

snoc : {auto p : VIM_Types a} -> {auto p' : VIM_Types (VimList a)} -> VimList a -> (x : a) -> VimList a
snoc {a} list x =
  unsafePerformIO $
  foreign
  FFI_VIM
  VIM_ListSnoc
  (VimList a -> a -> VIM_IO (VimList a))
  list
  x

fromFoldable : (Foldable f) => {auto p : VIM_Types a} -> f a -> VimList a
fromFoldable = foldl snoc empty

l1 : List String
l1 = do
  hi <- ["Hi", "Hello", "Greetings"]
  who <- ["world", "Vim", "Idris"]
  pure (hi ++ ", " ++ who ++ "!")

l2 : VimList String
l2 = fromFoldable l1
