module Vimscript.List

import Vimscript.FFI

%default total

public export
data VimList a = MkList (Raw a)

public export
empty : VimList a
empty {a} =
  unsafePerformIO $ do
    MkRaw l <- foreign FFI_VIM VIM_ListEmpty (VIM_IO (Raw (VimList a)))
    pure l

unsafeIndex : Int -> VimList a -> a
unsafeIndex {a} i l =
  unsafePerformIO $ do
    MkRaw a <- foreign FFI_VIM VIM_ListIndex (Int -> Raw (VimList a) -> VIM_IO (Raw a)) i (MkRaw l)
    pure a

public export
length : VimList a -> Nat
length {a} l =
  unsafePerformIO $ do
    n <- foreign FFI_VIM (VIM_BuiltIn "len") (Raw (VimList a) -> VIM_IO Int) (MkRaw l)
    pure (fromInteger (cast n))

public export
concat : VimList a -> VimList a -> VimList a
concat {a} l1 l2 =
  unsafePerformIO $ do
    MkRaw l3 <-
      foreign
      FFI_VIM
      VIM_ListConcat
      (Raw (VimList a) -> Raw (VimList a) -> VIM_IO (Raw (VimList a)))
      (MkRaw l1)
      (MkRaw l2)
    pure l3

public export
cons : (x : a) -> VimList a -> VimList a
cons {a} x l1 =
  unsafePerformIO $ do
    MkRaw l2 <-
      foreign
      FFI_VIM
      VIM_ListCons
      (Raw a -> Raw (VimList a) -> VIM_IO (Raw (VimList a)))
      (MkRaw x)
      (MkRaw l1)
    pure l2

public export
head : VimList a -> Maybe a
head {a} l1 =
  case length l1 of
    Z => Nothing
    S _ => Just (unsafeIndex 0 l1)


public export
snoc : VimList a -> (x : a) -> VimList a
snoc {a} l1 x =
  unsafePerformIO $ do
    MkRaw l2 <-
      foreign
      FFI_VIM
      VIM_ListSnoc
      (Raw (VimList a) -> Raw a -> VIM_IO (Raw (VimList a)))
      (MkRaw l1)
      (MkRaw x)
    pure l2

public export
fromFoldable : (Foldable f) => f a -> VimList a
fromFoldable = foldl snoc empty

test1 : List String
test1 = do
  hi <- ["Hi", "Hello", "Greetings"]
  who <- ["world", "Vim", "Idris"]
  pure (hi ++ ", " ++ who ++ "!")

test2 : VimList String
test2 = fromFoldable test1
