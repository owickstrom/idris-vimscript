module Vimscript.List

import Vimscript.FFI

%access private

data Prim_VimList = Prim_MkVimList

prim_empty : VIM_IO Prim_VimList
prim_empty = do
  MkRaw arr <- foreign FFI_VIM VIM_ListEmpty (VIM_IO (Raw Prim_VimList))
  pure arr

prim_concat : Prim_VimList -> Prim_VimList -> VIM_IO Prim_VimList
prim_concat a1 a2 = do
  MkRaw arr <-
    foreign
      FFI_VIM
      VIM_ListConcat
      (Raw Prim_VimList -> Raw Prim_VimList -> VIM_IO (Raw Prim_VimList))
      (MkRaw a1)
      (MkRaw a2)
  pure arr

prim_setAt : Int -> a -> Prim_VimList -> VIM_IO ()
prim_setAt {a} i x arr =
  foreign
    FFI_VIM
    VIM_ListSetAt
    (Int -> Raw a -> Raw Prim_VimList -> VIM_IO ())
    i
    (MkRaw x)
    (MkRaw arr)


%access export

data VimList : Type -> Type where
  MkVimList : (prim_list : Prim_VimList) -> VimList a

empty : VIM_IO (VimList t)
empty = do
  parr <- prim_empty
  pure (MkVimList parr)

concat : VimList t -> VimList t -> VIM_IO (VimList t)
concat (MkVimList l1) (MkVimList l2) = do
  l3 <- prim_concat l1 l2
  pure (MkVimList l3)

-- prepend : t -> VimList t -> VIM_IO (VimList t)
--
-- append : t -> VimList t -> VIM_IO (VimList t)

-- toVimList : (Traversable f) => f String -> VIM_IO (VimList )
-- toVimList {from} {to} xs = do
--   arr <- empty
--   traverse_ (\x => arr `push` (toJS {from} {to} x)) xs
--   pure arr

