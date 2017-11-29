module Vimscript.List

import Vimscript.FFI

%access private

data Prim_VimList = Prim_MkVimList

prim_empty : VIM_IO Prim_VimList
prim_empty = do
  MkRaw list <- foreign FFI_VIM VIM_ListEmpty (VIM_IO (Raw Prim_VimList))
  pure list

prim_concat : Prim_VimList -> Prim_VimList -> VIM_IO Prim_VimList
prim_concat a1 a2 = do
  MkRaw list <-
    foreign
      FFI_VIM
      VIM_ListConcat
      (Raw Prim_VimList -> Raw Prim_VimList -> VIM_IO (Raw Prim_VimList))
      (MkRaw a1)
      (MkRaw a2)
  pure list

prim_cons : a -> Prim_VimList -> VIM_IO Prim_VimList
prim_cons {a} x list = do
  MkRaw list <-
    foreign
    FFI_VIM
    VIM_ListCons
    (Raw a -> Raw Prim_VimList -> VIM_IO (Raw Prim_VimList))
    (MkRaw x)
    (MkRaw list)
  pure list

prim_snoc : Prim_VimList -> a -> VIM_IO Prim_VimList
prim_snoc {a} list x = do
  MkRaw list <-
    foreign
    FFI_VIM
    VIM_ListSnoc
    (Raw Prim_VimList -> Raw a -> VIM_IO (Raw Prim_VimList))
    (MkRaw list)
    (MkRaw x)
  pure list

prim_setAt : Int -> a -> Prim_VimList -> VIM_IO ()
prim_setAt {a} i x list =
  foreign
    FFI_VIM
    VIM_ListSetAt
    (Int -> Raw a -> Raw Prim_VimList -> VIM_IO ())
    i
    (MkRaw x)
    (MkRaw list)


%access export

data VimList : Type -> Type where
  MkVimList : (prim_list : Prim_VimList) -> VimList a

empty : VIM_IO (VimList t)
empty = do
  parr <- prim_empty
  pure (MkVimList parr)

setAt: Int -> t -> VimList t -> VIM_IO ()
setAt i x (MkVimList l) = prim_setAt i x l

cons : t -> VimList t -> VIM_IO (VimList t)
cons x (MkVimList l) = do
  l2 <- prim_cons x l
  pure (MkVimList l2)

snoc : VimList t -> t -> VIM_IO (VimList t)
snoc (MkVimList l) x = do
  l2 <- prim_snoc l x
  pure (MkVimList l2)

concat : VimList t -> VimList t -> VIM_IO (VimList t)
concat (MkVimList l1) (MkVimList l2) = do
  l3 <- prim_concat l1 l2
  pure (MkVimList l3)

-- prepend : t -> VimList t -> VIM_IO (VimList t)
--
-- append : t -> VimList t -> VIM_IO (VimList t)

-- toVimList : (Traversable f) => f String -> VIM_IO (VimList )
-- toVimList {from} {to} xs = do
--   list <- empty
--   traverse_ (\x => list `push` (toJS {from} {to} x)) xs
--   pure list

