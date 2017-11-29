module Main

import Builtins

namespace Vim
  mutual
    public export
    data VimFn t = MkVimFn t

    public export
    data VIM_FnTypes : Type -> Type where
      VIM_Fn : VIM_Types s -> VIM_FnTypes t -> VIM_FnTypes (s -> t)
      VIM_FnIO : VIM_Types t -> VIM_FnTypes (IO' l t)
      VIM_FnBase : VIM_Types t -> VIM_FnTypes t

    public export
    data VIM_Types : Type -> Type where
      VIM_Str   : VIM_Types String
      VIM_Int   : VIM_Types Int
      VIM_Ptr   : VIM_Types Ptr
      VIM_Float : VIM_Types Double
      VIM_Unit  : VIM_Types ()
      VIM_FnT   : VIM_FnTypes t -> VIM_Types (VimFn t)

  %error_reverse
  public export
  FFI_VIM : FFI
  FFI_VIM = MkFFI VIM_Types String String

  %error_reverse
  public export
  VIM_IO : Type -> Type
  VIM_IO = IO' FFI_VIM

  IO : Type -> Type
  IO a = IO' FFI_VIM a

  vimCode : (code : String) -> (ty : Type) -> {auto fty : FTy FFI_VIM [] ty} -> ty
  vimCode code ty = foreign FFI_VIM code ty

  data VimType
    = VimInt
    | VimArray
    | VimString

  data VimValue : VimType -> Type where
    MkVimInt : Ptr -> VimValue VimInt
    MkVimString : Ptr -> VimValue VimString
    MkVimArray : Ptr -> VimValue VimArray

  unpack : VimValue t -> Ptr
  unpack (MkVimInt ptr) = ptr
  unpack (MkVimArray ptr) = ptr
  unpack (MkVimString ptr) = ptr

  interface ToVim from (to : VimType) where
    toVim : from -> VimValue to

  implementation ToVim String VimString where
    toVim str = MkVimString (believe_me str)

  implementation ToVim Int VimInt where
    toVim n = MkVimInt (believe_me n)

  interface FromVim (from : VimType) to where
    fromVim : VimValue from -> to

  implementation FromVim VimString String where
    fromVim (MkVimString str) = believe_me str

  implementation FromVim VimInt Int where
    fromVim (MkVimInt n) = believe_me n

  namespace Array
    empty : VIM_IO (VimValue VimArray)
    empty = do
      ptr <- vimCode "Idris_array_empty" (VIM_IO Ptr)
      pure (MkVimArray ptr)

    prepend : VimValue t -> VimValue VimArray -> VIM_IO (VimValue VimArray)
    prepend x xs = do
      ptr <- vimCode "Idris_array_prepend" (Ptr -> Ptr -> VIM_IO Ptr) (unpack x) (unpack xs)
      pure (MkVimArray ptr)

    append : VimValue t -> VimValue VimArray -> VIM_IO (VimValue VimArray)
    append x xs = do
      ptr <- vimCode "Idris_array_append" (Ptr -> Ptr -> VIM_IO Ptr) (unpack x) (unpack xs)
      pure (MkVimArray ptr)

    concat : VimValue VimArray -> VimValue VimArray -> VIM_IO (VimValue VimArray)
    concat x1 x2 = do
      ptr <- vimCode "Idris_array_concat" (Ptr -> Ptr -> VIM_IO Ptr) (unpack x1) (unpack x2)
      pure (MkVimArray ptr)

    -- toVimArray : (Traversable f) => f String -> VIM_IO (VimArray )
    -- toVimArray {from} {to} xs = do
    --   arr <- empty
    --   traverse_ (\x => arr `push` (toJS {from} {to} x)) xs
    --   pure arr


  line : VimValue VimString -> VIM_IO (VimValue VimInt)
  line s = do
    ptr <- vimCode "line" (Ptr -> VIM_IO Ptr) (unpack s)
    pure (MkVimInt ptr)

  getline : VimValue VimInt -> VIM_IO (VimValue VimString)
  getline n = do
    ptr <- vimCode "getline" (Ptr -> VIM_IO Ptr) (unpack n)
    pure (MkVimString ptr)

  echo : VimValue t -> VIM_IO ()
  echo v = vimCode "Idris_echo" (Ptr -> VIM_IO ()) (unpack v)

  match : String -> String -> Int
  match str regexp =
    unsafePerformIO
      (vimCode "match" (String -> String -> VIM_IO Int) str regexp)

  matchstr : String -> String -> String
  matchstr str regexp =
    unsafePerformIO
      (vimCode "matchstr" (String -> String -> VIM_IO String) str regexp)

substr : Int -> Int -> String -> String
substr = prim__strSubstr

length : String -> Int
length = prim_lenString

main : Vim.IO ()
main = do
  l <- Vim.line (toVim "$")
  s <- Vim.getline (toVim (the Int (fromVim l) - 1))
  Vim.echo s
  arr1 <-
    Array.empty
    >>= Array.prepend (toVim {from=Int} {to=VimInt} 333)
    >>= Array.append (toVim {from=Int} {to=VimInt} 666)
  arr2 <-
    Array.empty
    >>= Array.prepend (toVim {from=Int} {to=VimInt} 222)
    >>= Array.append (toVim {from=Int} {to=VimInt} 444)
  arr <- Array.concat arr1 arr2
  Vim.echo arr
