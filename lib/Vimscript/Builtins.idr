module Vimscript.Builtins

import Vimscript.FFI
import Vimscript.List

%access export

builtin : String -> (ty : Type) -> {auto fty : FTy FFI_VIM [] ty} -> ty
builtin name = foreign FFI_VIM (VIM_BuiltIn name)

||| Execute a string as Vimscript.
%inline
execute : String -> VIM_IO ()
execute = builtin "execute" (String -> VIM_IO ())

||| Set a Vim option. Type-unsafe.
%inline
set : String -> String -> VIM_IO ()
set o v = execute ("set " ++ o ++ "=" ++ v)

%inline
line : String -> VIM_IO Int
line = builtin "line" (String -> VIM_IO Int)

%inline
getline : Int -> VIM_IO String
getline = builtin "getline" (Int -> VIM_IO String)

%inline
appendLines : Int -> VimList String -> VIM_IO ()
appendLines i l =
  builtin "append" (Int -> Raw (VimList String) -> VIM_IO ()) i (MkRaw l)

%inline
echo : (x : t) -> VIM_IO ()
echo {t} v = foreign FFI_VIM VIM_Echo (Raw t -> VIM_IO ()) (MkRaw v)

%inline
match : String -> String -> Int
match str regexp = unsafePerformIO
    (builtin "match" (String -> String -> VIM_IO Int) str regexp)

%inline
matchstr : String -> String -> String
matchstr str regexp = unsafePerformIO
    (builtin "matchstr" (String -> String -> VIM_IO String) str regexp)

%inline
shellescape : String -> String
shellescape str = unsafePerformIO
    (builtin "shellescape" (String -> VIM_IO String) str)

%inline
toupper : String -> String
toupper str = unsafePerformIO
    (builtin "toupper" (String -> VIM_IO String) str)

%inline
tolower : String -> String
tolower str = unsafePerformIO
    (builtin "tolower" (String -> VIM_IO String) str)

||| Compute the length of a string.
%inline
strlen : String -> Int
strlen str = unsafePerformIO
    (builtin "strlen" (String -> VIM_IO Int) str)

||| Extract a substring from a start index to an end index.
%inline
strpart : String -> (start : Int) -> (end : Int) -> String
strpart str start end = unsafePerformIO
    (builtin "strpart" (String -> Int -> Int -> VIM_IO String) str start end)

||| Extract a substring from a start index to the (implicit) end.
%inline
strpart' : String -> (start : Int) -> String
strpart' str start = unsafePerformIO
    (builtin "strpart" (String -> Int -> VIM_IO String) str start)

||| Access Vim's built-in globbing functionality.
||| 
||| Current file name:
||| ```idris
||| expand '%'
||| ```
|||
||| Word under cursor:
||| ```idris
||| expand '<cword>'
||| ```
expand : String -> VIM_IO String
expand str = builtin "expand" (String -> VIM_IO String) str 
