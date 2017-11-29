module Vimscript.Builtins

import Vimscript.FFI
import Vimscript.List

%access export

line : String -> VIM_IO Int
line = foreign FFI_VIM (VIM_BuiltIn "line") (String -> VIM_IO Int)

getline : Int -> VIM_IO String
getline = foreign FFI_VIM (VIM_BuiltIn "getline") (Int -> VIM_IO String)

appendLines : Int -> VimList String -> VIM_IO ()
appendLines i l =
  foreign FFI_VIM (VIM_BuiltIn "append") (Int -> Raw (VimList String) -> VIM_IO ()) i (MkRaw l)

echo : (x : t) -> VIM_IO ()
echo {t} v = foreign FFI_VIM VIM_Echo (Raw t -> VIM_IO ()) (MkRaw v)

match : String -> String -> Int
match str regexp =
  unsafePerformIO
    (foreign FFI_VIM  (VIM_BuiltIn "match") (String -> String -> VIM_IO Int) str regexp)

matchstr : String -> String -> String
matchstr str regexp =
  unsafePerformIO
    (foreign FFI_VIM  (VIM_BuiltIn "matchstr") (String -> String -> VIM_IO String) str regexp)
