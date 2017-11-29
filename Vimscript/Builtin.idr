module Vimscript.Builtins

import Vimscript.FFI

%access export

line : String -> VIM_IO Int
line = foreign FFI_VIM (VIM_BuiltIn "line") (String -> VIM_IO Int)

getline : Int -> VIM_IO String
getline = foreign FFI_VIM (VIM_BuiltIn "getline") (Int -> VIM_IO String)

echo : t -> VIM_IO ()
echo {t} v = foreign FFI_VIM VIM_Echo (Raw t -> VIM_IO ()) (MkRaw v)

match : String -> String -> Int
match str regexp =
  unsafePerformIO
    (foreign FFI_VIM  (VIM_BuiltIn "match") (String -> String -> VIM_IO Int) str regexp)

matchstr : String -> String -> String
matchstr str regexp =
  unsafePerformIO
    (foreign FFI_VIM  (VIM_BuiltIn "matchstr") (String -> String -> VIM_IO String) str regexp)
