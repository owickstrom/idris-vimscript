module Vimscript.Builtins

import Vimscript.FFI

%access export

line : String -> VIM_IO Int
line = vimCode (VIM_BuiltIn "line") (String -> VIM_IO Int)

getline : Int -> VIM_IO String
getline = vimCode (VIM_BuiltIn "getline") (Int -> VIM_IO String)

echo : t -> VIM_IO ()
echo v = vimCode VIM_Echo (Raw t -> VIM_IO ()) (MkRaw v)

match : String -> String -> Int
match str regexp =
  unsafePerformIO
    (vimCode (VIM_BuiltIn "match") (String -> String -> VIM_IO Int) str regexp)

matchstr : String -> String -> String
matchstr str regexp =
  unsafePerformIO
    (vimCode (VIM_BuiltIn "matchstr") (String -> String -> VIM_IO String) str regexp)
