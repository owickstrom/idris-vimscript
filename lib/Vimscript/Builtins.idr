module Vimscript.Builtins

import Vimscript.FFI
import Vimscript.List

%access export

-- Note [Inline FStr]
--
-- For all functions operating on Vim mutable references, a %inline
-- annotation is a must:
--
-- This function *must* be given a %inline annotation, otherwise the
-- `name` is not reduced sufficiently during compilation and remains
-- an FCon instead of becoming an FStr.

%inline
echo : (x : t) -> VIM_IO ()
echo {t} v = foreign FFI_VIM VIM_Echo (Raw t -> VIM_IO ()) (MkRaw v)

||| Call a Vim builtin.
%inline
builtin : String -> (ty : Type) -> {auto fty : FTy FFI_VIM [] ty} -> ty
builtin name = 
  foreign FFI_VIM (VIM_BuiltIn name)

||| Read the value of a Vim mutable reference.
%inline
readMutableRef : (ref : VIM_MutableRef) -> (name : String) -> VIM_IO String
readMutableRef ref name = 
  foreign FFI_VIM (VIM_Get ref name) (VIM_IO String)

||| Write the value of a Vim mutable reference.
%inline
writeMutableRef : (ref : VIM_MutableRef) -> (name : String) -> (x : t) -> VIM_IO ()
writeMutableRef {t} ref name x = 
  foreign FFI_VIM (VIM_Set ref name) (Raw t -> VIM_IO ()) (MkRaw x)
 
||| Read the value of a Vim option with unspecified scope (`&foo`).
%inline
readOption : (name : String) -> VIM_IO String
readOption name = readMutableRef VIM_Option name

||| Write the value of a Vim option with unspecified scope (`&foo`).
%inline
writeOption : (name : String) -> (x : t) -> VIM_IO ()
writeOption name x = writeMutableRef VIM_Option name x

||| Read the value of a Vim option with global scope (`&l:foo`).
%inline
readLocalOption : (name : String) -> VIM_IO String
readLocalOption name = readMutableRef VIM_LocalOption name

||| Write the value of a Vim option with global scope (`&l:foo`).
%inline
writeLocalOption : (name : String) -> (x : t) -> VIM_IO ()
writeLocalOption name x = writeMutableRef VIM_LocalOption name x

||| Read the value of a Vim option with global scope (`&g:foo`).
%inline
readGlobalOption : (name : String) -> VIM_IO String
readGlobalOption name = readMutableRef VIM_GlobalOption name

||| Write the value of a Vim option with global scope (`&g:foo`).
%inline
writeGlobalOption : (name : String) -> (x : t) -> VIM_IO ()
writeGlobalOption name x = writeMutableRef VIM_GlobalOption name x

||| Read the contents of a Vim register.
%inline
readRegister : (name : String) -> VIM_IO String
readRegister name = readMutableRef VIM_Register name

||| Set the contents of a Vim register.
%inline
writeRegister : (name : String) -> (x : t) -> VIM_IO ()
writeRegister name = writeMutableRef VIM_Register name

||| Execute a string as Vimscript.
%inline
execute : String -> VIM_IO ()
execute = builtin "execute" (String -> VIM_IO ())

-- [ Vim builtins ] ------------------------------------------------------------

%inline
line : String -> VIM_IO Int
line = builtin "line" (String -> VIM_IO Int)

%inline
col : String -> VIM_IO Int
col = builtin "col" (String -> VIM_IO Int)

%inline
getline : Int -> VIM_IO String
getline = builtin "getline" (Int -> VIM_IO String)

%inline
appendLines : Int -> VimList String -> VIM_IO ()
appendLines i l =
  builtin "append" (Int -> Raw (VimList String) -> VIM_IO ()) i (MkRaw l)

-- %inline
-- putStr : String -> VIM_IO ()
-- putStr str = execute ("echomsg" ++ " " ++ str)

-- %inline
-- putErr : String -> VIM_IO ()
-- putErr str = execute ("echoerr" ++ " " ++ str)

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

cursor : (line : Int) -> (col : Int) -> VIM_IO ()
cursor line col = builtin "cursor" (Int -> Int -> VIM_IO ()) line col

getcwd : VIM_IO String
getcwd = builtin "getcwd" (VIM_IO String)

||| Execute a shell command and discard the output.
system' : String -> VIM_IO ()
system' = builtin "system" (String -> VIM_IO ())

-- [ Derived operations ] ------------------------------------------------------

||| Set a Vim option of the form `setting=value`. Type-unsafe.
%inline
unsafeSetUnary : String -> String -> VIM_IO ()
unsafeSetUnary o v = execute ("set " ++ o ++ "=" ++ v)

-- [Ex commands] ---------------------------------------------------------------

split : VIM_IO ()
split = execute "split"

vsplit : VIM_IO ()
vsplit = execute "vsplit"

-- %inline
-- getpos : (mark : String) -> VIM_IO ()
-- getpos = builtin "line" (String -> VIM_IO Int)

public export
Regex : Type
Regex = String

public export
Filename : Type
Filename = String

public export
FlagSet : Type
FlagSet = String

public export
Range : Type
Range = String

entireFile : Range
entireFile = "%"

currentLine : Range
currentLine = "."

lineNum : Int -> Range
lineNum = show

lastLine : Range
lastLine = "$"

noFlags : FlagSet
noFlags = ""

public export
data Line = Last

public export
data SubRepeatFlag = UseLastSearch | GlobalReplace

public export
data Ex
  = Delete
  | Write (Maybe Filename)
  | Sub Range Regex String FlagSet
  | SubRepeat (List SubRepeatFlag)
  | Global Regex Ex
  | VGlobal Regex Ex
  | Move (Maybe Range) Line
  | Undo

ppSubRepeatFlag : SubRepeatFlag -> String
ppSubRepeatFlag UseLastSearch = "r"
ppSubRepeatFlag GlobalReplace = "g"

ppLine : Line -> String
ppLine Last = "$"

ppRange : Maybe Range -> String
ppRange Nothing = ""
ppRange (Just r) = r

ppEx : Ex -> String
ppEx Delete = "d"
ppEx (SubRepeat fs) = "s" ++ (concat (map ppSubRepeatFlag fs))
ppEx (Sub r re s f) = r ++ "s/" ++ re ++ "/" ++ s ++ "/" ++ f
ppEx (Write f)
  = case f of
         Nothing => "w"
         Just x => "w " ++ x
ppEx (Global re a) = "g/" ++ re ++ "/" ++ ppEx a
ppEx (VGlobal re a) = "v/" ++ re ++ "/" ++ ppEx a
ppEx (Move r l) = ppRange r ++ "m" ++ ppLine l

exec : Ex -> VIM_IO ()
exec = execute . ppEx

sub : Range -> Regex -> String -> FlagSet -> VIM_IO ()
sub r re s fs = exec (Sub r re s fs)

sub' : Range -> Regex -> String -> VIM_IO ()
sub' r re s = sub r re s noFlags

-- TODO escaping
global : Regex -> Ex -> VIM_IO ()
global re a = exec (Global re a)

vglobal : Regex -> Ex -> VIM_IO ()
vglobal re a = exec (VGlobal re a)

v : Regex -> Ex -> VIM_IO ()
v = vglobal

g : Regex -> Ex -> VIM_IO ()
g = global

d : Ex
d = Delete

-- m$
moveEnd : Ex
moveEnd = Move Nothing Last

check : List (Lazy Bool) -> VIM_IO ()
check x = echo (if and x then "All tests passed" else "Some tests failed")

public export
data VimSwitch
  = No VimSwitch
  | Compatible
  | ExpandTab
  | Hidden
  | Wildmenu
  | RelativeNumber
  | UndoFile
  | VisualBell

ppSwitch : VimSwitch -> String
ppSwitch vs = case vs of
  Compatible => "compatible"
  No o => "no" ++ ppSwitch o

enable : VimSwitch -> VIM_IO ()
enable s = execute ("set" ++ " " ++ ppSwitch s)

disable : VimSwitch -> VIM_IO ()
disable s = enable (No s)

public export
data Power = On | Off

total
ppPower : Power -> String
ppPower p = case p of
                 On => "on"
                 Off => "off"

public export
data VimEncoding = UTF8

public export
data VimOption
  = Syntax Power
  | TabStop Int
  | SoftTabStop Int
  | ShiftWidth Int
  | Encoding VimEncoding

set : VimOption -> VIM_IO ()
set (Syntax p) = unsafeSetUnary "syntax" (ppPower p)


