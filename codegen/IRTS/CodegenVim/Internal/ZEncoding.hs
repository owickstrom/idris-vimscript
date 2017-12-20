{-
 - Implements Z-encoding for name mangling.
 -
 - This is mostly taken from the `zenc` package, which contains code from 
 - the GHC source.
 - 
 - https://hackage.haskell.org/package/zenc
 -}
module IRTS.CodegenVim.Internal.ZEncoding
  ( zEncode
  ) where

import           Data.Char
import           Numeric

isUnencoded :: Char -> Bool -- True for chars that don't need encoding
isUnencoded 'Z' = False
isUnencoded 'z' = False
isUnencoded c   = isAsciiLower c || isAsciiUpper c || isDigit c

zEncode :: Char -> String
zEncode c
  | isDigit c = encodeUnicode c
zEncode c = encodeCh c

encodeCh :: Char -> String
encodeCh c
  | isUnencoded c = [c]
-- encodeCh '('  = "ZL"
-- encodeCh ')'  = "ZR"
-- encodeCh '['  = "ZM"
-- encodeCh ']'  = "ZN"
encodeCh c =
  case c of
    ':'  -> "ZC" -- Colon
    'Z'  -> "ZZ"
    'z'  -> "zz"
    '&'  -> "za" -- And
    '|'  -> "zb"
    '^'  -> "zc" -- Caret
    '$'  -> "zd" -- Dollar
    '='  -> "ze" -- Equals
    '>'  -> "zg" -- Greater than
    '#'  -> "zh" -- Hash
    '.'  -> "zi" -- ... there's a dot on top of an 'i'?
    '<'  -> "zl" -- Less than
    '-'  -> "zm" -- eM-dash?
    '!'  -> "zn"
    '+'  -> "zp" -- Plus
    '\'' -> "zq" -- Quote?
    '\\' -> "zr"
    '/'  -> "zs" -- Slash
    '*'  -> "zt" -- Times
    '_'  -> "zu" -- Underscore
    '%'  -> "zv"
    _    -> encodeUnicode c

encodeUnicode :: Char -> String
encodeUnicode c =
  'z' :
  if isDigit (head hexStr)
    then hexStr
    else '0' : hexStr
  where
    hexStr = showHex (ord c) "U"
