module IRTS.CodegenVim.Internal.ZEncoding (zEncode) where

import Data.Char
import Numeric

isUnencoded :: Char -> Bool   -- True for chars that don't need encoding
isUnencoded 'Z' = False
isUnencoded 'z' = False
isUnencoded c   =  c >= 'a' && c <= 'z'
                || c >= 'A' && c <= 'Z'
                || c >= '0' && c <= '9'

zEncode :: Char -> String
zEncode c | c >= '0' && c <= '9' = encodeUnicode c
zEncode c | otherwise            = encodeCh c

encodeCh :: Char -> String
encodeCh c | isUnencoded c = [c]
-- encodeCh '('  = "ZL"   
-- encodeCh ')'  = "ZR"   
-- encodeCh '['  = "ZM"
-- encodeCh ']'  = "ZN"
encodeCh c = case c of
  ':'  -> "ZC" -- Colon
  'Z' -> "ZZ"
  'z' -> "zz"
  '&' -> "za" -- And
  '|' -> "zb"
  '^' -> "zc" -- Caret
  '$' -> "zd" -- Dollar
  '=' -> "ze" -- Equals
  '>' -> "zg" -- Greater than
  '#' -> "zh" -- Hash
  '.' -> "zi" -- ... there's a dot on top of an 'i'?
  '<' -> "zl" -- Less than
  '-' -> "zm" -- eM-dash?
  '!' -> "zn" 
  '+' -> "zp" -- Plus
  '\'' -> "zq" -- Quote?
  '\\' -> "zr"
  '/' -> "zs" -- Slash
  '*' -> "zt" -- Times
  '_' -> "zu" -- Underscore
  '%' -> "zv"
  _ -> encodeUnicode c

encodeUnicode :: Char -> String
encodeUnicode c = 'z' : if isDigit (head hexStr) then hexStr
                                                           else '0':hexStr
  where hexStr = showHex (ord c) "U"
