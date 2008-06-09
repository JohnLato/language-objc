-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.AST.Constants
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- This module provides support for representing, checking and exporting c
-- constants, i.e. integral, float, character and string constants.
--
-- TODO: Move representation of constants from AST to here.
-----------------------------------------------------------------------------
module Language.C.AST.Constants (
  showIntConstant,
  showCharConstant,
  showStringLiteral,
)
where
import Data.Char
import Numeric (showOct, showHex)

-- | @showIntConstant i@ prepends the String representing the C expression for the integer i 
showIntConstant :: Integer -> ShowS
showIntConstant i = shows i

-- | @isAsciiSourceChar b@ returns @True@ if the given character is a character which
--   may appear in a ASCII C source file and is printable.
isAsciiSourceChar :: Char -> Bool
isAsciiSourceChar c = isAscii c && isPrint c

-- | @isCChar c@ returns true, if c is a source character which does not have to be escaped in
--   C char constants (C99: 6.4.4.4)
isCChar :: Char -> Bool
isCChar '\\' = False
isCChar '\'' = False
isCChar '\n' = False
isCChar c = isAsciiSourceChar c

-- | @isSChar c@ returns true if c is a source character which does not have to be escaped in C string
--  literals (C99: 6.4.5)
isSChar :: Char -> Bool
isSChar '\\' = False
isSChar '\"' = False
isSChar '\n' = False
isSChar c = isAsciiSourceChar c

-- | @showCharConstant c@ prepends _a_ String representing the C char constant corresponding to @c@.
-- If neccessary uses octal or hexadecimal escape sequences.
showCharConstant :: Char -> ShowS
showCharConstant c | isCChar c = shows c
                   | c == '\'' = sQuote $ "\\'"
                   | otherwise = sQuote $ escapeChar c
                     
-- | @showStringLiteral s@ prepends a String representing the C string literal corresponding to @s@.
-- If neccessary it uses octal or hexadecimal escape sequences.
showStringLiteral :: String -> ShowS
showStringLiteral = dQuote . concatMap showStringChar
  where
  showStringChar c | isSChar c = return c
                     | c == '"'  = "\\\""
                     | otherwise = escapeChar c

escapeChar :: Char -> String                     
escapeChar '\\' = "\\\\"
escapeChar '\a' = "\\a"
escapeChar '\b' = "\\b"
escapeChar '\f' = "\\f"
escapeChar '\n' = "\\n"
escapeChar '\r' = "\\r"
escapeChar '\t' = "\\t"
escapeChar '\v' = "\\v"
escapeChar c  | (ord c) < 512   = '\\' : showOct (ord c) ""
                 | otherwise       = '\\' : 'x'  : showHex (ord c) ""

-- helpers
sQuote :: String -> ShowS
sQuote s t = "'" ++ s ++ "'" ++ t
dQuote :: String -> ShowS
dQuote s t = ('"' : s) ++ "\"" ++ t