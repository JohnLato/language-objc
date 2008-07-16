{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Common.Constants
-- Copyright   :  (c) 2007..2008 Duncan Coutts, Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- This module provides support for representing, checking and exporting c
-- constants, i.e. integral, float, character and string constants.
-----------------------------------------------------------------------------
module Language.C.Common.Constants (
  -- * Utilities
  escapeChar, unescapeChar, unescapeString,
  Flags(..), noFlags, setFlag, clearFlag, testFlag,
  -- * C char constants (and multi-character character constants)
  cchar, cchar_w, cchars, CChar(..), getCChar, isWideChar, showCharConst,
  -- * C integral constants
  CIntFlag(..), cinteger, CInteger(..), getCInteger,readCInteger,
  -- * C floating point constants
  cfloat,  CFloat(..), readCFloat,
  -- * C string literals
  cstring, cstring_w, CString(..), getCString, showStringLit, concatCStrings,
)
where
import Data.Bits
import Data.Char
import Numeric (showOct, showHex, readHex, readOct, readDec)
import Language.C.Common.Node
import Language.C.Common.Position
import Data.Generics

-- | C char constants (abstract)
data CChar = CChar 
              {-# UNPACK #-} !Char
              {-# UNPACK #-} !Bool  -- wide flag
           | CChars
                              [Char] -- multi-character character constant
              {-# UNPACK #-} !Bool   -- wide flag
           deriving (Eq,Ord,Data,Typeable)

instance Show CChar where
    showsPrec _ (CChar c wideflag)   = _showWideFlag wideflag . showCharConst c
    showsPrec _ (CChars cs wideflag) = _showWideFlag wideflag . (sQuote $ concatMap escapeCChar cs)

-- | @showCharConst c@ prepends _a_ String representing the C char constant corresponding to @c@.
-- If neccessary uses octal or hexadecimal escape sequences.
showCharConst :: Char -> ShowS
showCharConst c = sQuote $ escapeCChar c

_showWideFlag :: Bool -> ShowS
_showWideFlag flag = if flag then showString "L" else id

-- | get the haskell representation of a char constant
getCChar :: CChar -> [Char]
getCChar (CChar  c _)   = [c]
getCChar (CChars  cs _) = cs

-- | return @true@ if the character constant is /wide/.
isWideChar :: CChar -> Bool
isWideChar (CChar _ wideFlag) = wideFlag
isWideChar (CChars _ wideFlag) = wideFlag

-- | construct a character constant from a haskell 'Char'
-- Use 'cchar_w' if you want a wide character constant.
cchar :: Char -> CChar
cchar c = CChar c False

-- | construct a wide chararacter constant
cchar_w :: Char -> CChar
cchar_w c = CChar c True

-- | create a multi-character character constant
cchars :: [Char] -> Bool -> CChar
cchars = CChars

-- | datatype representing type flags for integers
data CIntFlag = FlagUnsigned | FlagLong | FlagLongLong | FlagImag deriving (Eq,Ord,Enum,Bounded,Data,Typeable) 
instance Show CIntFlag where
    show FlagUnsigned = "u"
    show FlagLong = "L"
    show FlagLongLong = "LL"
    show FlagImag = "i"
    
{-# SPECIALIZE setFlag :: CIntFlag -> Flags CIntFlag -> Flags CIntFlag #-}
{-# SPECIALIZE clearFlag :: CIntFlag -> Flags CIntFlag -> Flags CIntFlag #-}
{-# SPECIALIZE testFlag :: CIntFlag -> Flags CIntFlag -> Bool #-}

data CInteger = CInteger 
                 {-# UNPACK #-} !Integer
                 {-# UNPACK #-} !(Flags CIntFlag)  -- integer flags
                 deriving (Eq,Ord,Data,Typeable)
instance Show CInteger where
    showsPrec _ (CInteger i flags) = shows i . showString (concatMap showIFlag [FlagUnsigned .. ]) where
        showIFlag f = if testFlag f flags then show f else []

-- To be used in the lexer
-- Note that the flag lexer won't scale
readCInteger :: ReadS Integer -> String -> Either String CInteger
readCInteger readNum str = 
  case readNum str of
    [(n,suffix)] -> mkCInt n suffix
    parseFailed  -> Left $ "Bad Integer literal: "++show parseFailed
  where
    mkCInt n suffix = either Left (Right . CInteger n)  $ readSuffix suffix
    readSuffix = parseFlags noFlags 
    parseFlags flags [] = Right flags
    parseFlags flags ('l':'l':fs) = parseFlags (setFlag FlagLongLong flags) fs
    parseFlags flags ('L':'L':fs) = parseFlags (setFlag FlagLongLong flags) fs
    parseFlags flags (f:fs) = 
      let go1 flag = parseFlags (setFlag flag flags) fs in
      case f of
        'l' -> go1 FlagLong ; 'L' -> go1 FlagLong
        'u' -> go1 FlagUnsigned ; 'U' -> go1 FlagUnsigned
        'i' -> go1 FlagImag ; 'I' -> go1 FlagImag; 'j' -> go1 FlagImag; 'J' -> go1 FlagImag
        _ -> Left $ "Unexpected flag " ++ show f

getCInteger :: CInteger -> Integer
getCInteger (CInteger i _) = i

-- | construct a integer constant (without type flags) from a haskell integer
cinteger :: Integer -> CInteger
cinteger i = CInteger i noFlags

-- | Floats (represented as strings)
data CFloat = CFloat 
                {-# UNPACK #-} !String
                 deriving (Eq,Ord,Data,Typeable)
instance Show CFloat where
  showsPrec _ (CFloat internal) = showString internal

cfloat :: Float -> CFloat
cfloat = CFloat . show

-- dummy implementation
readCFloat :: String -> CFloat
readCFloat = CFloat

-- | C String literals
data CString = CString 
                [Char]    -- characters 
                Bool      -- wide flag
                deriving (Eq,Ord,Data,Typeable)
instance Show CString where
    showsPrec _ (CString str wideflag) = _showWideFlag wideflag . showStringLit str

-- construction
cstring :: String -> CString
cstring str = CString str False
cstring_w :: String -> CString
cstring_w str = CString str True

-- selectors
getCString :: CString -> String
getCString (CString str _) = str
isWideString :: CString -> Bool
isWideString (CString _ wideflag) = wideflag

-- | concatenate a list of C string literals
concatCStrings :: [CString] -> CString
concatCStrings cs = CString (concatMap getCString cs) (any isWideString cs)

-- | @showStringLiteral s@ prepends a String representing the C string literal corresponding to @s@.
-- If neccessary it uses octal or hexadecimal escape sequences.
showStringLit :: String -> ShowS
showStringLit = dQuote . concatMap showStringChar
  where
  showStringChar c | isSChar c = return c
                     | c == '"'  = "\\\""
                     | otherwise = escapeChar c



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

-- | @escapeCChar c@ escapes c for use in a char constant
escapeCChar :: Char -> String
escapeCChar '\'' = "\\'"
escapeCChar c | isCChar c = [c]
              | otherwise = escapeChar c

-- | @isSChar c@ returns true if c is a source character which does not have to be escaped in C string
--  literals (C99: 6.4.5)
isSChar :: Char -> Bool
isSChar '\\' = False
isSChar '\"' = False
isSChar '\n' = False
isSChar c = isAsciiSourceChar c

escapeChar :: Char -> String                     
escapeChar '\\' = "\\\\"
escapeChar '\a' = "\\a"
escapeChar '\b' = "\\b"
escapeChar '\ESC' = "\\e";
escapeChar '\f' = "\\f"
escapeChar '\n' = "\\n"
escapeChar '\r' = "\\r"
escapeChar '\t' = "\\t"
escapeChar '\v' = "\\v"
escapeChar c  | (ord c) < 512   = '\\' : showOct (ord c) ""
              | otherwise       = '\\' : 'x'  : showHex (ord c) ""

unescapeChar :: String -> (Char, String)
unescapeChar ('\\':c:cs)  = case c of
       'n'  -> ('\n', cs)
       't'  -> ('\t', cs)
       'v'  -> ('\v', cs)
       'b'  -> ('\b', cs)
       'r'  -> ('\r', cs)
       'f'  -> ('\f', cs)
       'a'  -> ('\a', cs)
       'e'  -> ('\ESC', cs)  -- GNU extension
       'E'  -> ('\ESC', cs)  -- GNU extension
       '\\' -> ('\\', cs)
       '?'  -> ('?', cs)
       '\'' -> ('\'', cs)
       '"'  -> ('"', cs)
       'x'  -> case head' "bad escape sequence" (readHex cs) of
                 (i, cs') -> (toEnum i, cs')
       _    -> case head' "bad escape sequence" (readOct (c:cs)) of
                 (i, cs') -> (toEnum i, cs')
unescapeChar (c   :cs)    = (c, cs)
unescapeChar []  = error $ "unescape char: empty string"

unescapeString :: String -> String
unescapeString [] = []
unescapeString cs = case unescapeChar cs of
                        (c, cs') -> c : unescapeString cs'
                        
-- helpers
sQuote :: String -> ShowS
sQuote s t = "'" ++ s ++ "'" ++ t
dQuote :: String -> ShowS
dQuote s t = ('"' : s) ++ "\"" ++ t
head' :: String -> [a] -> a
head' err []  = error err
head' _ (x:_) = x

-- TODO: Move to seperate file ?
newtype Flags f = Flags Integer deriving (Eq,Ord,Data,Typeable)
noFlags :: Flags f
noFlags = Flags 0
setFlag :: (Enum f) => f -> Flags f -> Flags f
setFlag flag (Flags k)   = Flags$ k  `setBit` fromEnum flag
clearFlag :: (Enum f) => f -> Flags f -> Flags f
clearFlag flag (Flags k) = Flags$ k `clearBit` fromEnum flag
testFlag :: (Enum f) => f -> Flags f -> Bool
testFlag flag (Flags k)  = k `testBit` fromEnum flag


