{-# LANGUAGE CPP, BangPatterns #-}
{-# OPTIONS -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.ObjC.Data.InputStream
-- Copyright   :  (c) 2008,2011 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  jwlato@gmail.com
-- Stability   :  experimental
-- Portability :  ghc
--
-- Compile time input abstraction for the parser, relying on ByteString.
-- The String interface only supports Latin-1 since alex-3, as alex now requires
-- byte based access to the input stream.
-------------------------------------------------------------------------------
module Language.ObjC.Data.InputStream (
    InputStream, readInputStream,inputStreamToString,inputStreamFromString,
    takeByte, takeChar, inputStreamEmpty, takeChars,
    countLines,
)
where

import Data.Word

#ifndef NO_BYTESTRING
import Data.ByteString (ByteString)
import qualified Data.ByteString as BSW
import qualified Data.ByteString.Char8 as BSC
#else
import qualified Data.Char as Char
#endif

-- Generic InputStream stuff

-- | read a file into an 'InputStream'
readInputStream :: FilePath -> IO InputStream

-- | convert 'InputStream' to 'String'
inputStreamToString :: InputStream -> String
{-# INLINE inputStreamToString #-}

-- | convert a 'String' to an 'InputStream'
inputStreamFromString :: String -> InputStream

-- | @(b,is') = takeByte is@ reads and removes
-- the first byte @b@ from the 'InputStream' @is@
takeByte :: InputStream -> (Word8, InputStream)
{-# INLINE takeByte #-}

-- | @(c,is') = takeChar is@ reads and removes
-- the first character @c@ from the 'InputStream' @is@
takeChar :: InputStream -> (Char, InputStream)
{-# INLINE takeChar #-}

-- | return @True@ if the given input stream is empty
inputStreamEmpty :: InputStream -> Bool
{-# INLINE inputStreamEmpty #-}

-- | @str = takeChars n is@ returns the first @n@ characters
-- of the given input stream, without removing them
takeChars :: Int -> InputStream -> [Char]
{-# INLINE takeChars #-}

-- | @countLines@ returns the number of text lines  in the
-- given 'InputStream'
countLines :: InputStream -> Int

#ifndef NO_BYTESTRING

type InputStream = ByteString
takeByte bs = BSW.head  bs `seq`  (BSW.head bs, BSW.tail bs)
takeChar bs = BSC.head bs `seq`  (BSC.head bs, BSC.tail bs)
inputStreamEmpty = BSW.null
#ifndef __HADDOCK__
takeChars 0  _    = []
takeChars !n bstr = let str = BSC.unpack $ BSC.take n bstr --leaks
                    in  tail str `seq` str
#endif
readInputStream       = BSW.readFile

inputStreamToString   = BSC.unpack
inputStreamFromString = BSC.pack
countLines             = length . BSC.lines


#else

type InputStream = String
takeByte bs
  | Char.isLatin1 c = let b = fromIntegral (Char.ord c) in b `seq` (b, tail bs)
  | otherwise       = error "takeByte: not a latin-1 character"
  where c = head bs
takeChar bs = (head bs, tail bs)
inputStreamEmpty = null
takeChars n str = take n str
readInputStream = readFile
inputStreamToString = id
inputStreamFromString = id
countLines = length . lines
#endif

