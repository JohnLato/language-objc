{-# LANGUAGE CPP, BangPatterns #-}
{-# OPTIONS -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.InputStream
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  provisional
-- Portability :  CPP, BangPatterns
--
-- Compile time InputStream abstraction for the parser. 
-- Supports either ByteString or String.
-------------------------------------------------------------------------------
module Language.C.InputStream (
    InputStream, readInputStream,inputStreamToString,inputStreamFromString,
    takeChar,inputStreamEmpty,takeChars,
    countLines,
)
where
#ifndef NO_BYTESTRING
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
#endif

-- Generic InputStream stuff
readInputStream :: FilePath -> IO InputStream
inputStreamToString :: InputStream -> String
{-# INLINE inputStreamToString #-}
inputStreamFromString :: String -> InputStream
takeChar :: InputStream -> (Char, InputStream)
{-# INLINE takeChar #-}
inputStreamEmpty :: InputStream -> Bool
{-# INLINE inputStreamEmpty #-}
takeChars :: Int -> InputStream -> String
{-# INLINE takeChars #-}
countLines :: InputStream -> Int
#ifndef NO_BYTESTRING
type InputStream = ByteString
takeChar bs = BS.head bs `seq`  (BS.head bs, BS.tail bs)
inputStreamEmpty = BS.null
#ifndef __HADDOCK__
takeChars !n bstr = BS.unpack $ BS.take n bstr --leaks
#endif
readInputStream = BS.readFile
inputStreamToString = BS.unpack
inputStreamFromString = BS.pack
countLines = length . BS.lines
#else
type InputStream = String
takeChar bs = (head bs, tail bs)
inputStreamEmpty = null
takeChars n str = take n str
readInputStream = readFile
inputStreamToString = id
inputStreamFromString = id
countLines = length . lines
#endif    