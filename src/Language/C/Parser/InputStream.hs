{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Parser.InputStream
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  
--
-- InputStream abstraction to support both ByteString and String
-----------------------------------------------------------------------------
module Language.C.Parser.InputStream (
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
inputStreamFromString :: String -> InputStream
takeChar :: InputStream -> (Char, InputStream)
inputStreamEmpty :: InputStream -> Bool
takeChars :: Int -> InputStream -> String
countLines :: InputStream -> Int

#ifndef NO_BYTESTRING
type InputStream = ByteString
takeChar bs = (BS.head bs, BS.tail bs)
inputStreamEmpty = BS.null
takeChars n str = BS.unpack$ BS.take n str
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