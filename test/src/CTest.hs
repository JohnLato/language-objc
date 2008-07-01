{-# OPTIONS -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  CTest.hs (executable)
-- Copyright   :  (c) 2008 Duncan Coutts, Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- This module provides a wrapper for parsing C-files which haven't been preprocessed yet.
-- It is used as if gcc was called, and internally calls cpp (gcc -E) to preprocess the file.
-- It then outputs the pretty printed AST, replacing declarations from included header 
-- files with a corresponding #include directive (This isn't always correct, as e.g. #define s 
-- get lost. But it makes it a lot easier to focus on the relevant part of the output).
--
-- If used with a `-e str' command-line argument, the given string is parsed as an expression and pretty
-- printed. Similar for `-d str' and top-level declarations.
-------------------------------------------------------------------------------------------------------
module Main (
main
)  where
import Control.Monad
import System.Environment (getEnv, getArgs)
import System.IO
import Language.C
import Language.C.Test.CPP
import Language.C.Test.Environment
import Language.C.Test.GenericAST


main :: IO ()
main = do
  tmpdir     <- getEnv "TMPDIR"
  debugFlag  <- getEnvFlag "DEBUG"
  args <- getArgs
  -- TODO: getOpt
  case args of
      ("-e":str:[]) -> parseAndPrint debugFlag (exprInput str) (Position "stdin" 1 1)
      ("-d":str:[]) -> parseAndPrint debugFlag (declInput str) (Position "stdin" 1 1)
      otherArgs ->
          case mungeCcArgs args of
            Groked [cFile] gccOpts -> do
              ast <- parseCC tmpdir gccOpts cFile >>= either (ioError.userError.show) return
              print $ prettyUsingInclude ast
              when (debugFlag) $ print . pretty . mkGenericCAST $ ast
            Groked cFiles _ -> usage $ "More than one source file given: " ++ unwords cFiles
            Ignore -> usage $ "Not input files given"
            Unknown reason -> usage $ "Could not process arguments: " ++ reason

usage :: String -> IO ()
usage msg = hPutStr stderr . unlines $ 
  [ "! "++msg,"",
    "Usage: ./CTest -e expression",
    "Usage: ./CTest -d declaration",
    "Usage: ./CTest [cpp-opts] file.(c|hc|i)",
    "   parses the given C source file and pretty print the AST",
    "Environment Variables: ",
    "   TMPDIR: temporary directory for preprocessing",
    "   DEBUG:  debug flag"
  ]
exprInput str = "void *x = " ++ str ++ " ;"
declInput str = str ++ ";"
parseAndPrint :: Bool -> String -> Position -> IO ()
parseAndPrint debugFlag str pos = do
    ast <- either (ioError.userError.show) return (parseC (inputStreamFromString str) pos)
    print $ prettyUsingInclude ast
    when (debugFlag) $ print . pretty . mkGenericCAST $ ast
