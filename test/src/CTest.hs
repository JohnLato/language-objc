{-# OPTIONS -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  CTest.hs (executable)
-- Copyright   :  (c) 2008 Duncan Coutts, Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  non-portable (Data.Generics)
--
-- This is a very simple module, usable for quick tests.
--
-- It provides a wrapper for parsing C-files which haven't been preprocessed yet.
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
import Data.Generics
import Language.C
import Language.C.Test.CPP
import Language.C.Test.Environment
import Language.C.Test.GenericAST

data CTestConfig = CTestConfig { debugFlag :: Bool, parseOnlyFlag :: Bool, semanticAnalysis :: Bool }
main :: IO ()
main = do
  tmpdir     <- getEnv "TMPDIR"
  dbg       <- getEnvFlag "DEBUG"
  parseonly <- getEnvFlag "PARSE_ONLY"
  semantic  <- getEnvFlag "SEMANTIC_ANALYSIS" -- disabled right now
  let config = CTestConfig dbg parseonly semantic
  args <- getArgs
  -- TODO: getOpt
  case args of
      ("-e":str:[]) -> parseAndPrint config (exprInput str) (Position "stdin" 1 1)
      ("-d":str:[]) -> parseAndPrint config (declInput str) (Position "stdin" 1 1)
      otherArgs ->
          case mungeCcArgs args of
            Groked [cFile] gccOpts -> do
              ast <- parseCC tmpdir gccOpts cFile >>= either (ioError.userError.show) return
              output config ast
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
    "   DEBUG:  debug flag",
    "   SEMANTIC_ANALYSIS: perform semantic analysis",
    "   PARSE_ONLY: do not pretty print"
  ]
exprInput str = "void *x = " ++ str ++ " ;"
declInput str = str ++ ";"
parseAndPrint :: CTestConfig -> String -> Position -> IO ()
parseAndPrint config str pos = do
    ast <- either (ioError.userError.show) return (parseC (inputStreamFromString str) pos)
    output config ast
output :: CTestConfig -> CTranslUnit -> IO ()
output config ast = do
    when (not $ parseOnlyFlag config) $ print $ prettyUsingInclude ast
    when (debugFlag config) $ print . pretty . mkGenericCAST $ ast
    when (semanticAnalysis config) $  return ()
        -- case runTrav_ (translateAST ast) of
        --     Left errors -> mapM_ print errors
        --     Right (translunit, ts) -> demoAnalysis translunit
-- demoAnalysis :: TranslUnit -> IO ()
-- demoAnalysis tunit@(TranslUnit decls _) =
--     mapM_ (print . describe Spec) decls
    