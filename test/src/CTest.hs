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
import Language.C
import Language.C.System.GCC
import Language.C.Analysis
import Language.C.Test.Environment
import Language.C.Test.GenericAST

import Control.Monad
import System.Environment (getEnv, getArgs)
import System.Exit
import System.IO
import Data.Generics
import Text.PrettyPrint.HughesPJ

data CTestConfig = CTestConfig { debugFlag :: Bool, parseOnlyFlag :: Bool, useIncludes :: Bool,
                                 dumpAst :: Bool, semanticAnalysis :: Bool }
main :: IO ()
main = do
  tmpdir     <- getEnv "TMPDIR"
  dbg       <- getEnvFlag "DEBUG"
  parseonly <- getEnvFlag "PARSE_ONLY"
  dumpast   <- getEnvFlag "DUMP_AST"
  no_includes <- getEnvFlag "NO_HEADERS_VIA_INCLUDE"
  semantic  <- liftM not (getEnvFlag "NO_SEMANTIC_ANALYSIS")
  let config = CTestConfig dbg parseonly (not $ no_includes) dumpast semantic
  args <- getArgs
  let stdinPos = Position "<stdin>" 1 1
  (file,ast) <-
    case args of
      ("-e":str:[]) -> either bailOut (return.((,) ""))
                         (parseC (inputStreamFromString (exprInput str)) stdinPos)
      ("-d":str:[]) -> either bailOut (return.((,) ""))
                         (parseC (inputStreamFromString (exprInput str)) stdinPos)
      otherArgs ->
          case mungeCcArgs args of
            Groked [cFile] gccOpts -> do
                parseFile (newGCC "gcc") (Just tmpdir) gccOpts cFile >>= either bailOut (return.((,) cFile))
            Groked cFiles _ -> usage $ "More than one source file given: " ++ unwords cFiles
            Ignore -> usage $ "Not input files given"
            Unknown reason -> usage $ "Could not process arguments: " ++ reason
  output config file ast
bailOut :: (Show err) => err -> IO a
bailOut err = do
    hPutStrLn stderr (show err)
    hPutStrLn stderr "*** Exit on Error ***"
    exitWith (ExitFailure 1)
usage :: String -> IO a
usage msg = printUsage >> exitWith (ExitFailure 2) where
  printUsage = hPutStr stderr . unlines $ 
      [ "! "++msg,"",
        "Usage: ./CTest -e expression",
        "Usage: ./CTest -d declaration",
        "Usage: ./CTest [cpp-opts] file.(c|hc|i)",
        "   parses the given C source file and pretty print the AST",
        "Environment Variables: ",
        "   TMPDIR: temporary directory for preprocessing",
        "   NO_HEADERS_VIA_INCLUDE: do not use heuristic #include directives for pretty printing",
        "   DEBUG:  debug flag",
        "   DUMP_AST:  dump the ast to file dump.ast",
        "   NO_SEMANTIC_ANALYSIS: do not perform semantic analysis",
        "   PARSE_ONLY: do not pretty print"
      ]
exprInput str = "void *x = " ++ str ++ " ;"
declInput str = str ++ ";"
output :: CTestConfig -> FilePath -> CTranslUnit -> IO ()
output config file ast = do
    when (dumpAst config) $ writeFile "dump.ast" (gshow ast)
    when (semanticAnalysis config && (not (null file))) $ do
        let result = runTrav_ (analyseAST ast)
        case result of
            Left errs -> hPutStrLn stderr (show errs)
            Right (ok,warnings) -> do mapM (hPutStrLn stderr . show) warnings
                                      printStats file ok
    when (not $ parseOnlyFlag config) $ print $ (if useIncludes config then prettyUsingInclude else pretty) ast
    when (debugFlag config) $ putStrLn . comment . show . pretty . mkGenericCAST $ ast
comment str = "/*\n" ++ str ++ "\n*/"
printStats file = putStrLn . comment . show . prettyAssocsWith "global decl stats" text (text.show) . globalDeclStats (== file)

