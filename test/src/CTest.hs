-----------------------------------------------------------------------------
-- |
-- Module      :  CCWrapper.hs (executable)
-- Copyright   :  (c) 2008 Duncan Coutts, Benedikt Huber
--
-- This module provides a wrapper for parsing C-files which haven't been preprocessed yet.
-- It is used as if gcc is called, and internally calls gcc to preprocess the file.
-- It then outputs the pretty printed AST, replacing declarations from included header 
-- files with a corresponding #include directive (This isn't always correct, as e.g. #define s 
-- get lost. But it makes it a lot easire to focus on the relevant part of the output).
-------------------------------------------------------------------------------------------------------
module Main (
main
)  where
import System.Environment (getEnv, getArgs)
import Language.C.AST.AST
import Language.C.AST.Pretty
import Language.C.Test.CPP

main :: IO ()
main = do
  tmpdir <- getEnv "TMPDIR"
  args <- getArgs
  case mungeCcArgs  args of
    Groked cFile gccOpts -> do
      ast <- parseCC tmpdir gccOpts cFile >>= either (ioError.userError.show) return
      print $ prettyUsingInclude ast
    Ignore -> return ()
    Unknown reason -> error $ "Could not process arguments: " ++ reason
