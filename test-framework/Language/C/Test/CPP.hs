-----------------------------------------------------------------------------
-- |
-- Module      :  Preprocess
-- Copyright   :  (c) 2008 Duncan Coutts, Benedikt Huber
--
-- This module provides a wrapper for parsing C-files which haven't been preprocessed yet.
-- It is used as if gcc is called, and internally calls gcc to preprocess the file.
--
-- FIXME: Merge with the cpp function of ParseTests
-----------------------------------------------------------------------------
module Language.C.Test.CPP (
  withTemporaryFile,
  parseCC,
)  where
import System.IO
import System.Exit
import System.Cmd
import Language.C
import Language.C.Test.Environment

-- | @withTempFile directory filename-template action@ opens a temporary file in @directory@, passes the file handle to @action@,
--   then closes handle and returns the filename of the temporary file just created.
withTemporaryFile :: FilePath -> FilePath -> (Handle -> IO ()) -> IO FilePath
withTemporaryFile dir templ action = do
  (tmpFile, tmpHnd) <- openTempFile dir templ
  action tmpHnd
  hClose tmpHnd
  return tmpFile      

-- | @runGccPreprocessor [additional-gcc-options] file_in.c file_out.c@ runs 
--   @gcc <additional-gcc-options> -E -o file_out.c file_in.c@ and returns the exit code
runGccPreprocessor :: [String] -> FilePath -> FilePath -> IO ExitCode
runGccPreprocessor gccArgs inFile outFile =
  rawSystem "gcc" (gccArgs ++ ["-E", "-o", outFile, inFile])

-- | @parseCC tmp-dir [additional-gcc-options] file.c@ returns the AST of the parsed file and of all included header files.
--   @parseCC tmp-dir _ file.i@ returns the AST of the already preprocessed file @file.i@
parseCC :: FilePath -> [String] -> FilePath -> IO (Either ([String],Position) CTranslUnit)
parseCC _ _ cFile | isPreprocessedFile cFile = do
  input <- readInputStream cFile
  return $ parseC input (Position cFile 1 1)
parseCC tmpdir gccArgs cFile = do
  -- preprocess C file
  preFile <- withTemporaryFile tmpdir "parseCC.i" (\_ -> return ())
  gccExitcode <- runGccPreprocessor gccArgs cFile preFile
  preDat <- 
    case gccExitcode of
      ExitSuccess -> readInputStream preFile
      ExitFailure exitCode ->
          ioError $ userError $ "C preprocessor failed: $CPP "++ show gccArgs ++ 
                                " [" ++ cFile ++ " ==> " ++ preFile ++ "] with exit code"++show exitCode
  return $ parseC preDat (Position cFile 1 1)