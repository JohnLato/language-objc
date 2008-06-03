-----------------------------------------------------------------------------
-- |
-- Module      :  Preprocess
-- Copyright   :  (c) 2007 Duncan Coutts
--                    2008 Benedikt Huber
--
-- This module provides a wrapper for parsing C-files which haven't been preprocessed yet.
-- It is used as if gcc is called, and internally calls gcc to preprocess the file.
-----------------------------------------------------------------------------
module CPP (
  withTempFile,
  parseCC,
  MungeResult(..), mungeCcArgs,
)  where
import Control.Monad (liftM)
import Data.List (isSuffixOf)
import System.IO
import System.Exit
import System.Cmd
import Language.C.Toolkit.Position
import Language.C.Toolkit.State
import Language.C.Parser.Parser (parseC)
import Language.C.AST.AST

-- | @withTempFile directory filename-template action@ opens a temporary file in @directory@, passes the file handle to @action@,
--   then closes handle and returns the filename of the temporary file just created.
withTempFile :: FilePath -> FilePath -> (Handle -> IO ()) -> IO FilePath
withTempFile dir templ action = do
  (tmpFile, tmpHnd) <- openTempFile dir templ
  action tmpHnd
  hClose tmpHnd
  return tmpFile      

-- | @runGccPreprocessor [additional-gcc-options] file_in.c file_out.c@ runs 
--   @gcc <additional-gcc-options> -E -o file_out.c file_in.c@ and returns the exit code
runGccPreprocessor :: [String] -> FilePath -> FilePath -> IO ExitCode
runGccPreprocessor gccArgs inFile outFile =
  rawSystem "gcc" (gccArgs ++ ["-E", "-o", outFile, inFile])

-- | @parseCC tmp-dir [additional-gcc-options] file.c@ returns the AST of the parsed file and all included header files.
parseCC :: FilePath -> [String] -> FilePath -> IO (Either ([String],Position) CHeader)
parseCC tmpDir gccArgs cFile = do
  -- preprocess C file
  preFile <- withTempFile tmpDir "parseCC.c" (\_ -> return ())
  gccExitcode <- runGccPreprocessor gccArgs cFile preFile
  preDat <- 
    case gccExitcode of
      ExitSuccess -> readFile preFile
      ExitFailure exitCode ->
          ioError $ userError $ "C preprocessor failed: $CPP "++ show gccArgs ++ 
                                " [" ++ cFile ++ " ==> " ++ preFile ++ "] with exit code"++show exitCode
  return $ parseC preDat (Position cFile 1 1)

data MungeResult = Unknown String | Ignore | Groked FilePath [String]

-- | Collect and process cmd line args for gcc
mungeCcArgs :: [String] -> MungeResult
mungeCcArgs = mungeArgs [] ""

mungeArgs :: [String] -> String -> [String] -> MungeResult
mungeArgs accum []    [] = Unknown "No .c / .hc / .i source file given"
mungeArgs accum cfile [] = Groked cfile (reverse accum)
-- ignore preprocessing - only calls
mungeArgs accum cfile ("-E":args) = Ignore
-- ignore make-rule creation
mungeArgs accum cfile ("-M":args) = Ignore
-- strip outfile
mungeArgs accum cfile ("-o":outfile:args) = mungeArgs accum cfile args
mungeArgs accum cfile (cfile':args)
          | ".c" `isSuffixOf` cfile'
         || ".hc" `isSuffixOf` cfile'
         || ".i"  `isSuffixOf` cfile' =
              if null cfile
                then mungeArgs (cfile':accum) cfile' args
                else Unknown $ "Two source files given: " ++ cfile ++ " and " ++ cfile'
mungeArgs accum cfile (cfile':args)
          | ".S" `isSuffixOf` cfile' = Ignore
mungeArgs accum cfile (arg:args) = mungeArgs (arg:accum) cfile args