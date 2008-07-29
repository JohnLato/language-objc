-- Simple example demonstrating the API: parse a file, and print its definition table
module Main where
import System.Environment ; import System.FilePath
import System.IO
import Control.Arrow      ; import Control.Monad
import Debug.Trace

import Language.C              -- simple API
import Language.C.Analysis     -- analysis API
import Language.C.System.GCC   -- preprocessor used

main :: IO ()
main = do
    let usage = error "Example Usage: ./ScanFile -I/usr/include my_file.c"
    args <- getArgs
    when (length args < 1) usage

    -- get cpp options and input file
    let (opts,c_file) = (init &&& last) args

    -- parse
    ast                <- parseFile (newGCC "gcc") Nothing opts c_file
                          >>= checkResult "[parsing]"
    -- analyze
    print (pretty ast)
    
    (globals,warnings) <- (runTrav_ >>> checkResult "[analysis]") $ analyseAST ast

    -- print
    mapM (hPutStrLn stderr . show) warnings
    print $ pretty globals -- (filterGlobalDecls (fileOfInterest c_file . fileOfNode) globals)    

    where
    checkResult :: (Show a) => String -> (Either a b) -> IO b
    checkResult label = either (error . (label++) . show) return
    fileOfInterest c_file file_name = fileOfInterest' (splitExtension c_file) (splitExtension file_name)
    fileOfInterest' (c_base,c_ext) (f_base,f_ext) | c_base /= f_base = trace (show $ (c_base,c_ext,f_base,f_ext)) False
                                                  | f_ext == ".i"    = trace (show $ (c_base,c_ext,f_base,f_ext)) True
                                                  | c_ext == f_ext   = trace (show $ (c_base,c_ext,f_base,f_ext)) True
                                                  | otherwise        = error ("?")