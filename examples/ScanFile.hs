-- Simple example demonstrating the API: parse a file, and print its definition table
module Main where
import System.Environment
import Control.Arrow
import Control.Monad

import Language.C              -- simple API
import Language.C.Analysis     -- analysis API
import Language.C.System.GCC   -- preprocessor used

main :: IO ()
main = do
    let usage = error "Example Usage: ./ScanFile -I/usr/include my_file.c"
    (args,c_file)   <- liftM ((usage  ||| (init &&& last)) <<^ listToEither) getArgs

    ast             <- parseFile (newGCC "gcc") Nothing args c_file
                           >>= checkResult "[parsing]"

    global_defs     <- checkResult "[analysis]" $
                           runTrav_ (analyseAST ast)

    print $ pretty (filterGlobalDecls ( (==c_file) . fileOfNode ) global_defs)

    where
    checkResult :: (Show a) => String -> (Either a b) -> IO b
    checkResult label = either (error . (label++) . show) return
    listToEither [] = Left  []
    listToEither xs = Right xs
    
