module Main where

import Data.List
import Language.C
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.TravMonad
import Language.C.System.GCC
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

processFile :: CLanguage -> [String] -> FilePath -> IO ()
processFile lang cppOpts file =
  do hPutStr stderr $ file ++ ": "
     result <- parseCFile (newGCC "gcc") Nothing cppOpts file
     case result of
       Left err -> hPutStrLn stderr ('\n' : show err)
       Right tu -> case runTrav_ (body tu) of
                     Left errs -> hPutStrLn stderr ('\n' : concatMap show errs)
                     Right _   -> hPutStrLn stderr "success"
  where body tu = do modifyOptions (\opts -> opts { language = lang })
                     analyseAST tu

main :: IO ()
main =
  do args <- getArgs
     let (cppOpts, files) = partition (isPrefixOf "-") args
     mapM_ (processFile GNU99 cppOpts) files
