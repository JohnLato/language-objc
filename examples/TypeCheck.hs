module Main where

import Language.C
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.TravMonad
import Language.C.System.GCC
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data Flag
  = Language CLanguage
  | Include String
  | Define String
  | BadLang String

isLangOpt :: Flag -> Bool
isLangOpt (Language _) = True
isLangOpt (BadLang _) = True
isLangOpt _ = False

isCPPOpt :: Flag -> Bool
isCPPOpt (Include _) = True
isCPPOpt (Define _) = True
isCPPOpt _ = False

options :: [OptDescr Flag]
options = [ Option ['l'] ["lang"]    (ReqArg parseLang "LANG") "set C language variant"
          , Option ['I'] ["include"] (ReqArg Include "DIR")    "add include file directory"
          , Option ['D'] ["define"]  (ReqArg Define "SYM")     "define CPP symbol"
          ]

parseLang :: String -> Flag
parseLang "c89" = Language C89
parseLang "C89" = Language C89
parseLang "c99" = Language C99
parseLang "C99" = Language C99
parseLang "gnu89" = Language GNU89
parseLang "GNU89" = Language GNU89
parseLang "gnu99" = Language GNU99
parseLang "GNU99" = Language GNU99
parseLang s = BadLang s

splitOptions :: [String] -> IO ([Flag], [FilePath])
splitOptions args =
  case getOpt Permute options args of
    (flags, files, []) -> return (flags, files)
    (_, _, errs)       -> hPutStrLn stderr (concat errs) >> exitFailure

processFile :: CLanguage -> [Flag] -> FilePath -> IO ()
processFile lang cppOpts file =
  do hPutStr stderr $ file ++ ": "
     result <- parseCFile (newGCC "gcc") Nothing (map convertCPPOpt cppOpts) file
     case result of
       Left err -> hPutStrLn stderr ('\n' : show err)
       Right tu -> case runTrav_ (body tu) of
                     Left errs -> hPutStrLn stderr ('\n' : concatMap show errs)
                     Right _   -> hPutStrLn stderr "success"
  where body tu = do modifyOptions (\opts -> opts { language = lang })
                     analyseAST tu
        convertCPPOpt (Include dir) = '-':'I':dir
        convertCPPOpt (Define sym) = '-':'D':sym
        convertCPPOpt _            = error "not CPP option"

main :: IO ()
main =
  do (flags, files) <- splitOptions =<< getArgs
     let cppOpts = filter isCPPOpt flags
     case filter isLangOpt flags of
       [] -> processFiles GNU99 cppOpts files
       [BadLang arg] -> hPutStrLn stderr $ "unknown language: " ++ arg
       [Language lang] -> processFiles lang cppOpts files
       _ -> do hPutStrLn stderr "more than one language specified"
               exitFailure
  where processFiles _ _ [] = hPutStrLn stderr "no files given"
        processFiles lang cppOpts files = mapM_ (processFile lang cppOpts) files
