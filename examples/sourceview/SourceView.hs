{-# LANGUAGE PatternSignatures, RankNTypes #-}
-- Example demonstrating how link the AST back to the source code,
-- using a simple heuristic
module Main where
import System.Environment
import System.Exit
import System.IO
import Control.Monad ()
import Control.Monad.Error as Err
import Data.List
import Text.PrettyPrint.HughesPJ
import Data.Tree
--import Debug.Trace

import Language.C              -- simple API

import GenericTree
import SourceBrowser

usageMsg :: String -> String
usageMsg prg = render $
  text "Usage:" <+> text prg <+> hsep (map text ["input_file.i"])
errorOnLeftM :: (MonadError e m, Err.Error e, Show a) => String -> m (Either a b) -> m b
errorOnLeftM msg action = either (throwError . strMsg . showWith) return =<< action
    where showWith s = msg ++ ": " ++ (show s)

main :: IO ()
main = do
    let usageErr = (hPutStrLn stderr (usageMsg "./Annotate") >> exitWith (ExitFailure 1))
    -- get command line arguments
    args <- getArgs
    c_file <- case args of
                [a1] -> return a1
                _    -> usageErr
    -- parse the file
    ast <- errorOnLeftM "Parse Error" (parseCFilePre c_file)
    -- show the generic tree
    putStrLn (drawTree . fmap show $ treeView c_file ast)
    -- run the source view
    runGTK (treeView c_file ast) c_file
