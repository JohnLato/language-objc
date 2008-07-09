{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Common.Error
-- Copyright   :  (c) [1995..2000] Manuel M. T. Chakravarty
--                    2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- This modules exports some datatypes and auxilliary routines for error handling.
module Language.C.Common.Error (
  -- * handling of internal errors
  internalErr, todo,
  -- * errors in the parsed program
  ErrorLevel(..), Error(..), makeError, showError
) where

import Language.C.Common.Position (Position(..), isInternalPos)


-- internal errors
-- ---------------
internalErrPrefix :: String
internalErrPrefix = unlines [ "Language.C : Internal Error" ,
                              "This is propably a bug, and should be reported at "++     
                              "http://www.sivity.net/projects/language.c/newticket"]

-- | raise a fatal internal error; message may have multiple lines
internalErr     :: String -> a
internalErr msg  = error (internalErrPrefix ++ "\n"
                       ++ indentMultilineString 2 msg 
                       ++ "\n")

-- | raise a error due to a implementation restriction; message may have multiple
-- lines
todo     :: String -> a
todo msg  = error ("Feature not yet implemented:\n"
                   ++ indentMultilineString 2 msg 
                   ++ "\n")


-- errors in the translation program
-- ------------------------------

-- | Error levels (priorities)
data ErrorLevel = LevelWarning
                | LevelError
                | LevelFatal
              deriving (Eq, Ord)

-- | create a `Error' with the given level, position and error lines
makeError :: ErrorLevel -> Position -> [String] -> Error
makeError  = Error

data Error = Error { errorLevel :: ErrorLevel, 
                       errorPos   :: Position,
                       errorLines :: [String] }

instance Eq Error where
  (Error lvl1 pos1 _) == (Error lvl2 pos2 _) = lvl1 == lvl2 && pos1 == pos2
  
instance Ord Error where
  (Error lvl1 pos1 _) <  (Error lvl2 pos2 _) = pos1 < pos2
                                               || (pos1 == pos2 && lvl1 < lvl2)
  e1                  <= e2                  = e1 < e2 || e1 == e2
instance Show Error where
      
-- | converts an error into a string using a fixed format
--
-- * the list of lines of the error message must not be empty
--
-- * the format is
--
-- >    <fname>:<row>: (column <col>) [<err lvl>] 
-- >      >>> <line_1>
-- >      <line_2>
-- >        ...
-- >      <line_n>
--
-- * internal errors are formatted as
--
-- >    INTERNAL ERROR!
-- >      >>> <line_1>
-- >      <line_2>
-- >        ...
-- >      <line_n>
--
showError :: Error -> String
showError (Error _   pos               (l:ls))  | isInternalPos pos =
  "INTERNAL ERROR!\n" 
  ++ "  >>> " ++ l ++ "\n"
  ++ (indentMultilineString 2 . unlines) ls  
showError (Error lvl (Position fname row col) (l:ls))  =
  let
    prefix = fname ++ ":" ++ show (row::Int) ++ ": "
             ++ "(column " 
             ++ show (col::Int) 
             ++ ") [" 
             ++ showErrorLvl lvl
             ++ "] "
    showErrorLvl LevelWarning = "WARNING"
    showErrorLvl LevelError   = "ERROR"
    showErrorLvl LevelFatal   = "FATAL"
  in
  prefix ++ "\n" 
  ++ "  >>> " ++ l ++ "\n"
  ++ (indentMultilineString 2 . unlines) ls
showError (Error _  _                  []   )   = internalErr "Errors: showError: Empty error message!"


-- indent the given multiline text by the given number of spaces
--
indentMultilineString   :: Int -> String -> String
indentMultilineString n  = unlines . (map (spaces++)) . lines
                           where
                             spaces = take n (repeat ' ')
