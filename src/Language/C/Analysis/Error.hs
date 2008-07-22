-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis.Error
-- Copyright   :  (c) [1995..2000] Manuel M. T. Chakravarty
--                    2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- Typed analysis errors.
--
-- Additionally, this modules exports some datatypes and auxilliary routines
-----------------------------------------------------------------------------
module Language.C.Analysis.Error (
  -- * error levels
  ErrorLevel(..), isWarning,isHardError,
  -- * errors in the parsed program
  CError(..), mkWarning, mkError, 
  -- * helpers  
  showError,  
  internalErr, todo, -- deprecated
) where

import Language.C.Syntax.Position (Position(..), isInternalPos)


-- errors in the translation program
-- ------------------------------

-- | Error levels (priorities)
data ErrorLevel = LevelWarn
                | LevelError
                | LevelFatal
              deriving (Eq, Ord)

-- | create a warning
mkWarning :: Position -> String -> CError
mkWarning pos msg = mkError LevelWarn pos (lines msg)

-- | create a `Error' with the given level, position and error lines
mkError :: ErrorLevel -> Position -> [String] -> CError
mkError  = CError

data CError = CError { errorLevel :: ErrorLevel, 
                       errorPos   :: Position,
                       errorMsgs :: [String] }

instance Eq CError where
  (CError lvl1 pos1 _) == (CError lvl2 pos2 _) = lvl1 == lvl2 && pos1 == pos2  

instance Ord CError where
  (<) (CError lvl1 pos1 _) (CError lvl2 pos2 _) = pos1 < pos2
                                              || (pos1 == pos2 && lvl1 < lvl2)
instance Show CError where
  show (CError lvl pos msgs) = showError lvl pos msgs

isWarning :: CError -> Bool
isWarning = ( <= LevelWarn) . errorLevel

isHardError :: CError -> Bool
isHardError = ( > LevelWarn) . errorLevel

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
showError :: ErrorLevel -> Position -> [String] -> String
showError _ pos (l:ls)  | isInternalPos pos =
  "INTERNAL ERROR!\n" 
  ++ "  >>> " ++ l ++ "\n"
  ++ (indentMultilineString 2 . unlines) ls  
showError lvl (Position fname row col) (l:ls)  =
  let
    prefix = fname ++ ":" ++ show (row::Int) ++ ": "
             ++ "(column " 
             ++ show (col::Int) 
             ++ ") [" 
             ++ showErrorLvl lvl
             ++ "] "
    showErrorLvl LevelWarn    = "WARNING"
    showErrorLvl LevelError   = "ERROR"
    showErrorLvl LevelFatal   = "FATAL"
  in
  prefix ++ "\n" 
  ++ "  >>> " ++ l ++ "\n"
  ++ (indentMultilineString 2 . unlines) ls
showError _ _ [] = internalErr "Errors: showError: Empty error message!"


-- * utilities

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

-- indent the given multiline text by the given number of spaces
--
indentMultilineString   :: Int -> String -> String
indentMultilineString n  = unlines . (map (spaces++)) . lines
                           where
                             spaces = take n (repeat ' ')
