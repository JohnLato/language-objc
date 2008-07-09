{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Common.Position
-- Copyright   :  (c) [1995..2000] Manuel M. T. Chakravarty
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- Source code position
-----------------------------------------------------------------------------
module Language.C.Common.Position (
  --
  -- source text positions
  --
  Position(Position),
  posFile,posRow,posColumn,isSourcePos,
  nopos, isNopos,
  builtinPos, isBuiltinPos,
  internalPos, isInternalPos,
  incPos, tabPos, retPos,
  Pos(..),
) where
import Data.Generics

-- | uniform representation of source file positions; the order of the arguments
-- is important as it leads to the desired ordering of source positions
data Position = Position String         -- file name
        {-# UNPACK #-}   !Int           -- row
        {-# UNPACK #-}   !Int           -- column
  deriving (Eq, Ord, Typeable, Data)
    
instance Show Position where
  show pos@(Position fname row col) 
    | isNopos pos = "<no file>"
    | isBuiltinPos pos = "<builtin>"
    | isInternalPos pos = "<internal>"
    | otherwise = show (fname, row, col)

-- | get the source file of the specified position. Fails unless @isSourcePos pos@.
posFile :: Position -> String
posFile (Position fname _ _) = fname
-- | get the line number of the specified position. Fails if @isSourcePos pos@
posRow  :: Position -> Int
posRow (Position _ row _) = row
-- | get the column of the specified position. Fails if @isSourcePos pos@
posColumn :: Position -> Int
posColumn (Position _ _ col) = col

class Pos a where
    posOf :: a -> Position
      
isSourcePos :: Position -> Bool
isSourcePos (Position _ row col) = row >= 0 && col >= 0      

-- | no position (for unknown position information) 
nopos :: Position
nopos  = Position "<no file>" (-1) 0

isNopos :: Position -> Bool
isNopos (Position _ (-1) 0) = True
isNopos _                   = False

-- | position attached to built-in objects
--
builtinPos :: Position
builtinPos  = Position "<built into the parser>" (-1) 1

isBuiltinPos :: Position -> Bool
isBuiltinPos (Position _ (-1) 1) = True
isBuiltinPos _                   = False

-- | position used for internal errors
internalPos :: Position
internalPos = Position "<internal error>" (-1) 2

isInternalPos :: Position -> Bool
isInternalPos (Position _ (-1) 2) = True
isInternalPos _                      = False

-- | advance column
incPos :: Position -> Int -> Position
incPos (Position fname row col) n = Position fname row (col + n)

-- | advance column to next tab positions (tabs are considered to be at every 8th column)
tabPos :: Position -> Position
tabPos (Position fname row col) =
        Position fname row (col + 8 - (col - 1) `mod` 8)

-- | advance to next line
retPos :: Position -> Position
retPos (Position fname row _col) = Position fname (row + 1) 1
