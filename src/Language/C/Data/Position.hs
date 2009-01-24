{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Data.Position
-- Copyright   :  (c) [1995..2000] Manuel M. T. Chakravarty
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  experimental
-- Portability :  ghc
--
-- Source code position
-----------------------------------------------------------------------------
module Language.C.Data.Position (
  --
  -- source text positions
  --
  Position(Position),PosLength,
  posFile,posRow,posColumn,posOffset,
  initPos, isSourcePos,
  nopos, isNoPos,
  builtinPos, isBuiltinPos,
  internalPos, isInternalPos,
  incPos, retPos, retPosOffset,
  Pos(..),
) where
import Data.Generics

-- | uniform representation of source file positions 
data Position = Position 
        String                          -- file name
        {-# UNPACK #-}   !Int           -- row
        {-# UNPACK #-}   !Int           -- column or absolute offset
  deriving (Eq, Ord, Typeable, Data)

type PosLength = (Position,Int)

instance Show Position where
  show pos@(Position fname row _)
    | isNoPos pos = "<no file>"
    | isBuiltinPos pos = "<builtin>"
    | isInternalPos pos = "<internal>"
    | otherwise = "(" ++ show fname ++ ": line " ++ show row ++ ")"
    
-- | get the source file of the specified position. Fails unless @isSourcePos pos@.
posFile :: Position -> String
posFile (Position fname _ _) = fname

-- | get the line number of the specified position. Fails unless @isSourcePos pos@
posRow  :: Position -> Int
posRow (Position _ row col) = row

-- | get the column of the specified position.
-- Meaningless on preprocessed sources (because of #LINE pramas).
posColumn :: Position -> Int
posColumn (Position _ _ col) = (col+1)

-- | get the absolute offset of the position (in the preprocessed source)
posOffset :: Position -> Int
posOffset (Position _ _ off) = off

-- | class of type which aggregate a source code location
class Pos a where
    posOf :: a -> Position

-- | initialize a Position to the start the file (line = 1 and column = 1 or offset = 0)
initPos :: FilePath -> Position
initPos file = Position file 1 0

-- |
-- | returns @True@ if the given position refers to an actual source file
isSourcePos :: Position -> Bool
isSourcePos (Position _ row col) = row >= 0

-- | no position (for unknown position information)
nopos :: Position
nopos  = Position "<no file>" (-1) (-1)

isNoPos :: Position -> Bool
isNoPos (Position _ (-1) _) = True
isNoPos _                     = False

-- | position attached to built-in objects
--
builtinPos :: Position
builtinPos  = Position "<built into the parser>" (-2) (-2) 

-- | returns @True@ if the given position refers to a builtin definition
isBuiltinPos :: Position -> Bool
isBuiltinPos (Position _ (-2) _) = True
isBuiltinPos _                   = False

-- | position used for internal errors
internalPos :: Position
internalPos = Position "<internal error>" (-3) (-3)

-- | returns @True@ if the given position is internal
isInternalPos :: Position -> Bool
isInternalPos (Position _ (-3) _) = True
isInternalPos _                   = False

{-# INLINE incPos #-}
-- | advance column
incPos :: Position -> Int -> Position
incPos (Position fname row offsOrCol) n = Position fname row (offsOrCol+n)

{-# INLINE retPos #-}

-- | advance to next line  (for column oriented view)
retPos :: Position -> Position
retPos (Position fname row _) = Position fname (row + 1) 1

-- | advance to next line  (for preprocessed sources)
retPosOffset :: Position -> Position
retPosOffset (Position fname row offs) = Position fname (row + 1) (offs + 1) 
