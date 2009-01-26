{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Data.Position
-- Copyright   :  (c) [1995..2000] Manuel M. T. Chakravarty
--                    [2008..2009] Benedikt Huber
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
  incPos, retPos, adjustPos,
  Pos(..),
) where
import Data.Generics

-- | uniform representation of source file positions
data Position = Position
        {-# UNPACK #-}   !Int           -- offset/id
        String                          -- file name
        {-# UNPACK #-}   !Int           -- row
        {-# UNPACK #-}   !Int           -- column (remove ?)
  deriving (Eq, Ord, Typeable, Data)

type PosLength = (Position,Int)

instance Show Position where
  show pos@(Position _ fname row _)
    | isNoPos pos = "<no file>"
    | isBuiltinPos pos = "<builtin>"
    | isInternalPos pos = "<internal>"
    | otherwise = "(" ++ show fname ++ ": line " ++ show row ++ ")"

-- | get the source file of the specified position. Fails unless @isSourcePos pos@.
posFile :: Position -> String
posFile (Position _ fname _ col) = fname

-- | get the line number of the specified position. Fails unless @isSourcePos pos@
posRow  :: Position -> Int
posRow (Position _ _ row col) = row

{-# DEPRECATED posColumn "column number information is inaccurate in presence of macros - do not rely on it." #-}
-- | Get the column of the specified position.
--
-- Note that this is the column in the preprocessed source, so this information might be inaccurate w.r.t. to
-- the original source.
posColumn :: Position -> Int
posColumn (Position _ _ _ col) = col

-- | get the absolute offset of the position (in the preprocessed source)
posOffset :: Position -> Int
posOffset (Position off _ _ col) = off

-- | class of type which aggregate a source code location
class Pos a where
    posOf :: a -> Position

-- | initialize a Position to the start of the translation unit starting in the given file
initPos :: FilePath -> Position
initPos file = Position 0 file 1 1

-- | returns @True@ if the given position refers to an actual source file
isSourcePos :: Position -> Bool
isSourcePos (Position _ _ row col) = row >= 0

-- | no position (for unknown position information)
nopos :: Position
nopos  = Position (-1) "<no file>" (-1) 0

-- | returns @True@ if the there is no position information available
isNoPos :: Position -> Bool
isNoPos (Position _ _ (-1) _) = True
isNoPos _                     = False

-- | position attached to built-in objects
--
builtinPos :: Position
builtinPos  = Position (-2) "<built into the parser>" (-2) 0

-- | returns @True@ if the given position refers to a builtin definition
isBuiltinPos :: Position -> Bool
isBuiltinPos (Position _ _ (-2) _) = True
isBuiltinPos _                   = False

-- | position used for internal errors
internalPos :: Position
internalPos = Position (-3) "<internal error>" (-3) 0

-- | returns @True@ if the given position is internal
isInternalPos :: Position -> Bool
isInternalPos (Position _ _ (-3) _) = True
isInternalPos _                   = False

{-# INLINE incPos #-}
-- | advance column
incPos :: Position -> Int -> Position
incPos (Position offs fname row col) n = Position (offs+n) fname row (col+n)

{-# INLINE retPos #-}
-- | advance to next line
retPos :: Position -> Position
retPos (Position offs fname row _) = Position (offs+1) fname (row + 1) 1

{-# INLINE adjustPos #-}
-- | adjust position: change file and line number, reseting column to 1. This is usually
--   used for #LINE pragmas. The absolute offset is not changed - this can be done
--   by @adjustPos newFile line . incPos (length pragma)@.
adjustPos :: FilePath -> Int -> Position -> Position
adjustPos fname row (Position offs _ _ _) = Position offs fname row 1
