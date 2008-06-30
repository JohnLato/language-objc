{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Toolkit.Attributes
-- Copyright   :  (c) [1995..1999] Manuel M. T. Chakravarty
--                (c) 2008 Benedikt Huber (stripped radically)
-- License     :  BSD-style
-- Maintainer  :  -
-- Portability :  portable
--
module Language.C.Toolkit.Attributes (
   Attrs(..), newAttrsOnlyPos, newAttrs,
   Attributed(attrsOf), eqOfAttrsOf, posOfAttrsOf,
) where

import Language.C.Toolkit.Position   (Position, Pos(posOf))
import Language.C.Toolkit.Errors     (interr)
import Language.C.Toolkit.Names      (Name)

-- | Parsed entity attribute
data Attrs = OnlyPos Position           -- only pos (for internal stuff only)
           | Attrs   Position Name      -- pos and unique name
           deriving (Show)

-- get the position associated with an attribute
instance Pos Attrs where
  posOf (OnlyPos pos  ) = pos
  posOf (Attrs   pos _) = pos

-- name equality of attributes, used to define (name) equality of objects
instance Eq Attrs where
  (Attrs   _ id1) == (Attrs   _ id2) = id1 == id2
  _               == _               = 
    interr "Attributes: Attempt to compare `OnlyPos' attributes!"

-- attribute ordering
instance Ord Attrs where
  (Attrs   _ id1) <= (Attrs   _ id2) = id1 <= id2
  _               <= _               = 
    interr "Attributes: Attempt to compare `OnlyPos' attributes!"

-- | a class for convenient access to the attributes of an attributed object
class Attributed a where
  attrsOf :: a -> Attrs

-- | equality by name
eqOfAttrsOf           :: Attributed a => a -> a -> Bool
eqOfAttrsOf obj1 obj2  = (attrsOf obj1) == (attrsOf obj2)

-- | position of an attributed object
posOfAttrsOf :: Attributed a => a -> Position
posOfAttrsOf  = posOf . attrsOf


-- attribute identifier creation
-- -----------------------------

-- | Given only a source position, create a new attribute identifier
newAttrsOnlyPos     :: Position -> Attrs
newAttrsOnlyPos pos  = OnlyPos pos

-- | Given a source position and a unique name, create a new attribute
-- identifier
newAttrs          :: Position -> Name -> Attrs
newAttrs pos name  = Attrs pos name