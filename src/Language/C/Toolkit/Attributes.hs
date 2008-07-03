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
-----------------------------------------------------------------------------
module Language.C.Toolkit.Attributes (
   NodeInfo(..), noNodeInfo,mkNodeInfoOnlyPos,mkNodeInfo,
   CNode(nodeInfo), eqByName,  
) where

import Language.C.Toolkit.Position   (Position, Pos(posOf), nopos)
import Language.C.Toolkit.Errors     (interr)
import Language.C.Toolkit.Names      (Name)

-- | Parsed entity attribute
data NodeInfo = OnlyPos   Position           -- only pos (for internal stuff only)
              | NodeInfo Position Name      -- pos and unique name
           deriving (Show)

-- get the position associated with an attribute
instance Pos NodeInfo where
  posOf (OnlyPos pos  ) = pos
  posOf (NodeInfo   pos _) = pos

-- name equality of attributes, used to define (name) equality of objects
instance Eq NodeInfo where
  (NodeInfo   _ id1) == (NodeInfo   _ id2) = id1 == id2
  _               == _               = 
    interr "Attributes: Attempt to compare `OnlyPos' attributes!"

-- attribute ordering
instance Ord NodeInfo where
  (NodeInfo   _ id1) <= (NodeInfo   _ id2) = id1 <= id2
  _               <= _               = 
    interr "Attributes: Attempt to compare `OnlyPos' attributes!"

-- | a class for convenient access to the attributes of an attributed object
class CNode a where
  nodeInfo :: a -> NodeInfo

-- | equality by name
eqByName           :: CNode a => a -> a -> Bool
eqByName obj1 obj2  = (nodeInfo obj1) == (nodeInfo obj2)


-- attribute identifier creation
-- -----------------------------

noNodeInfo :: NodeInfo
noNodeInfo = OnlyPos nopos

-- | Given only a source position, create a new attribute identifier
mkNodeInfoOnlyPos :: Position -> NodeInfo
mkNodeInfoOnlyPos pos  = OnlyPos pos

-- | Given a source position and a unique name, create a new attribute
-- identifier
mkNodeInfo :: Position -> Name -> NodeInfo
mkNodeInfo pos name  = NodeInfo pos name