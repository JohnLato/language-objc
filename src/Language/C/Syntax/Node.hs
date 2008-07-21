{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Syntax.Attributes
-- Copyright   :  (c) [1995..1999] Manuel M. T. Chakravarty
--                (c) 2008 Benedikt Huber (stripped radically)
-- License     :  BSD-style
-- Maintainer  :  -
-- Portability :  portable
--
-- source position and unqiue name
-----------------------------------------------------------------------------
module Language.C.Syntax.Node (
   NodeInfo(..), noNodeInfo,mkNodeInfoOnlyPos,mkNodeInfo,
   CNode(nodeInfo), eqByName, nodePos, nodeName,
) where
import Language.C.Syntax.Position   (Position, nopos)
import Language.C.Syntax.Error     (internalErr)
import Language.C.Syntax.Name     (Name)
import Data.Generics

-- | Parsed entity attribute
data NodeInfo = OnlyPos   Position           -- only pos (for internal stuff only)
              | NodeInfo Position Name      -- pos and unique name
           deriving (Show,Read,Data,Typeable)

-- name equality of attributes, used to define (name) equality of objects
instance Eq NodeInfo where
  (NodeInfo   _ id1) == (NodeInfo   _ id2) = id1 == id2
  _               == _               = 
    internalErr "Attributes: Attempt to compare `OnlyPos' attributes!"

-- attribute ordering
instance Ord NodeInfo where
  (NodeInfo   _ id1) <= (NodeInfo   _ id2) = id1 <= id2
  _               <= _               = 
    internalErr "Attributes: Attempt to compare `OnlyPos' attributes!"

-- | a class for convenient access to the attributes of an attributed object
class CNode a where
  nodeInfo :: a -> NodeInfo
nodePos :: NodeInfo -> Position
nodePos ni = case ni of (OnlyPos pos  ) -> pos; (NodeInfo   pos _) -> pos
nodeName :: NodeInfo -> Maybe Name
nodeName (OnlyPos _) = Nothing
nodeName (NodeInfo _ name) = Just name
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