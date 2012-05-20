{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables, RankNTypes #-}

-- | Functions to assist with writing abstract syntax manually.
module Language.ObjC.Syntax.Builders (
 -- * simple constructors
  idType
 ,idSpec
 -- * create a node with only internal information
 ,nonode
)

where

import Language.ObjC.Syntax
import Language.ObjC.Data.Ident
import Language.ObjC.Data.Node
import Language.ObjC.Data.Position
import Data.Foldable (foldMap)
import Data.Maybe
import Data.Monoid
import Control.Newtype

idType :: CDeclSpec
idType = idTypeG nonode

idSpec :: CTypeSpec
idSpec = idSpecG nonode

idTypeG :: a -> CDeclarationSpecifier a
idTypeG = CTypeSpec . idSpecG

-- formerly idQuals
idSpecG :: a -> CTypeSpecifier a
idSpecG = CTypeDef (internalIdent "id")

nonode :: NodeInfo
nonode = mkNodeInfoOnlyPos internalPos
