{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables, RankNTypes #-}

-- | Functions to assist with writing abstract syntax manually.
module Language.ObjC.Syntax.Builders (
 -- * simple constructors
  idType
 ,idTypeSpec
 ,idSpec
 ,protoType
 -- * create a node with only internal information
 ,nonode
)

where

import Language.ObjC.Syntax.AST
import Language.ObjC.Data.Ident
import Language.ObjC.Data.Node
import Language.ObjC.Data.Position
import Data.Foldable (foldMap)
import Data.Maybe
import Data.Monoid
import Control.Newtype

idType :: CDecl
idType = CDecl [idTypeSpec] [] nonode

idTypeSpec :: CDeclSpec
idTypeSpec = idTypeG nonode

idSpec :: CTypeSpec
idSpec = idSpecG nonode

idTypeG :: a -> CDeclarationSpecifier a
idTypeG = CTypeSpec . idSpecG

-- | Create a type name of @id <protoname>@
protoType :: Ident -> CDecl
protoType pNm = CDecl [CTypeSpec (ObjCTypeProto (internalIdent "id")
                        [ObjCProtoNm pNm nonode] nonode)] [] nonode

-- formerly idQuals
idSpecG :: a -> CTypeSpecifier a
idSpecG = CTypeDef (internalIdent "id")

nonode :: NodeInfo
nonode = mkNodeInfoOnlyPos internalPos
