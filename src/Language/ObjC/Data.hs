-----------------------------------------------------------------------------
-- |
-- Module      :  Language.ObjC.Data
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  : benedikt.huber@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Common data types for Language.ObjC: Identifiers, unique names, source code locations,
-- ast node attributes and extensible errors.
-----------------------------------------------------------------------------
module Language.ObjC.Data (
     -- * Input stream
     module Language.ObjC.Data.InputStream,
     -- * Identifiers
     SUERef(..), isAnonymousRef,
     Ident,mkIdent, identToString, internalIdent, isInternalIdent, builtinIdent,
     -- * Unqiue names
     Name(..),newNameSupply,
     -- * Source code positions
     Position(..),Pos(..),
     initPos, nopos,builtinPos,internalPos,
     posFile, posRow, posColumn,
     isSourcePos,isBuiltinPos,isInternalPos,
     -- * Syntax tree nodes
     NodeInfo(..),CNode(..),
     fileOfNode,posOfNode,nameOfNode,
     undefNode,mkNodeInfoOnlyPos,mkNodeInfo,
     internalNode, -- DEPRECATED
     -- * Extensible errors
     module Language.ObjC.Data.Error
)
where
import Language.ObjC.Data.InputStream
import Language.ObjC.Data.Ident
import Language.ObjC.Data.Name
import Language.ObjC.Data.Position
import Language.ObjC.Data.Error
import Language.ObjC.Data.Node
