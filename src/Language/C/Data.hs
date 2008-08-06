-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Data
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  experimental
--
-- Data structures for Language.C: Identifiers, Source Code Positions,
-- Extensible Errors and Unique Names.
-----------------------------------------------------------------------------
module Language.C.Data (
     -- * Identifiers
     SUERef(..), isAnonymousType,
     Ident,mkIdent, identToString, internalIdent, isInternalIdent, builtinIdent,
     -- * Unqiue names
     Name(..),namesStartingFrom,
     -- * Source code positions
     Position(..),Pos(..),
     posFile,posRow,posColumn,
     nopos,builtinPos,internalPos,
     isSourcePos,isBuiltinPos,isInternalPos,
     -- * Syntax tree nodes
     NodeInfo(..),CNode(..),
     fileOfNode,posOfNode,nameOfNode,
     internalNode,mkNodeInfoOnlyPos,mkNodeInfo,
     -- * extensible errors
     module Language.C.Data.Error
)
where
import Language.C.Data.Ident
import Language.C.Data.Name
import Language.C.Data.Position
import Language.C.Data.Error
import Language.C.Data.Node
