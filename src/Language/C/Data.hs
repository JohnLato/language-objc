-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Data
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  provisional
-- Portability :  DeriveDataTypable
--
-- Common data types for Language.C: Identifiers, unique names, source code locations,
-- ast node attributes and extensible errors.
-----------------------------------------------------------------------------
module Language.C.Data (
     -- * Input stream
     module Language.C.Data.InputStream,
     -- * Identifiers
     SUERef(..), isAnonymousRef,
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
     -- * Extensible errors
     module Language.C.Data.Error
)
where
import Language.C.Data.InputStream
import Language.C.Data.Ident
import Language.C.Data.Name
import Language.C.Data.Position
import Language.C.Data.Error
import Language.C.Data.Node
