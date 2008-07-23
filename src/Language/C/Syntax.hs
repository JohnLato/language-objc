-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Syntax
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- Syntax of C files, including the AST, constants, operators, identifiers,
-- unqiue names, AST nodes and source locations.
-----------------------------------------------------------------------------
module Language.C.Syntax (
     -- * Constants
     module Language.C.Syntax.Constants,
     -- * Syntax tree
     module Language.C.Syntax.AST,
     -- * Identifiers
     SUERef(..),
     Ident,mkIdent, identToString, internalIdent, isInternalIdent, builtinIdent,
     -- * Unqiue names
     Name(..),namesStartingFrom,
     -- * AST nodes
     NodeInfo(..),CNode(..),
     fileOfNode,posOfNode,nameOfNode,
     mkUndefNodeInfo,mkNodeInfoOnlyPos,mkNodeInfo,
     -- * Source code positions
     Position(..),Pos(..),
     posFile,posRow,posColumn,
     nopos,builtinPos,internalPos,
     isSourcePos,isBuiltinPos,isInternalPos,
)
where
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Node
import Language.C.Syntax.Ops
import Language.C.Syntax.Position
