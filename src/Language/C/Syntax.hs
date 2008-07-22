-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Syntax
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
-- Stability   :  provisional
--
-- Syntax of C files, including the AST, constants, operators, identifiers,
-- unqiue names, AST nodes and source locations.
-----------------------------------------------------------------------------
module Language.C.Syntax (
     module Language.C.Syntax.Constants,
     module Language.C.Syntax.AST,
     -- * identifiers
     SUERef(..),
     Ident,identToString,internalIdent,
     -- * unqiue names
     Name(..),
     -- * AST nodes
     NodeInfo(..),CNode(..),fileOfNode,nodePos,
     -- * source code positions
     Position(..),Pos(..),posFile,posRow,posColumn,
)
where
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Syntax.Ident
import Language.C.Syntax.Name
import Language.C.Syntax.Node
import Language.C.Syntax.Ops
import Language.C.Syntax.Position
