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
)
where
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Syntax.Ops
