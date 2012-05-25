-----------------------------------------------------------------------------
-- |
-- Module      :  Language.ObjC.Syntax
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  : benedikt.huber@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Syntax of C files: The abstract syntax tree and constants.
-----------------------------------------------------------------------------
module Language.ObjC.Syntax (
     -- * Constants
     module Language.ObjC.Syntax.Constants,
     -- * Syntax tree
     module Language.ObjC.Syntax.AST,
     -- * Construction utility functions
     module Language.ObjC.Syntax.Builders
)
where
import Language.ObjC.Syntax.AST
import Language.ObjC.Syntax.Constants
import Language.ObjC.Syntax.Builders
