-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Syntax
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- Syntax of C files: The AST ('Language.C.Syntax.AST') and 
-- constants ('Language.C.Syntax.Constants').
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
