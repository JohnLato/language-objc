-- |
-- Module      :  Language.C.AST.Builtin
-- Copyright   :  (c) 2001 Manuel M. T. Chakravarty
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- This module provides information about builtin entities.
--
--  Currently, only builtin type names are supported.  The only builtin type
--  name is `__builtin_va_list', which is a builtin of GNU C.
--
module Language.C.AST.Builtin (
  builtinTypeNames
) where

import Language.C.Toolkit.Position (builtinPos)
import Language.C.Toolkit.Idents (Ident, onlyPosIdent)

import Language.C.AST.Attrs (CObj(BuiltinCO))


-- predefined type names
--
builtinTypeNames :: [Ident]
builtinTypeNames  = [onlyPosIdent builtinPos "__builtin_va_list"]
