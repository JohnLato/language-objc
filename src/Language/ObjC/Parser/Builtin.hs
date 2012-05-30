-- |
-- Module      :  Language.ObjC.Parser.Builtin
-- Copyright   :  (c) 2001 Manuel M. T. Chakravarty
-- License     :  BSD-style
-- Maintainer  :  jwlato@gmail.com
-- Portability :  portable
--
-- This module provides information about builtin entities.
--
--  Currently, only builtin type names are supported.  The only builtin type
--  name is `__builtin_va_list', which is a builtin of GNU C.
--
module Language.ObjC.Parser.Builtin (
  builtinTypeNames
) where
import Language.ObjC.Data.Ident (Ident, builtinIdent)

-- predefined type names
--
builtinTypeNames :: [Ident]
builtinTypeNames  = [builtinIdent "__builtin_va_list"]
