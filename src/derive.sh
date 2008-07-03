#!/bin/sh
cat > Language/C/AST/Generic.hs  <<EOF
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.AST.Generic
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
-- Stability   :  experimental
--
-- SYB for the AST
-----------------------------------------------------------------------------
module Language.C.AST.Generic where
import Language.C.Toolkit.Idents
import Language.C.Toolkit.Attributes
import Language.C.Toolkit.Names
import Language.C.Toolkit.Position
import Language.C.AST.AST
import Language.C.AST.Constants
import Data.Generics
EOF
derive -d Data,Typeable Language/C/Toolkit/Attributes.hs  Language/C/Toolkit/Names.hs Language/C/Toolkit/Idents.hs Language/C/Toolkit/Position.hs Language/C/AST/Constants.hs Language/C/AST/AST.hs  >> Language/C/AST/Generic.hs 