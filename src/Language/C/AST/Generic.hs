{-# LANGUAGE CPP,StandaloneDeriving, DeriveDataTypeable  #-}
{-# OPTIONS -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  needs (StandaloneDeriving, DeriveDataTypeable)
-- Stability   :  alpha
--
-- SYB for the AST
--
-- TODO: Standalone Deriving Data is buggy cross module borders in ghc.
-- Switch to offline - deriving - this also has the advantage of supplying
-- Uniplate, Data.Generic etc. instances
-----------------------------------------------------------------------------
module Language.C.AST.Generic where
import Language.C.Toolkit.Idents
import Language.C.Toolkit.Attributes
import Language.C.Toolkit.Names
import Language.C.Toolkit.Position
import Language.C.AST.AST
import Language.C.AST.Constants
import Data.Generics
#ifndef __HADDOCK__
deriving instance Typeable Name
deriving instance Data Name
deriving instance Typeable Ident
deriving instance Data Ident

deriving instance Typeable Attrs
deriving instance Data Attrs
deriving instance Typeable Position
deriving instance Data Position
deriving instance Typeable CHeader
deriving instance Typeable CExtDecl
deriving instance Typeable CFunDef
deriving instance Typeable CStat
deriving instance Typeable CBlockItem
deriving instance Typeable CDecl
deriving instance Typeable CDeclSpec
deriving instance Typeable CStorageSpec
deriving instance Typeable CTypeSpec
deriving instance Typeable CTypeQual
deriving instance Typeable CStructUnion
deriving instance Typeable CStructTag
deriving instance Typeable CEnum
deriving instance Typeable CDeclr
deriving instance Typeable CInit
deriving instance Typeable CDesignator
deriving instance Typeable CExpr
deriving instance Typeable CAssignOp
deriving instance Typeable CBinaryOp
deriving instance Typeable CUnaryOp
deriving instance Typeable CConst
deriving instance Typeable CStrLit
deriving instance Typeable CAsmStmt
deriving instance Typeable CAsmOperand
deriving instance Typeable CAttr

deriving instance Typeable CBuiltin
deriving instance Typeable CChar
deriving instance Typeable CFloat
deriving instance Typeable CInteger
deriving instance Typeable CIntFlag
deriving instance Typeable CString
deriving instance Typeable1 Flags
--
deriving instance Data CHeader
deriving instance Data CExtDecl
deriving instance Data CFunDef
deriving instance Data CStat
deriving instance Data CBlockItem
deriving instance Data CDecl
deriving instance Data CDeclSpec
deriving instance Data CStorageSpec
deriving instance Data CTypeSpec
deriving instance Data CTypeQual
deriving instance Data CStructUnion
deriving instance Data CStructTag
deriving instance Data CEnum
deriving instance Data CDeclr
deriving instance Data CInit
deriving instance Data CDesignator
deriving instance Data CExpr
deriving instance Data CAssignOp
deriving instance Data CBinaryOp
deriving instance Data CUnaryOp
deriving instance Data CConst
deriving instance Data CStrLit
deriving instance Data CAsmStmt
deriving instance Data CAsmOperand
deriving instance Data CAttr
deriving instance Data CBuiltin

deriving instance Data CChar
deriving instance Data CFloat
deriving instance Data CInteger
deriving instance Data CString
deriving instance Data CIntFlag
deriving instance (Data a) => Data (Flags a)
#endif HADDOCK
