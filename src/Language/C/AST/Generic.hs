{-# OPTIONS -fno-warn-orphans  #-}
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
-- Therefore, we use Data.Derive at the moment. Unfortunately,
-- the derivation of Data isn't complete - gunofld, toConstr and dataTypeOf is missing.
-- I'll work on a patch soon.
-----------------------------------------------------------------------------
module Language.C.AST.Generic where
import Language.C.Toolkit.Idents
import Language.C.Toolkit.Attributes
import Language.C.Toolkit.Names
import Language.C.Toolkit.Position
import Language.C.AST.AST
import Language.C.AST.Constants
import Data.Generics


instance Data Attrs
    where gfoldl k r (OnlyPos x1) = k (r OnlyPos) x1
          gfoldl k r (Attrs x1 x2) = k (k (r Attrs) x1) x2
          gunfold k z c = case constrIndex c of
                              1 -> k (z OnlyPos)
                              2 -> k (k (z Attrs))
          toConstr (ctor@(OnlyPos x1)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(Attrs x1 x2)) = indexConstr (dataTypeOf ctor) 2
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "Attrs" [con_C1, con_C2]
                               con_C1 = mkConstr ty_T "OnlyPos" [] Prefix
                               con_C2 = mkConstr ty_T "Attrs" [] Prefix

typename_Attrs = mkTyCon "Attrs"
instance Typeable Attrs
    where typeOf _ = mkTyConApp typename_Attrs []

instance Data Name
    where gfoldl k r (Name x1) = k (r Name) x1
          gunfold k z c = case constrIndex c of
                              1 -> k (z Name)
          toConstr (ctor@(Name x1)) = indexConstr (dataTypeOf ctor) 1
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "Name" [con_C1]
                               con_C1 = mkConstr ty_T "Name" [] Prefix

typename_Name = mkTyCon "Name"
instance Typeable Name
    where typeOf _ = mkTyConApp typename_Name []

instance Data Ident
    where gfoldl k r (Ident x1 x2 x3) = k (k (k (r Ident) x1) x2) x3
          gunfold k z c = case constrIndex c of
                              1 -> k (k (k (z Ident)))
          toConstr (ctor@(Ident x1 x2 x3)) = indexConstr (dataTypeOf ctor) 1
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "Ident" [con_C1]
                               con_C1 = mkConstr ty_T "Ident" [] Prefix

typename_Ident = mkTyCon "Ident"
instance Typeable Ident
    where typeOf _ = mkTyConApp typename_Ident []

instance Data Position
    where gfoldl k r (Position x1
                               x2
                               x3) = k (k (k (r Position) x1) x2) x3
          gunfold k z c = case constrIndex c of
                              1 -> k (k (k (z Position)))
          toConstr (ctor@(Position x1
                                   x2
                                   x3)) = indexConstr (dataTypeOf ctor) 1
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "Position" [con_C1]
                               con_C1 = mkConstr ty_T "Position" [] Prefix

typename_Position = mkTyCon "Position"
instance Typeable Position
    where typeOf _ = mkTyConApp typename_Position []

instance Data CChar
    where gfoldl k r (CChar x1 x2) = k (k (r CChar) x1) x2
          gfoldl k r (CChars x1 x2) = k (k (r CChars) x1) x2
          gunfold k z c = case constrIndex c of
                              1 -> k (k (z CChar))
                              2 -> k (k (z CChars))
          toConstr (ctor@(CChar x1 x2)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(CChars x1 x2)) = indexConstr (dataTypeOf ctor) 2
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CChar" [con_C1, con_C2]
                               con_C1 = mkConstr ty_T "CChar" [] Prefix
                               con_C2 = mkConstr ty_T "CChars" [] Prefix

typename_CChar = mkTyCon "CChar"
instance Typeable CChar
    where typeOf _ = mkTyConApp typename_CChar []

instance Data CIntFlag
    where gfoldl k r (FlagUnsigned) = r FlagUnsigned
          gfoldl k r (FlagLong) = r FlagLong
          gfoldl k r (FlagLongLong) = r FlagLongLong
          gfoldl k r (FlagImag) = r FlagImag
          gunfold k z c = case constrIndex c of
                              1 -> z FlagUnsigned
                              2 -> z FlagLong
                              3 -> z FlagLongLong
                              4 -> z FlagImag
          toConstr (ctor@(FlagUnsigned)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(FlagLong)) = indexConstr (dataTypeOf ctor) 2
          toConstr (ctor@(FlagLongLong)) = indexConstr (dataTypeOf ctor) 3
          toConstr (ctor@(FlagImag)) = indexConstr (dataTypeOf ctor) 4
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CIntFlag" [con_C1, con_C2, con_C3, con_C4]
                               con_C1 = mkConstr ty_T "FlagUnsigned" [] Prefix
                               con_C2 = mkConstr ty_T "FlagLong" [] Prefix
                               con_C3 = mkConstr ty_T "FlagLongLong" [] Prefix
                               con_C4 = mkConstr ty_T "FlagImag" [] Prefix

typename_CIntFlag = mkTyCon "CIntFlag"
instance Typeable CIntFlag
    where typeOf _ = mkTyConApp typename_CIntFlag []

instance Data CInteger
    where gfoldl k r (CInteger x1 x2) = k (k (r CInteger) x1) x2
          gunfold k z c = case constrIndex c of
                              1 -> k (k (z CInteger))
          toConstr (ctor@(CInteger x1 x2)) = indexConstr (dataTypeOf ctor) 1
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CInteger" [con_C1]
                               con_C1 = mkConstr ty_T "CInteger" [] Prefix

typename_CInteger = mkTyCon "CInteger"
instance Typeable CInteger
    where typeOf _ = mkTyConApp typename_CInteger []

instance Data CFloat
    where gfoldl k r (CFloat x1) = k (r CFloat) x1
          gunfold k z c = case constrIndex c of
                              1 -> k (z CFloat)
          toConstr (ctor@(CFloat x1)) = indexConstr (dataTypeOf ctor) 1
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CFloat" [con_C1]
                               con_C1 = mkConstr ty_T "CFloat" [] Prefix

typename_CFloat = mkTyCon "CFloat"
instance Typeable CFloat
    where typeOf _ = mkTyConApp typename_CFloat []

instance Data CString
    where gfoldl k r (CString x1 x2) = k (k (r CString) x1) x2
          gunfold k z c = case constrIndex c of
                              1 -> k (k (z CString))
          toConstr (ctor@(CString x1 x2)) = indexConstr (dataTypeOf ctor) 1
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CString" [con_C1]
                               con_C1 = mkConstr ty_T "CString" [] Prefix

typename_CString = mkTyCon "CString"
instance Typeable CString
    where typeOf _ = mkTyConApp typename_CString []

instance (Data t1, Typeable t1) => Data (Flags t1)
    where gfoldl k r (Flags x1) = k (r Flags) x1
          gunfold k z c = case constrIndex c of
                              1 -> k (z Flags)
          toConstr (ctor@(Flags x1)) = indexConstr (dataTypeOf ctor) 1
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "Flags" [con_C1]
                               con_C1 = mkConstr ty_T "Flags" [] Prefix

typename_Flags = mkTyCon "Flags"
instance Typeable1 Flags
    where typeOf1 _ = mkTyConApp typename_Flags []
instance Typeable a => Typeable (Flags a)
    where typeOf = typeOfDefault

instance Data CTranslUnit
    where gfoldl k r (CTranslUnit x1 x2) = k (k (r CTranslUnit) x1) x2
          gunfold k z c = case constrIndex c of
                              1 -> k (k (z CTranslUnit))
          toConstr (ctor@(CTranslUnit x1
                                      x2)) = indexConstr (dataTypeOf ctor) 1
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CTranslUnit" [con_C1]
                               con_C1 = mkConstr ty_T "CTranslUnit" [] Prefix

typename_CTranslUnit = mkTyCon "CTranslUnit"
instance Typeable CTranslUnit
    where typeOf _ = mkTyConApp typename_CTranslUnit []

instance Data CExtDecl
    where gfoldl k r (CDeclExt x1) = k (r CDeclExt) x1
          gfoldl k r (CFDefExt x1) = k (r CFDefExt) x1
          gfoldl k r (CAsmExt x1) = k (r CAsmExt) x1
          gunfold k z c = case constrIndex c of
                              1 -> k (z CDeclExt)
                              2 -> k (z CFDefExt)
                              3 -> k (z CAsmExt)
          toConstr (ctor@(CDeclExt x1)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(CFDefExt x1)) = indexConstr (dataTypeOf ctor) 2
          toConstr (ctor@(CAsmExt x1)) = indexConstr (dataTypeOf ctor) 3
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CExtDecl" [con_C1, con_C2, con_C3]
                               con_C1 = mkConstr ty_T "CDeclExt" [] Prefix
                               con_C2 = mkConstr ty_T "CFDefExt" [] Prefix
                               con_C3 = mkConstr ty_T "CAsmExt" [] Prefix

typename_CExtDecl = mkTyCon "CExtDecl"
instance Typeable CExtDecl
    where typeOf _ = mkTyConApp typename_CExtDecl []

instance Data CFunDef
    where gfoldl k r (CFunDef x1
                              x2
                              x3
                              x4
                              x5) = k (k (k (k (k (r CFunDef) x1) x2) x3) x4) x5
          gunfold k z c = case constrIndex c of
                              1 -> k (k (k (k (k (z CFunDef)))))
          toConstr (ctor@(CFunDef x1
                                  x2
                                  x3
                                  x4
                                  x5)) = indexConstr (dataTypeOf ctor) 1
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CFunDef" [con_C1]
                               con_C1 = mkConstr ty_T "CFunDef" [] Prefix

typename_CFunDef = mkTyCon "CFunDef"
instance Typeable CFunDef
    where typeOf _ = mkTyConApp typename_CFunDef []

instance Data CStat
    where gfoldl k r (CLabel x1
                             x2
                             x3
                             x4) = k (k (k (k (r CLabel) x1) x2) x3) x4
          gfoldl k r (CCase x1 x2 x3) = k (k (k (r CCase) x1) x2) x3
          gfoldl k r (CCases x1
                             x2
                             x3
                             x4) = k (k (k (k (r CCases) x1) x2) x3) x4
          gfoldl k r (CDefault x1 x2) = k (k (r CDefault) x1) x2
          gfoldl k r (CExpr x1 x2) = k (k (r CExpr) x1) x2
          gfoldl k r (CCompound x1 x2 x3) = k (k (k (r CCompound) x1) x2) x3
          gfoldl k r (CIf x1 x2 x3 x4) = k (k (k (k (r CIf) x1) x2) x3) x4
          gfoldl k r (CSwitch x1 x2 x3) = k (k (k (r CSwitch) x1) x2) x3
          gfoldl k r (CWhile x1
                             x2
                             x3
                             x4) = k (k (k (k (r CWhile) x1) x2) x3) x4
          gfoldl k r (CFor x1
                           x2
                           x3
                           x4
                           x5) = k (k (k (k (k (r CFor) x1) x2) x3) x4) x5
          gfoldl k r (CGoto x1 x2) = k (k (r CGoto) x1) x2
          gfoldl k r (CGotoPtr x1 x2) = k (k (r CGotoPtr) x1) x2
          gfoldl k r (CCont x1) = k (r CCont) x1
          gfoldl k r (CBreak x1) = k (r CBreak) x1
          gfoldl k r (CReturn x1 x2) = k (k (r CReturn) x1) x2
          gfoldl k r (CAsm x1 x2) = k (k (r CAsm) x1) x2
          gunfold k z c = case constrIndex c of
                              1 -> k (k (k (k (z CLabel))))
                              2 -> k (k (k (z CCase)))
                              3 -> k (k (k (k (z CCases))))
                              4 -> k (k (z CDefault))
                              5 -> k (k (z CExpr))
                              6 -> k (k (k (z CCompound)))
                              7 -> k (k (k (k (z CIf))))
                              8 -> k (k (k (z CSwitch)))
                              9 -> k (k (k (k (z CWhile))))
                              10 -> k (k (k (k (k (z CFor)))))
                              11 -> k (k (z CGoto))
                              12 -> k (k (z CGotoPtr))
                              13 -> k (z CCont)
                              14 -> k (z CBreak)
                              15 -> k (k (z CReturn))
                              16 -> k (k (z CAsm))
          toConstr (ctor@(CLabel x1
                                 x2
                                 x3
                                 x4)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(CCase x1 x2 x3)) = indexConstr (dataTypeOf ctor) 2
          toConstr (ctor@(CCases x1
                                 x2
                                 x3
                                 x4)) = indexConstr (dataTypeOf ctor) 3
          toConstr (ctor@(CDefault x1 x2)) = indexConstr (dataTypeOf ctor) 4
          toConstr (ctor@(CExpr x1 x2)) = indexConstr (dataTypeOf ctor) 5
          toConstr (ctor@(CCompound x1
                                    x2
                                    x3)) = indexConstr (dataTypeOf ctor) 6
          toConstr (ctor@(CIf x1 x2 x3 x4)) = indexConstr (dataTypeOf ctor) 7
          toConstr (ctor@(CSwitch x1
                                  x2
                                  x3)) = indexConstr (dataTypeOf ctor) 8
          toConstr (ctor@(CWhile x1
                                 x2
                                 x3
                                 x4)) = indexConstr (dataTypeOf ctor) 9
          toConstr (ctor@(CFor x1
                               x2
                               x3
                               x4
                               x5)) = indexConstr (dataTypeOf ctor) 10
          toConstr (ctor@(CGoto x1 x2)) = indexConstr (dataTypeOf ctor) 11
          toConstr (ctor@(CGotoPtr x1 x2)) = indexConstr (dataTypeOf ctor) 12
          toConstr (ctor@(CCont x1)) = indexConstr (dataTypeOf ctor) 13
          toConstr (ctor@(CBreak x1)) = indexConstr (dataTypeOf ctor) 14
          toConstr (ctor@(CReturn x1 x2)) = indexConstr (dataTypeOf ctor) 15
          toConstr (ctor@(CAsm x1 x2)) = indexConstr (dataTypeOf ctor) 16
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CStat" [con_C1,
                                                          con_C2,
                                                          con_C3,
                                                          con_C4,
                                                          con_C5,
                                                          con_C6,
                                                          con_C7,
                                                          con_C8,
                                                          con_C9,
                                                          con_C10,
                                                          con_C11,
                                                          con_C12,
                                                          con_C13,
                                                          con_C14,
                                                          con_C15,
                                                          con_C16]
                               con_C1 = mkConstr ty_T "CLabel" [] Prefix
                               con_C2 = mkConstr ty_T "CCase" [] Prefix
                               con_C3 = mkConstr ty_T "CCases" [] Prefix
                               con_C4 = mkConstr ty_T "CDefault" [] Prefix
                               con_C5 = mkConstr ty_T "CExpr" [] Prefix
                               con_C6 = mkConstr ty_T "CCompound" [] Prefix
                               con_C7 = mkConstr ty_T "CIf" [] Prefix
                               con_C8 = mkConstr ty_T "CSwitch" [] Prefix
                               con_C9 = mkConstr ty_T "CWhile" [] Prefix
                               con_C10 = mkConstr ty_T "CFor" [] Prefix
                               con_C11 = mkConstr ty_T "CGoto" [] Prefix
                               con_C12 = mkConstr ty_T "CGotoPtr" [] Prefix
                               con_C13 = mkConstr ty_T "CCont" [] Prefix
                               con_C14 = mkConstr ty_T "CBreak" [] Prefix
                               con_C15 = mkConstr ty_T "CReturn" [] Prefix
                               con_C16 = mkConstr ty_T "CAsm" [] Prefix

typename_CStat = mkTyCon "CStat"
instance Typeable CStat
    where typeOf _ = mkTyConApp typename_CStat []

instance Data CAsmStmt
    where gfoldl k r (CAsmStmt x1
                               x2
                               x3
                               x4
                               x5
                               x6) = k (k (k (k (k (k (r CAsmStmt) x1) x2) x3) x4) x5) x6
          gunfold k z c = case constrIndex c of
                              1 -> k (k (k (k (k (k (z CAsmStmt))))))
          toConstr (ctor@(CAsmStmt x1
                                   x2
                                   x3
                                   x4
                                   x5
                                   x6)) = indexConstr (dataTypeOf ctor) 1
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CAsmStmt" [con_C1]
                               con_C1 = mkConstr ty_T "CAsmStmt" [] Prefix

typename_CAsmStmt = mkTyCon "CAsmStmt"
instance Typeable CAsmStmt
    where typeOf _ = mkTyConApp typename_CAsmStmt []

instance Data CAsmOperand
    where gfoldl k r (CAsmOperand x1
                                  x2
                                  x3
                                  x4) = k (k (k (k (r CAsmOperand) x1) x2) x3) x4
          gunfold k z c = case constrIndex c of
                              1 -> k (k (k (k (z CAsmOperand))))
          toConstr (ctor@(CAsmOperand x1
                                      x2
                                      x3
                                      x4)) = indexConstr (dataTypeOf ctor) 1
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CAsmOperand" [con_C1]
                               con_C1 = mkConstr ty_T "CAsmOperand" [] Prefix

typename_CAsmOperand = mkTyCon "CAsmOperand"
instance Typeable CAsmOperand
    where typeOf _ = mkTyConApp typename_CAsmOperand []

instance Data CBlockItem
    where gfoldl k r (CBlockStmt x1) = k (r CBlockStmt) x1
          gfoldl k r (CBlockDecl x1) = k (r CBlockDecl) x1
          gfoldl k r (CNestedFunDef x1) = k (r CNestedFunDef) x1
          gunfold k z c = case constrIndex c of
                              1 -> k (z CBlockStmt)
                              2 -> k (z CBlockDecl)
                              3 -> k (z CNestedFunDef)
          toConstr (ctor@(CBlockStmt x1)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(CBlockDecl x1)) = indexConstr (dataTypeOf ctor) 2
          toConstr (ctor@(CNestedFunDef x1)) = indexConstr (dataTypeOf ctor) 3
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CBlockItem" [con_C1, con_C2, con_C3]
                               con_C1 = mkConstr ty_T "CBlockStmt" [] Prefix
                               con_C2 = mkConstr ty_T "CBlockDecl" [] Prefix
                               con_C3 = mkConstr ty_T "CNestedFunDef" [] Prefix

typename_CBlockItem = mkTyCon "CBlockItem"
instance Typeable CBlockItem
    where typeOf _ = mkTyConApp typename_CBlockItem []

instance Data CDecl
    where gfoldl k r (CDecl x1 x2 x3) = k (k (k (r CDecl) x1) x2) x3
          gunfold k z c = case constrIndex c of
                              1 -> k (k (k (z CDecl)))
          toConstr (ctor@(CDecl x1 x2 x3)) = indexConstr (dataTypeOf ctor) 1
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CDecl" [con_C1]
                               con_C1 = mkConstr ty_T "CDecl" [] Prefix

typename_CDecl = mkTyCon "CDecl"
instance Typeable CDecl
    where typeOf _ = mkTyConApp typename_CDecl []

instance Data CDeclSpec
    where gfoldl k r (CStorageSpec x1) = k (r CStorageSpec) x1
          gfoldl k r (CTypeSpec x1) = k (r CTypeSpec) x1
          gfoldl k r (CTypeQual x1) = k (r CTypeQual) x1
          gunfold k z c = case constrIndex c of
                              1 -> k (z CStorageSpec)
                              2 -> k (z CTypeSpec)
                              3 -> k (z CTypeQual)
          toConstr (ctor@(CStorageSpec x1)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(CTypeSpec x1)) = indexConstr (dataTypeOf ctor) 2
          toConstr (ctor@(CTypeQual x1)) = indexConstr (dataTypeOf ctor) 3
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CDeclSpec" [con_C1, con_C2, con_C3]
                               con_C1 = mkConstr ty_T "CStorageSpec" [] Prefix
                               con_C2 = mkConstr ty_T "CTypeSpec" [] Prefix
                               con_C3 = mkConstr ty_T "CTypeQual" [] Prefix

typename_CDeclSpec = mkTyCon "CDeclSpec"
instance Typeable CDeclSpec
    where typeOf _ = mkTyConApp typename_CDeclSpec []

instance Data CStorageSpec
    where gfoldl k r (CAuto x1) = k (r CAuto) x1
          gfoldl k r (CRegister x1) = k (r CRegister) x1
          gfoldl k r (CStatic x1) = k (r CStatic) x1
          gfoldl k r (CExtern x1) = k (r CExtern) x1
          gfoldl k r (CTypedef x1) = k (r CTypedef) x1
          gfoldl k r (CThread x1) = k (r CThread) x1
          gunfold k z c = case constrIndex c of
                              1 -> k (z CAuto)
                              2 -> k (z CRegister)
                              3 -> k (z CStatic)
                              4 -> k (z CExtern)
                              5 -> k (z CTypedef)
                              6 -> k (z CThread)
          toConstr (ctor@(CAuto x1)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(CRegister x1)) = indexConstr (dataTypeOf ctor) 2
          toConstr (ctor@(CStatic x1)) = indexConstr (dataTypeOf ctor) 3
          toConstr (ctor@(CExtern x1)) = indexConstr (dataTypeOf ctor) 4
          toConstr (ctor@(CTypedef x1)) = indexConstr (dataTypeOf ctor) 5
          toConstr (ctor@(CThread x1)) = indexConstr (dataTypeOf ctor) 6
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CStorageSpec" [con_C1,
                                                                 con_C2,
                                                                 con_C3,
                                                                 con_C4,
                                                                 con_C5,
                                                                 con_C6]
                               con_C1 = mkConstr ty_T "CAuto" [] Prefix
                               con_C2 = mkConstr ty_T "CRegister" [] Prefix
                               con_C3 = mkConstr ty_T "CStatic" [] Prefix
                               con_C4 = mkConstr ty_T "CExtern" [] Prefix
                               con_C5 = mkConstr ty_T "CTypedef" [] Prefix
                               con_C6 = mkConstr ty_T "CThread" [] Prefix

typename_CStorageSpec = mkTyCon "CStorageSpec"
instance Typeable CStorageSpec
    where typeOf _ = mkTyConApp typename_CStorageSpec []

instance Data CTypeSpec
    where gfoldl k r (CVoidType x1) = k (r CVoidType) x1
          gfoldl k r (CCharType x1) = k (r CCharType) x1
          gfoldl k r (CShortType x1) = k (r CShortType) x1
          gfoldl k r (CIntType x1) = k (r CIntType) x1
          gfoldl k r (CLongType x1) = k (r CLongType) x1
          gfoldl k r (CFloatType x1) = k (r CFloatType) x1
          gfoldl k r (CDoubleType x1) = k (r CDoubleType) x1
          gfoldl k r (CSignedType x1) = k (r CSignedType) x1
          gfoldl k r (CUnsigType x1) = k (r CUnsigType) x1
          gfoldl k r (CBoolType x1) = k (r CBoolType) x1
          gfoldl k r (CComplexType x1) = k (r CComplexType) x1
          gfoldl k r (CSUType x1 x2) = k (k (r CSUType) x1) x2
          gfoldl k r (CEnumType x1 x2) = k (k (r CEnumType) x1) x2
          gfoldl k r (CTypeDef x1 x2) = k (k (r CTypeDef) x1) x2
          gfoldl k r (CTypeOfExpr x1 x2) = k (k (r CTypeOfExpr) x1) x2
          gfoldl k r (CTypeOfType x1 x2) = k (k (r CTypeOfType) x1) x2
          gunfold k z c = case constrIndex c of
                              1 -> k (z CVoidType)
                              2 -> k (z CCharType)
                              3 -> k (z CShortType)
                              4 -> k (z CIntType)
                              5 -> k (z CLongType)
                              6 -> k (z CFloatType)
                              7 -> k (z CDoubleType)
                              8 -> k (z CSignedType)
                              9 -> k (z CUnsigType)
                              10 -> k (z CBoolType)
                              11 -> k (z CComplexType)
                              12 -> k (k (z CSUType))
                              13 -> k (k (z CEnumType))
                              14 -> k (k (z CTypeDef))
                              15 -> k (k (z CTypeOfExpr))
                              16 -> k (k (z CTypeOfType))
          toConstr (ctor@(CVoidType x1)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(CCharType x1)) = indexConstr (dataTypeOf ctor) 2
          toConstr (ctor@(CShortType x1)) = indexConstr (dataTypeOf ctor) 3
          toConstr (ctor@(CIntType x1)) = indexConstr (dataTypeOf ctor) 4
          toConstr (ctor@(CLongType x1)) = indexConstr (dataTypeOf ctor) 5
          toConstr (ctor@(CFloatType x1)) = indexConstr (dataTypeOf ctor) 6
          toConstr (ctor@(CDoubleType x1)) = indexConstr (dataTypeOf ctor) 7
          toConstr (ctor@(CSignedType x1)) = indexConstr (dataTypeOf ctor) 8
          toConstr (ctor@(CUnsigType x1)) = indexConstr (dataTypeOf ctor) 9
          toConstr (ctor@(CBoolType x1)) = indexConstr (dataTypeOf ctor) 10
          toConstr (ctor@(CComplexType x1)) = indexConstr (dataTypeOf ctor) 11
          toConstr (ctor@(CSUType x1 x2)) = indexConstr (dataTypeOf ctor) 12
          toConstr (ctor@(CEnumType x1
                                    x2)) = indexConstr (dataTypeOf ctor) 13
          toConstr (ctor@(CTypeDef x1 x2)) = indexConstr (dataTypeOf ctor) 14
          toConstr (ctor@(CTypeOfExpr x1
                                      x2)) = indexConstr (dataTypeOf ctor) 15
          toConstr (ctor@(CTypeOfType x1
                                      x2)) = indexConstr (dataTypeOf ctor) 16
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CTypeSpec" [con_C1,
                                                              con_C2,
                                                              con_C3,
                                                              con_C4,
                                                              con_C5,
                                                              con_C6,
                                                              con_C7,
                                                              con_C8,
                                                              con_C9,
                                                              con_C10,
                                                              con_C11,
                                                              con_C12,
                                                              con_C13,
                                                              con_C14,
                                                              con_C15,
                                                              con_C16]
                               con_C1 = mkConstr ty_T "CVoidType" [] Prefix
                               con_C2 = mkConstr ty_T "CCharType" [] Prefix
                               con_C3 = mkConstr ty_T "CShortType" [] Prefix
                               con_C4 = mkConstr ty_T "CIntType" [] Prefix
                               con_C5 = mkConstr ty_T "CLongType" [] Prefix
                               con_C6 = mkConstr ty_T "CFloatType" [] Prefix
                               con_C7 = mkConstr ty_T "CDoubleType" [] Prefix
                               con_C8 = mkConstr ty_T "CSignedType" [] Prefix
                               con_C9 = mkConstr ty_T "CUnsigType" [] Prefix
                               con_C10 = mkConstr ty_T "CBoolType" [] Prefix
                               con_C11 = mkConstr ty_T "CComplexType" [] Prefix
                               con_C12 = mkConstr ty_T "CSUType" [] Prefix
                               con_C13 = mkConstr ty_T "CEnumType" [] Prefix
                               con_C14 = mkConstr ty_T "CTypeDef" [] Prefix
                               con_C15 = mkConstr ty_T "CTypeOfExpr" [] Prefix
                               con_C16 = mkConstr ty_T "CTypeOfType" [] Prefix

typename_CTypeSpec = mkTyCon "CTypeSpec"
instance Typeable CTypeSpec
    where typeOf _ = mkTyConApp typename_CTypeSpec []

instance Data CTypeQual
    where gfoldl k r (CConstQual x1) = k (r CConstQual) x1
          gfoldl k r (CVolatQual x1) = k (r CVolatQual) x1
          gfoldl k r (CRestrQual x1) = k (r CRestrQual) x1
          gfoldl k r (CInlinQual x1) = k (r CInlinQual) x1
          gfoldl k r (CAttrQual x1) = k (r CAttrQual) x1
          gunfold k z c = case constrIndex c of
                              1 -> k (z CConstQual)
                              2 -> k (z CVolatQual)
                              3 -> k (z CRestrQual)
                              4 -> k (z CInlinQual)
                              5 -> k (z CAttrQual)
          toConstr (ctor@(CConstQual x1)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(CVolatQual x1)) = indexConstr (dataTypeOf ctor) 2
          toConstr (ctor@(CRestrQual x1)) = indexConstr (dataTypeOf ctor) 3
          toConstr (ctor@(CInlinQual x1)) = indexConstr (dataTypeOf ctor) 4
          toConstr (ctor@(CAttrQual x1)) = indexConstr (dataTypeOf ctor) 5
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CTypeQual" [con_C1,
                                                              con_C2,
                                                              con_C3,
                                                              con_C4,
                                                              con_C5]
                               con_C1 = mkConstr ty_T "CConstQual" [] Prefix
                               con_C2 = mkConstr ty_T "CVolatQual" [] Prefix
                               con_C3 = mkConstr ty_T "CRestrQual" [] Prefix
                               con_C4 = mkConstr ty_T "CInlinQual" [] Prefix
                               con_C5 = mkConstr ty_T "CAttrQual" [] Prefix

typename_CTypeQual = mkTyCon "CTypeQual"
instance Typeable CTypeQual
    where typeOf _ = mkTyConApp typename_CTypeQual []

instance Data CStructUnion
    where gfoldl k r (CStruct x1
                              x2
                              x3
                              x4
                              x5) = k (k (k (k (k (r CStruct) x1) x2) x3) x4) x5
          gunfold k z c = case constrIndex c of
                              1 -> k (k (k (k (k (z CStruct)))))
          toConstr (ctor@(CStruct x1
                                  x2
                                  x3
                                  x4
                                  x5)) = indexConstr (dataTypeOf ctor) 1
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CStructUnion" [con_C1]
                               con_C1 = mkConstr ty_T "CStruct" [] Prefix

typename_CStructUnion = mkTyCon "CStructUnion"
instance Typeable CStructUnion
    where typeOf _ = mkTyConApp typename_CStructUnion []

instance Data CStructTag
    where gfoldl k r (CStructTag) = r CStructTag
          gfoldl k r (CUnionTag) = r CUnionTag
          gunfold k z c = case constrIndex c of
                              1 -> z CStructTag
                              2 -> z CUnionTag
          toConstr (ctor@(CStructTag)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(CUnionTag)) = indexConstr (dataTypeOf ctor) 2
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CStructTag" [con_C1, con_C2]
                               con_C1 = mkConstr ty_T "CStructTag" [] Prefix
                               con_C2 = mkConstr ty_T "CUnionTag" [] Prefix

typename_CStructTag = mkTyCon "CStructTag"
instance Typeable CStructTag
    where typeOf _ = mkTyConApp typename_CStructTag []

instance Data CEnum
    where gfoldl k r (CEnum x1
                            x2
                            x3
                            x4) = k (k (k (k (r CEnum) x1) x2) x3) x4
          gunfold k z c = case constrIndex c of
                              1 -> k (k (k (k (z CEnum))))
          toConstr (ctor@(CEnum x1
                                x2
                                x3
                                x4)) = indexConstr (dataTypeOf ctor) 1
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CEnum" [con_C1]
                               con_C1 = mkConstr ty_T "CEnum" [] Prefix

typename_CEnum = mkTyCon "CEnum"
instance Typeable CEnum
    where typeOf _ = mkTyConApp typename_CEnum []

instance Data CDeclr
    where gfoldl k r (CDeclr x1
                             x2
                             x3
                             x4
                             x5) = k (k (k (k (k (r CDeclr) x1) x2) x3) x4) x5
          gunfold k z c = case constrIndex c of
                              1 -> k (k (k (k (k (z CDeclr)))))
          toConstr (ctor@(CDeclr x1
                                 x2
                                 x3
                                 x4
                                 x5)) = indexConstr (dataTypeOf ctor) 1
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CDeclr" [con_C1]
                               con_C1 = mkConstr ty_T "CDeclr" [] Prefix

typename_CDeclr = mkTyCon "CDeclr"
instance Typeable CDeclr
    where typeOf _ = mkTyConApp typename_CDeclr []

instance Data CDerivedDeclr
    where gfoldl k r (CPtrDeclr x1 x2) = k (k (r CPtrDeclr) x1) x2
          gfoldl k r (CArrDeclr x1 x2 x3) = k (k (k (r CArrDeclr) x1) x2) x3
          gfoldl k r (CFunDeclr x1 x2 x3) = k (k (k (r CFunDeclr) x1) x2) x3
          gunfold k z c = case constrIndex c of
                              1 -> k (k (z CPtrDeclr))
                              2 -> k (k (k (z CArrDeclr)))
                              3 -> k (k (k (z CFunDeclr)))
          toConstr (ctor@(CPtrDeclr x1 x2)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(CArrDeclr x1
                                    x2
                                    x3)) = indexConstr (dataTypeOf ctor) 2
          toConstr (ctor@(CFunDeclr x1
                                    x2
                                    x3)) = indexConstr (dataTypeOf ctor) 3
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CDerivedDeclr" [con_C1, con_C2, con_C3]
                               con_C1 = mkConstr ty_T "CPtrDeclr" [] Prefix
                               con_C2 = mkConstr ty_T "CArrDeclr" [] Prefix
                               con_C3 = mkConstr ty_T "CFunDeclr" [] Prefix

typename_CDerivedDeclr = mkTyCon "CDerivedDeclr"
instance Typeable CDerivedDeclr
    where typeOf _ = mkTyConApp typename_CDerivedDeclr []

instance Data CInit
    where gfoldl k r (CInitExpr x1 x2) = k (k (r CInitExpr) x1) x2
          gfoldl k r (CInitList x1 x2) = k (k (r CInitList) x1) x2
          gunfold k z c = case constrIndex c of
                              1 -> k (k (z CInitExpr))
                              2 -> k (k (z CInitList))
          toConstr (ctor@(CInitExpr x1 x2)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(CInitList x1 x2)) = indexConstr (dataTypeOf ctor) 2
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CInit" [con_C1, con_C2]
                               con_C1 = mkConstr ty_T "CInitExpr" [] Prefix
                               con_C2 = mkConstr ty_T "CInitList" [] Prefix

typename_CInit = mkTyCon "CInit"
instance Typeable CInit
    where typeOf _ = mkTyConApp typename_CInit []

instance Data CDesignator
    where gfoldl k r (CArrDesig x1 x2) = k (k (r CArrDesig) x1) x2
          gfoldl k r (CMemberDesig x1 x2) = k (k (r CMemberDesig) x1) x2
          gfoldl k r (CRangeDesig x1
                                  x2
                                  x3) = k (k (k (r CRangeDesig) x1) x2) x3
          gunfold k z c = case constrIndex c of
                              1 -> k (k (z CArrDesig))
                              2 -> k (k (z CMemberDesig))
                              3 -> k (k (k (z CRangeDesig)))
          toConstr (ctor@(CArrDesig x1 x2)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(CMemberDesig x1
                                       x2)) = indexConstr (dataTypeOf ctor) 2
          toConstr (ctor@(CRangeDesig x1
                                      x2
                                      x3)) = indexConstr (dataTypeOf ctor) 3
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CDesignator" [con_C1, con_C2, con_C3]
                               con_C1 = mkConstr ty_T "CArrDesig" [] Prefix
                               con_C2 = mkConstr ty_T "CMemberDesig" [] Prefix
                               con_C3 = mkConstr ty_T "CRangeDesig" [] Prefix

typename_CDesignator = mkTyCon "CDesignator"
instance Typeable CDesignator
    where typeOf _ = mkTyConApp typename_CDesignator []

instance Data CAttr
    where gfoldl k r (CAttr x1 x2 x3) = k (k (k (r CAttr) x1) x2) x3
          gunfold k z c = case constrIndex c of
                              1 -> k (k (k (z CAttr)))
          toConstr (ctor@(CAttr x1 x2 x3)) = indexConstr (dataTypeOf ctor) 1
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CAttr" [con_C1]
                               con_C1 = mkConstr ty_T "CAttr" [] Prefix

typename_CAttr = mkTyCon "CAttr"
instance Typeable CAttr
    where typeOf _ = mkTyConApp typename_CAttr []

instance Data CExpr
    where gfoldl k r (CComma x1 x2) = k (k (r CComma) x1) x2
          gfoldl k r (CAssign x1
                              x2
                              x3
                              x4) = k (k (k (k (r CAssign) x1) x2) x3) x4
          gfoldl k r (CCond x1
                            x2
                            x3
                            x4) = k (k (k (k (r CCond) x1) x2) x3) x4
          gfoldl k r (CBinary x1
                              x2
                              x3
                              x4) = k (k (k (k (r CBinary) x1) x2) x3) x4
          gfoldl k r (CCast x1 x2 x3) = k (k (k (r CCast) x1) x2) x3
          gfoldl k r (CUnary x1 x2 x3) = k (k (k (r CUnary) x1) x2) x3
          gfoldl k r (CSizeofExpr x1 x2) = k (k (r CSizeofExpr) x1) x2
          gfoldl k r (CSizeofType x1 x2) = k (k (r CSizeofType) x1) x2
          gfoldl k r (CAlignofExpr x1 x2) = k (k (r CAlignofExpr) x1) x2
          gfoldl k r (CAlignofType x1 x2) = k (k (r CAlignofType) x1) x2
          gfoldl k r (CComplexReal x1 x2) = k (k (r CComplexReal) x1) x2
          gfoldl k r (CComplexImag x1 x2) = k (k (r CComplexImag) x1) x2
          gfoldl k r (CIndex x1 x2 x3) = k (k (k (r CIndex) x1) x2) x3
          gfoldl k r (CCall x1 x2 x3) = k (k (k (r CCall) x1) x2) x3
          gfoldl k r (CMember x1
                              x2
                              x3
                              x4) = k (k (k (k (r CMember) x1) x2) x3) x4
          gfoldl k r (CVar x1 x2) = k (k (r CVar) x1) x2
          gfoldl k r (CConst x1 x2) = k (k (r CConst) x1) x2
          gfoldl k r (CCompoundLit x1
                                   x2
                                   x3) = k (k (k (r CCompoundLit) x1) x2) x3
          gfoldl k r (CStatExpr x1 x2) = k (k (r CStatExpr) x1) x2
          gfoldl k r (CLabAddrExpr x1 x2) = k (k (r CLabAddrExpr) x1) x2
          gfoldl k r (CBuiltinExpr x1) = k (r CBuiltinExpr) x1
          gunfold k z c = case constrIndex c of
                              1 -> k (k (z CComma))
                              2 -> k (k (k (k (z CAssign))))
                              3 -> k (k (k (k (z CCond))))
                              4 -> k (k (k (k (z CBinary))))
                              5 -> k (k (k (z CCast)))
                              6 -> k (k (k (z CUnary)))
                              7 -> k (k (z CSizeofExpr))
                              8 -> k (k (z CSizeofType))
                              9 -> k (k (z CAlignofExpr))
                              10 -> k (k (z CAlignofType))
                              11 -> k (k (z CComplexReal))
                              12 -> k (k (z CComplexImag))
                              13 -> k (k (k (z CIndex)))
                              14 -> k (k (k (z CCall)))
                              15 -> k (k (k (k (z CMember))))
                              16 -> k (k (z CVar))
                              17 -> k (k (z CConst))
                              18 -> k (k (k (z CCompoundLit)))
                              19 -> k (k (z CStatExpr))
                              20 -> k (k (z CLabAddrExpr))
                              21 -> k (z CBuiltinExpr)
          toConstr (ctor@(CComma x1 x2)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(CAssign x1
                                  x2
                                  x3
                                  x4)) = indexConstr (dataTypeOf ctor) 2
          toConstr (ctor@(CCond x1
                                x2
                                x3
                                x4)) = indexConstr (dataTypeOf ctor) 3
          toConstr (ctor@(CBinary x1
                                  x2
                                  x3
                                  x4)) = indexConstr (dataTypeOf ctor) 4
          toConstr (ctor@(CCast x1 x2 x3)) = indexConstr (dataTypeOf ctor) 5
          toConstr (ctor@(CUnary x1 x2 x3)) = indexConstr (dataTypeOf ctor) 6
          toConstr (ctor@(CSizeofExpr x1
                                      x2)) = indexConstr (dataTypeOf ctor) 7
          toConstr (ctor@(CSizeofType x1
                                      x2)) = indexConstr (dataTypeOf ctor) 8
          toConstr (ctor@(CAlignofExpr x1
                                       x2)) = indexConstr (dataTypeOf ctor) 9
          toConstr (ctor@(CAlignofType x1
                                       x2)) = indexConstr (dataTypeOf ctor) 10
          toConstr (ctor@(CComplexReal x1
                                       x2)) = indexConstr (dataTypeOf ctor) 11
          toConstr (ctor@(CComplexImag x1
                                       x2)) = indexConstr (dataTypeOf ctor) 12
          toConstr (ctor@(CIndex x1
                                 x2
                                 x3)) = indexConstr (dataTypeOf ctor) 13
          toConstr (ctor@(CCall x1 x2 x3)) = indexConstr (dataTypeOf ctor) 14
          toConstr (ctor@(CMember x1
                                  x2
                                  x3
                                  x4)) = indexConstr (dataTypeOf ctor) 15
          toConstr (ctor@(CVar x1 x2)) = indexConstr (dataTypeOf ctor) 16
          toConstr (ctor@(CConst x1 x2)) = indexConstr (dataTypeOf ctor) 17
          toConstr (ctor@(CCompoundLit x1
                                       x2
                                       x3)) = indexConstr (dataTypeOf ctor) 18
          toConstr (ctor@(CStatExpr x1
                                    x2)) = indexConstr (dataTypeOf ctor) 19
          toConstr (ctor@(CLabAddrExpr x1
                                       x2)) = indexConstr (dataTypeOf ctor) 20
          toConstr (ctor@(CBuiltinExpr x1)) = indexConstr (dataTypeOf ctor) 21
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CExpr" [con_C1,
                                                          con_C2,
                                                          con_C3,
                                                          con_C4,
                                                          con_C5,
                                                          con_C6,
                                                          con_C7,
                                                          con_C8,
                                                          con_C9,
                                                          con_C10,
                                                          con_C11,
                                                          con_C12,
                                                          con_C13,
                                                          con_C14,
                                                          con_C15,
                                                          con_C16,
                                                          con_C17,
                                                          con_C18,
                                                          con_C19,
                                                          con_C20,
                                                          con_C21]
                               con_C1 = mkConstr ty_T "CComma" [] Prefix
                               con_C2 = mkConstr ty_T "CAssign" [] Prefix
                               con_C3 = mkConstr ty_T "CCond" [] Prefix
                               con_C4 = mkConstr ty_T "CBinary" [] Prefix
                               con_C5 = mkConstr ty_T "CCast" [] Prefix
                               con_C6 = mkConstr ty_T "CUnary" [] Prefix
                               con_C7 = mkConstr ty_T "CSizeofExpr" [] Prefix
                               con_C8 = mkConstr ty_T "CSizeofType" [] Prefix
                               con_C9 = mkConstr ty_T "CAlignofExpr" [] Prefix
                               con_C10 = mkConstr ty_T "CAlignofType" [] Prefix
                               con_C11 = mkConstr ty_T "CComplexReal" [] Prefix
                               con_C12 = mkConstr ty_T "CComplexImag" [] Prefix
                               con_C13 = mkConstr ty_T "CIndex" [] Prefix
                               con_C14 = mkConstr ty_T "CCall" [] Prefix
                               con_C15 = mkConstr ty_T "CMember" [] Prefix
                               con_C16 = mkConstr ty_T "CVar" [] Prefix
                               con_C17 = mkConstr ty_T "CConst" [] Prefix
                               con_C18 = mkConstr ty_T "CCompoundLit" [] Prefix
                               con_C19 = mkConstr ty_T "CStatExpr" [] Prefix
                               con_C20 = mkConstr ty_T "CLabAddrExpr" [] Prefix
                               con_C21 = mkConstr ty_T "CBuiltinExpr" [] Prefix

typename_CExpr = mkTyCon "CExpr"
instance Typeable CExpr
    where typeOf _ = mkTyConApp typename_CExpr []

instance Data CBuiltin
    where gfoldl k r (CBuiltinVaArg x1
                                    x2
                                    x3) = k (k (k (r CBuiltinVaArg) x1) x2) x3
          gfoldl k r (CBuiltinOffsetOf x1
                                       x2
                                       x3) = k (k (k (r CBuiltinOffsetOf) x1) x2) x3
          gfoldl k r (CBuiltinTypesCompatible x1
                                              x2
                                              x3) = k (k (k (r CBuiltinTypesCompatible) x1) x2) x3
          gunfold k z c = case constrIndex c of
                              1 -> k (k (k (z CBuiltinVaArg)))
                              2 -> k (k (k (z CBuiltinOffsetOf)))
                              3 -> k (k (k (z CBuiltinTypesCompatible)))
          toConstr (ctor@(CBuiltinVaArg x1
                                        x2
                                        x3)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(CBuiltinOffsetOf x1
                                           x2
                                           x3)) = indexConstr (dataTypeOf ctor) 2
          toConstr (ctor@(CBuiltinTypesCompatible x1
                                                  x2
                                                  x3)) = indexConstr (dataTypeOf ctor) 3
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CBuiltin" [con_C1, con_C2, con_C3]
                               con_C1 = mkConstr ty_T "CBuiltinVaArg" [] Prefix
                               con_C2 = mkConstr ty_T "CBuiltinOffsetOf" [] Prefix
                               con_C3 = mkConstr ty_T "CBuiltinTypesCompatible" [] Prefix

typename_CBuiltin = mkTyCon "CBuiltin"
instance Typeable CBuiltin
    where typeOf _ = mkTyConApp typename_CBuiltin []

instance Data CAssignOp
    where gfoldl k r (CAssignOp) = r CAssignOp
          gfoldl k r (CMulAssOp) = r CMulAssOp
          gfoldl k r (CDivAssOp) = r CDivAssOp
          gfoldl k r (CRmdAssOp) = r CRmdAssOp
          gfoldl k r (CAddAssOp) = r CAddAssOp
          gfoldl k r (CSubAssOp) = r CSubAssOp
          gfoldl k r (CShlAssOp) = r CShlAssOp
          gfoldl k r (CShrAssOp) = r CShrAssOp
          gfoldl k r (CAndAssOp) = r CAndAssOp
          gfoldl k r (CXorAssOp) = r CXorAssOp
          gfoldl k r (COrAssOp) = r COrAssOp
          gunfold k z c = case constrIndex c of
                              1 -> z CAssignOp
                              2 -> z CMulAssOp
                              3 -> z CDivAssOp
                              4 -> z CRmdAssOp
                              5 -> z CAddAssOp
                              6 -> z CSubAssOp
                              7 -> z CShlAssOp
                              8 -> z CShrAssOp
                              9 -> z CAndAssOp
                              10 -> z CXorAssOp
                              11 -> z COrAssOp
          toConstr (ctor@(CAssignOp)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(CMulAssOp)) = indexConstr (dataTypeOf ctor) 2
          toConstr (ctor@(CDivAssOp)) = indexConstr (dataTypeOf ctor) 3
          toConstr (ctor@(CRmdAssOp)) = indexConstr (dataTypeOf ctor) 4
          toConstr (ctor@(CAddAssOp)) = indexConstr (dataTypeOf ctor) 5
          toConstr (ctor@(CSubAssOp)) = indexConstr (dataTypeOf ctor) 6
          toConstr (ctor@(CShlAssOp)) = indexConstr (dataTypeOf ctor) 7
          toConstr (ctor@(CShrAssOp)) = indexConstr (dataTypeOf ctor) 8
          toConstr (ctor@(CAndAssOp)) = indexConstr (dataTypeOf ctor) 9
          toConstr (ctor@(CXorAssOp)) = indexConstr (dataTypeOf ctor) 10
          toConstr (ctor@(COrAssOp)) = indexConstr (dataTypeOf ctor) 11
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CAssignOp" [con_C1,
                                                              con_C2,
                                                              con_C3,
                                                              con_C4,
                                                              con_C5,
                                                              con_C6,
                                                              con_C7,
                                                              con_C8,
                                                              con_C9,
                                                              con_C10,
                                                              con_C11]
                               con_C1 = mkConstr ty_T "CAssignOp" [] Prefix
                               con_C2 = mkConstr ty_T "CMulAssOp" [] Prefix
                               con_C3 = mkConstr ty_T "CDivAssOp" [] Prefix
                               con_C4 = mkConstr ty_T "CRmdAssOp" [] Prefix
                               con_C5 = mkConstr ty_T "CAddAssOp" [] Prefix
                               con_C6 = mkConstr ty_T "CSubAssOp" [] Prefix
                               con_C7 = mkConstr ty_T "CShlAssOp" [] Prefix
                               con_C8 = mkConstr ty_T "CShrAssOp" [] Prefix
                               con_C9 = mkConstr ty_T "CAndAssOp" [] Prefix
                               con_C10 = mkConstr ty_T "CXorAssOp" [] Prefix
                               con_C11 = mkConstr ty_T "COrAssOp" [] Prefix

typename_CAssignOp = mkTyCon "CAssignOp"
instance Typeable CAssignOp
    where typeOf _ = mkTyConApp typename_CAssignOp []

instance Data CBinaryOp
    where gfoldl k r (CMulOp) = r CMulOp
          gfoldl k r (CDivOp) = r CDivOp
          gfoldl k r (CRmdOp) = r CRmdOp
          gfoldl k r (CAddOp) = r CAddOp
          gfoldl k r (CSubOp) = r CSubOp
          gfoldl k r (CShlOp) = r CShlOp
          gfoldl k r (CShrOp) = r CShrOp
          gfoldl k r (CLeOp) = r CLeOp
          gfoldl k r (CGrOp) = r CGrOp
          gfoldl k r (CLeqOp) = r CLeqOp
          gfoldl k r (CGeqOp) = r CGeqOp
          gfoldl k r (CEqOp) = r CEqOp
          gfoldl k r (CNeqOp) = r CNeqOp
          gfoldl k r (CAndOp) = r CAndOp
          gfoldl k r (CXorOp) = r CXorOp
          gfoldl k r (COrOp) = r COrOp
          gfoldl k r (CLndOp) = r CLndOp
          gfoldl k r (CLorOp) = r CLorOp
          gunfold k z c = case constrIndex c of
                              1 -> z CMulOp
                              2 -> z CDivOp
                              3 -> z CRmdOp
                              4 -> z CAddOp
                              5 -> z CSubOp
                              6 -> z CShlOp
                              7 -> z CShrOp
                              8 -> z CLeOp
                              9 -> z CGrOp
                              10 -> z CLeqOp
                              11 -> z CGeqOp
                              12 -> z CEqOp
                              13 -> z CNeqOp
                              14 -> z CAndOp
                              15 -> z CXorOp
                              16 -> z COrOp
                              17 -> z CLndOp
                              18 -> z CLorOp
          toConstr (ctor@(CMulOp)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(CDivOp)) = indexConstr (dataTypeOf ctor) 2
          toConstr (ctor@(CRmdOp)) = indexConstr (dataTypeOf ctor) 3
          toConstr (ctor@(CAddOp)) = indexConstr (dataTypeOf ctor) 4
          toConstr (ctor@(CSubOp)) = indexConstr (dataTypeOf ctor) 5
          toConstr (ctor@(CShlOp)) = indexConstr (dataTypeOf ctor) 6
          toConstr (ctor@(CShrOp)) = indexConstr (dataTypeOf ctor) 7
          toConstr (ctor@(CLeOp)) = indexConstr (dataTypeOf ctor) 8
          toConstr (ctor@(CGrOp)) = indexConstr (dataTypeOf ctor) 9
          toConstr (ctor@(CLeqOp)) = indexConstr (dataTypeOf ctor) 10
          toConstr (ctor@(CGeqOp)) = indexConstr (dataTypeOf ctor) 11
          toConstr (ctor@(CEqOp)) = indexConstr (dataTypeOf ctor) 12
          toConstr (ctor@(CNeqOp)) = indexConstr (dataTypeOf ctor) 13
          toConstr (ctor@(CAndOp)) = indexConstr (dataTypeOf ctor) 14
          toConstr (ctor@(CXorOp)) = indexConstr (dataTypeOf ctor) 15
          toConstr (ctor@(COrOp)) = indexConstr (dataTypeOf ctor) 16
          toConstr (ctor@(CLndOp)) = indexConstr (dataTypeOf ctor) 17
          toConstr (ctor@(CLorOp)) = indexConstr (dataTypeOf ctor) 18
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CBinaryOp" [con_C1,
                                                              con_C2,
                                                              con_C3,
                                                              con_C4,
                                                              con_C5,
                                                              con_C6,
                                                              con_C7,
                                                              con_C8,
                                                              con_C9,
                                                              con_C10,
                                                              con_C11,
                                                              con_C12,
                                                              con_C13,
                                                              con_C14,
                                                              con_C15,
                                                              con_C16,
                                                              con_C17,
                                                              con_C18]
                               con_C1 = mkConstr ty_T "CMulOp" [] Prefix
                               con_C2 = mkConstr ty_T "CDivOp" [] Prefix
                               con_C3 = mkConstr ty_T "CRmdOp" [] Prefix
                               con_C4 = mkConstr ty_T "CAddOp" [] Prefix
                               con_C5 = mkConstr ty_T "CSubOp" [] Prefix
                               con_C6 = mkConstr ty_T "CShlOp" [] Prefix
                               con_C7 = mkConstr ty_T "CShrOp" [] Prefix
                               con_C8 = mkConstr ty_T "CLeOp" [] Prefix
                               con_C9 = mkConstr ty_T "CGrOp" [] Prefix
                               con_C10 = mkConstr ty_T "CLeqOp" [] Prefix
                               con_C11 = mkConstr ty_T "CGeqOp" [] Prefix
                               con_C12 = mkConstr ty_T "CEqOp" [] Prefix
                               con_C13 = mkConstr ty_T "CNeqOp" [] Prefix
                               con_C14 = mkConstr ty_T "CAndOp" [] Prefix
                               con_C15 = mkConstr ty_T "CXorOp" [] Prefix
                               con_C16 = mkConstr ty_T "COrOp" [] Prefix
                               con_C17 = mkConstr ty_T "CLndOp" [] Prefix
                               con_C18 = mkConstr ty_T "CLorOp" [] Prefix

typename_CBinaryOp = mkTyCon "CBinaryOp"
instance Typeable CBinaryOp
    where typeOf _ = mkTyConApp typename_CBinaryOp []

instance Data CUnaryOp
    where gfoldl k r (CPreIncOp) = r CPreIncOp
          gfoldl k r (CPreDecOp) = r CPreDecOp
          gfoldl k r (CPostIncOp) = r CPostIncOp
          gfoldl k r (CPostDecOp) = r CPostDecOp
          gfoldl k r (CAdrOp) = r CAdrOp
          gfoldl k r (CIndOp) = r CIndOp
          gfoldl k r (CPlusOp) = r CPlusOp
          gfoldl k r (CMinOp) = r CMinOp
          gfoldl k r (CCompOp) = r CCompOp
          gfoldl k r (CNegOp) = r CNegOp
          gunfold k z c = case constrIndex c of
                              1 -> z CPreIncOp
                              2 -> z CPreDecOp
                              3 -> z CPostIncOp
                              4 -> z CPostDecOp
                              5 -> z CAdrOp
                              6 -> z CIndOp
                              7 -> z CPlusOp
                              8 -> z CMinOp
                              9 -> z CCompOp
                              10 -> z CNegOp
          toConstr (ctor@(CPreIncOp)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(CPreDecOp)) = indexConstr (dataTypeOf ctor) 2
          toConstr (ctor@(CPostIncOp)) = indexConstr (dataTypeOf ctor) 3
          toConstr (ctor@(CPostDecOp)) = indexConstr (dataTypeOf ctor) 4
          toConstr (ctor@(CAdrOp)) = indexConstr (dataTypeOf ctor) 5
          toConstr (ctor@(CIndOp)) = indexConstr (dataTypeOf ctor) 6
          toConstr (ctor@(CPlusOp)) = indexConstr (dataTypeOf ctor) 7
          toConstr (ctor@(CMinOp)) = indexConstr (dataTypeOf ctor) 8
          toConstr (ctor@(CCompOp)) = indexConstr (dataTypeOf ctor) 9
          toConstr (ctor@(CNegOp)) = indexConstr (dataTypeOf ctor) 10
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CUnaryOp" [con_C1,
                                                             con_C2,
                                                             con_C3,
                                                             con_C4,
                                                             con_C5,
                                                             con_C6,
                                                             con_C7,
                                                             con_C8,
                                                             con_C9,
                                                             con_C10]
                               con_C1 = mkConstr ty_T "CPreIncOp" [] Prefix
                               con_C2 = mkConstr ty_T "CPreDecOp" [] Prefix
                               con_C3 = mkConstr ty_T "CPostIncOp" [] Prefix
                               con_C4 = mkConstr ty_T "CPostDecOp" [] Prefix
                               con_C5 = mkConstr ty_T "CAdrOp" [] Prefix
                               con_C6 = mkConstr ty_T "CIndOp" [] Prefix
                               con_C7 = mkConstr ty_T "CPlusOp" [] Prefix
                               con_C8 = mkConstr ty_T "CMinOp" [] Prefix
                               con_C9 = mkConstr ty_T "CCompOp" [] Prefix
                               con_C10 = mkConstr ty_T "CNegOp" [] Prefix

typename_CUnaryOp = mkTyCon "CUnaryOp"
instance Typeable CUnaryOp
    where typeOf _ = mkTyConApp typename_CUnaryOp []

instance Data CConst
    where gfoldl k r (CIntConst x1 x2) = k (k (r CIntConst) x1) x2
          gfoldl k r (CCharConst x1 x2) = k (k (r CCharConst) x1) x2
          gfoldl k r (CFloatConst x1 x2) = k (k (r CFloatConst) x1) x2
          gfoldl k r (CStrConst x1 x2) = k (k (r CStrConst) x1) x2
          gunfold k z c = case constrIndex c of
                              1 -> k (k (z CIntConst))
                              2 -> k (k (z CCharConst))
                              3 -> k (k (z CFloatConst))
                              4 -> k (k (z CStrConst))
          toConstr (ctor@(CIntConst x1 x2)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(CCharConst x1
                                     x2)) = indexConstr (dataTypeOf ctor) 2
          toConstr (ctor@(CFloatConst x1
                                      x2)) = indexConstr (dataTypeOf ctor) 3
          toConstr (ctor@(CStrConst x1 x2)) = indexConstr (dataTypeOf ctor) 4
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CConst" [con_C1, con_C2, con_C3, con_C4]
                               con_C1 = mkConstr ty_T "CIntConst" [] Prefix
                               con_C2 = mkConstr ty_T "CCharConst" [] Prefix
                               con_C3 = mkConstr ty_T "CFloatConst" [] Prefix
                               con_C4 = mkConstr ty_T "CStrConst" [] Prefix

typename_CConst = mkTyCon "CConst"
instance Typeable CConst
    where typeOf _ = mkTyConApp typename_CConst []

instance Data CStrLit
    where gfoldl k r (CStrLit x1 x2) = k (k (r CStrLit) x1) x2
          gunfold k z c = case constrIndex c of
                              1 -> k (k (z CStrLit))
          toConstr (ctor@(CStrLit x1 x2)) = indexConstr (dataTypeOf ctor) 1
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CStrLit" [con_C1]
                               con_C1 = mkConstr ty_T "CStrLit" [] Prefix

typename_CStrLit = mkTyCon "CStrLit"
instance Typeable CStrLit
    where typeOf _ = mkTyConApp typename_CStrLit []

instance Data CObj
    where gfoldl k r (TypeCO x1) = k (r TypeCO) x1
          gfoldl k r (ObjCO x1) = k (r ObjCO) x1
          gfoldl k r (EnumCO x1 x2) = k (k (r EnumCO) x1) x2
          gfoldl k r (BuiltinCO) = r BuiltinCO
          gunfold k z c = case constrIndex c of
                              1 -> k (z TypeCO)
                              2 -> k (z ObjCO)
                              3 -> k (k (z EnumCO))
                              4 -> z BuiltinCO
          toConstr (ctor@(TypeCO x1)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(ObjCO x1)) = indexConstr (dataTypeOf ctor) 2
          toConstr (ctor@(EnumCO x1 x2)) = indexConstr (dataTypeOf ctor) 3
          toConstr (ctor@(BuiltinCO)) = indexConstr (dataTypeOf ctor) 4
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CObj" [con_C1, con_C2, con_C3, con_C4]
                               con_C1 = mkConstr ty_T "TypeCO" [] Prefix
                               con_C2 = mkConstr ty_T "ObjCO" [] Prefix
                               con_C3 = mkConstr ty_T "EnumCO" [] Prefix
                               con_C4 = mkConstr ty_T "BuiltinCO" [] Prefix

typename_CObj = mkTyCon "CObj"
instance Typeable CObj
    where typeOf _ = mkTyConApp typename_CObj []

instance Data CTag
    where gfoldl k r (StructUnionCT x1) = k (r StructUnionCT) x1
          gfoldl k r (EnumCT x1) = k (r EnumCT) x1
          gunfold k z c = case constrIndex c of
                              1 -> k (z StructUnionCT)
                              2 -> k (z EnumCT)
          toConstr (ctor@(StructUnionCT x1)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(EnumCT x1)) = indexConstr (dataTypeOf ctor) 2
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CTag" [con_C1, con_C2]
                               con_C1 = mkConstr ty_T "StructUnionCT" [] Prefix
                               con_C2 = mkConstr ty_T "EnumCT" [] Prefix

typename_CTag = mkTyCon "CTag"
instance Typeable CTag
    where typeOf _ = mkTyConApp typename_CTag []

instance Data CDef
    where gfoldl k r (UndefCD) = r UndefCD
          gfoldl k r (DontCareCD) = r DontCareCD
          gfoldl k r (ObjCD x1) = k (r ObjCD) x1
          gfoldl k r (TagCD x1) = k (r TagCD) x1
          gunfold k z c = case constrIndex c of
                              1 -> z UndefCD
                              2 -> z DontCareCD
                              3 -> k (z ObjCD)
                              4 -> k (z TagCD)
          toConstr (ctor@(UndefCD)) = indexConstr (dataTypeOf ctor) 1
          toConstr (ctor@(DontCareCD)) = indexConstr (dataTypeOf ctor) 2
          toConstr (ctor@(ObjCD x1)) = indexConstr (dataTypeOf ctor) 3
          toConstr (ctor@(TagCD x1)) = indexConstr (dataTypeOf ctor) 4
          dataTypeOf _ = ty_T
                         where ty_T = mkDataType "CDef" [con_C1, con_C2, con_C3, con_C4]
                               con_C1 = mkConstr ty_T "UndefCD" [] Prefix
                               con_C2 = mkConstr ty_T "DontCareCD" [] Prefix
                               con_C3 = mkConstr ty_T "ObjCD" [] Prefix
                               con_C4 = mkConstr ty_T "TagCD" [] Prefix

typename_CDef = mkTyCon "CDef"
instance Typeable CDef
    where typeOf _ = mkTyConApp typename_CDef []
{-
deriving instance Typeable Name
deriving instance Data Name
deriving instance Typeable Ident
deriving instance Data Ident

deriving instance Typeable Attrs
deriving instance Data Attrs
deriving instance Typeable Position
deriving instance Data Position
deriving instance Typeable CTranslUnit
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
deriving instance Typeable CDerivedDeclr
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
deriving instance Data CTranslUnit
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
deriving instance Data CDerivedDeclr
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
-}