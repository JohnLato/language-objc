-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis.Export
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  alpha
-- Portability :  unspecified
--
-- /WARNING/ : This is just an implementation sketch
--
-- Export SemRep entities to AST nodes
--
-- This is just a stub. In practice, we'll maybe want a unique name generation
-- monad or something like that.
-----------------------------------------------------------------------------
module Language.C.Analysis.Export (
exportType, exportTypeDecl, exportTypeSpec,
exportCompType, exportCompTypeDecl, exportCompTypeRef,
exportEnumTypeDecl, exportEnumTypeRef,
)
where
import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Syntax.AST
import Language.C.Analysis.SemRep
import Data.Maybe

exportDeclr :: [CDeclSpec] -> Type -> Attributes -> VarName -> ([CDeclSpec],CDeclr)
exportDeclr other_specs ty attrs name =
    (other_specs ++ specs, CDeclr ident derived asmname (exportAttrs attrs) ni)
    where 
    (specs,derived) = exportType ty
    (ident,asmname) = case name of (VarName ident asmname_opt) -> (Just ident, asmname_opt)
                                   _ -> (Nothing,Nothing)

exportTypeDecl :: Type -> CDecl
exportTypeDecl ty = 
  CDecl declspecs declrs ni
  where
  (declspecs,derived) = exportType ty
  declrs | null derived = []
         | otherwise = [(Just $ CDeclr Nothing derived Nothing [] ni,Nothing,Nothing)]

exportType :: Type -> ([CDeclSpec],[CDerivedDeclr])
exportType ty = exportTy [] ty
    where
    exportTy dd (PtrType ty tyquals attrs) = exportTy (ptr_declr : dd) ty where
        ptr_declr = CPtrDeclr (exportTypeQuals tyquals) ni
    exportTy dd (ArrayType ty array_sz tyquals attrs) = exportTy (arr_declr : dd) ty where
        arr_declr = CArrDeclr (exportTypeQuals tyquals) (exportArraySize array_sz) ni 
    exportTy dd (FunctionType (FunType ty params variadic attrs)) = exportTy (fun_declr : dd) ty where
        fun_declr = CFunDeclr (Right (map exportParamDecl params,variadic)) (exportAttrs attrs) ni
    exportTy dd (TypeDefType (TypeDefRef ty_ident _ node)) = ([CTypeSpec (CTypeDef ty_ident node)], reverse dd)
    exportTy dd (DirectType ty quals attrs) = (map CTypeQual (exportTypeQuals quals) ++ 
                                              map (CTypeQual . CAttrQual) (exportAttrs attrs) ++
                                              map CTypeSpec (exportTypeSpec ty), reverse dd) 
exportTypeQuals :: TypeQuals -> [CTypeQual]
exportTypeQuals quals = mapMaybe (select quals) [(constant,CConstQual ni),(volatile,CVolatQual ni),(restrict,CRestrQual ni)]
    where
    select quals (predicate,tyqual) | predicate quals = Just tyqual
                                    | otherwise       = Nothing
exportArraySize :: ArraySize -> CArrSize
exportArraySize (ArraySize static e) = CArrSize static e
exportArraySize (UnknownArraySize complete) = CNoArrSize complete

exportTypeSpec :: TypeName -> [CTypeSpec]
exportTypeSpec tyname =
    case tyname of
        TyVoid -> [CVoidType ni]
        TyIntegral int -> exportIntType int
        TyFloating quals ty -> exportFloatType quals ty
        TyComp comp -> exportCompTypeDecl comp
        TyEnum enum -> exportEnumTypeDecl enum
        TyBuiltin TyVaList -> [CTypeDef (ident "va_list") ni]
exportIntType :: IntType -> [CTypeSpec]
exportIntType ty = 
    case ty of
      TyBool    -> [CBoolType ni]
      TyChar    -> [CCharType ni] 
      TySChar   -> [CSignedType ni,CCharType ni]
      TyUChar   -> [CUnsigType ni,CCharType ni]
      TyShort   -> [CShortType ni]
      TyUShort  -> [CUnsigType ni, CShortType ni]
      TyInt     -> [CIntType ni]
      TyUInt    -> [CUnsigType ni, CIntType ni]
      TyLong    -> [CLongType ni]
      TyULong   -> [CUnsigType ni,CLongType ni]
      TyLLong   -> [CLongType ni, CLongType ni]
      TyULLong  -> [CUnsigType ni, CLongType ni, CLongType ni]
exportFloatType :: FloatTypeQuals -> FloatType -> [CTypeSpec]
exportFloatType qual ty = (case qual of TyComplex -> (CComplexType ni :); _ -> id) $
    case ty of
      TyFloat   -> [CFloatType ni]
      TyDouble  -> [CDoubleType ni]
      TyLDouble -> [CLongType ni, CDoubleType ni]

exportCompTypeDecl :: CompTypeDecl -> [CTypeSpec]
exportCompTypeDecl ty = [CSUType (exportComp ty) ni]
    where
    exportComp (CompTypeDecl sue_ref comp_tag node_info) =
        CStruct (if comp_tag == StructTag then CStructTag else CUnionTag)
                (exportSUERef sue_ref) Nothing [] ni

exportEnumTypeDecl :: EnumTypeDecl -> [CTypeSpec]
exportEnumTypeDecl ty = [CEnumType (exportEnum ty) ni]
    where
    exportEnum (EnumTypeDecl sue_ref node_info) =
        CEnum (exportSUERef sue_ref) Nothing [] ni

exportCompType :: CompType -> [CTypeSpec]
exportCompType (CompType sue_ref comp_tag members attrs node_info) = [CSUType comp ni]
    where
    comp = CStruct (if comp_tag == StructTag then CStructTag else CUnionTag)
                   (exportSUERef sue_ref)
                   (Just (map exportMemberDecl members))
                   (exportAttrs attrs)
                   node_info
exportCompTypeRef :: CompType -> [CTypeSpec]
exportCompTypeRef (CompType sue_ref com_tag  _ _ node_info) = exportCompTypeDecl (CompTypeDecl sue_ref com_tag node_info) 

exportEnumTypeRef :: EnumType -> [CTypeSpec]
exportEnumTypeRef (EnumType sue_ref _ _ node_info) = exportEnumTypeDecl (EnumTypeDecl sue_ref node_info)
                   
exportSUERef = Just . ident . show -- relies on a the source program not having any $'s in it                   

exportMemberDecl :: MemberDecl -> CDecl
exportMemberDecl (AnonBitField ty expr node_info) = 
    CDecl (map CTypeSpec $ exportTypeSpec $ fromDirectType ty) [(Nothing,Nothing,Just expr)] node_info
exportMemberDecl (MemberDecl vardecl bitfieldsz node_info) = 
    let (specs,declarator) = exportVarDecl vardecl
    in  CDecl specs [(Just declarator, Nothing, bitfieldsz)] node_info
exportVarDecl :: VarDecl -> ([CDeclSpec],CDeclr)
-- FIXME: there is an ambiguity between two possible places for __attributes__ s here
exportVarDecl (VarDecl name attrs ty) = exportDeclr (exportDeclAttrs attrs) ty [] name
exportParamDecl :: ParamDecl -> CDecl
exportParamDecl (ParamDecl vardecl node_info) =
    let (specs,declr) = exportVarDecl vardecl
    in CDecl specs [(Just declr, Nothing , Nothing) ] node_info
    
-- this is just a stub - it depends on the type of declaration who to export attributes
exportDeclAttrs :: DeclAttrs -> [CDeclSpec]
exportDeclAttrs (DeclAttrs inline storage attrs) =
       (if inline then [CTypeQual (CInlineQual ni)] else [])
    ++ map (CStorageSpec) (exportStorage storage)
    ++ map (CTypeQual . CAttrQual) (exportAttrs attrs)
exportStorage :: Storage -> [CStorageSpec]
exportStorage NoStorage = []
exportStorage Register  = [CRegister ni]
exportStorage _ = error "TODO"

-- Doh ! useless wrapping never pays off
exportAttrs = map exportAttr where
    exportAttr (Attr ident es ni) = CAttr ident es ni
fromDirectType (DirectType ty _ _) = ty
fromDirectType (TypeDefType (TypeDefRef _ ref _)) = maybe (error "undefined typedef") fromDirectType ref
fromDirectType _ = error "fromDirectType"
ni :: NodeInfo
ni = internalNode
ident :: String -> Ident
ident = internalIdent
