-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis.Export
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  alpha
-- Portability :  unspecified
--
-- /WARNING/ : This is just an implementation sketch and not very well tested.
--
-- Export 'SemRep' entities to 'AST' nodes.
-----------------------------------------------------------------------------
module Language.C.Analysis.Export (
exportType, exportTypeDecl, exportTypeSpec,
exportTypeDef,
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
    (ident,asmname) = case name of (VarName vident asmname_opt) -> (Just vident, asmname_opt)
                                   _ -> (Nothing,Nothing)

exportTypeDecl :: Type -> CDecl
exportTypeDecl ty =
  CDecl declspecs declrs ni
  where
  (declspecs,derived) = exportType ty
  declrs | null derived = []
         | otherwise = [(Just $ CDeclr Nothing derived Nothing [] ni,Nothing,Nothing)]

exportTypeDef :: TypeDef -> CDecl
exportTypeDef (TypeDef ident ty attrs node_info) =
  CDecl (CStorageSpec (CTypedef ni) : declspecs) [declr] node_info
  where
  (declspecs,derived) = exportType ty
  declr = (Just $ CDeclr (Just ident) derived Nothing (exportAttrs attrs) ni, Nothing, Nothing)

exportType :: Type -> ([CDeclSpec],[CDerivedDeclr])
exportType ty = exportTy [] ty
    where
    exportTy dd (PtrType ity tyquals attrs) = exportTy (ptr_declr : dd) ity where
        ptr_declr = CPtrDeclr (exportTypeQualsAttrs tyquals attrs) ni
    exportTy dd (ArrayType ity array_sz tyquals attrs) = exportTy (arr_declr : dd) ity where
        arr_declr = CArrDeclr (exportTypeQualsAttrs tyquals attrs) (exportArraySize array_sz) ni
    exportTy dd (FunctionType (FunType ity params variadic attrs)) = exportTy (fun_declr : dd) ity where
        fun_declr = CFunDeclr (Right (map exportParamDecl params,variadic)) (exportAttrs attrs) ni
    exportTy dd (FunctionType (FunTypeIncomplete ity attrs)) = exportTy (fun_declr : dd) ity where
        fun_declr = CFunDeclr (Right ([],False)) (exportAttrs attrs) ni
    exportTy dd (TypeDefType (TypeDefRef ty_ident _ node)) = ([CTypeSpec (CTypeDef ty_ident node)], reverse dd)
    exportTy dd (DirectType ity quals) = (map CTypeQual (exportTypeQuals quals) ++
                                        map CTypeSpec (exportTypeSpec ity), reverse dd)
    exportTy _ (TypeOfExpr _) = error "export of TypeOfExpr isn't supported"

exportTypeQuals :: TypeQuals -> [CTypeQual]
exportTypeQuals quals = mapMaybe select [(constant,CConstQual ni),(volatile,CVolatQual ni),(restrict,CRestrQual ni)]
    where
    select (predicate,tyqual) | predicate quals = Just tyqual
                              | otherwise       = Nothing

exportTypeQualsAttrs :: TypeQuals -> Attributes -> [CTypeQual]
exportTypeQualsAttrs tyqs attrs = (exportTypeQuals tyqs ++ map CAttrQual (exportAttrs attrs))

exportArraySize :: ArraySize -> CArrSize
exportArraySize (ArraySize static e) = CArrSize static e
exportArraySize (UnknownArraySize complete) = CNoArrSize complete

exportTypeSpec :: TypeName -> [CTypeSpec]
exportTypeSpec tyname =
    case tyname of
        TyVoid -> [CVoidType ni]
        TyIntegral ity -> exportIntType ity
        TyFloating fty -> exportFloatType fty
        TyComplex fty -> exportComplexType fty
        TyComp comp -> exportCompTypeDecl comp
        TyEnum enum -> exportEnumTypeDecl enum
        TyBuiltin TyVaList -> [CTypeDef (internalIdent "va_list") ni]

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

exportFloatType :: FloatType -> [CTypeSpec]
exportFloatType ty =
    case ty of
      TyFloat   -> [CFloatType ni]
      TyDouble  -> [CDoubleType ni]
      TyLDouble -> [CLongType ni, CDoubleType ni]

exportComplexType :: FloatType -> [CTypeSpec]
exportComplexType ty = (CComplexType ni) : exportFloatType ty

exportCompTypeDecl :: CompTypeRef -> [CTypeSpec]
exportCompTypeDecl ty = [CSUType (exportComp ty) ni]
    where
    exportComp (CompTypeRef sue_ref comp_tag _n) =
        CStruct (if comp_tag == StructTag then CStructTag else CUnionTag)
                (exportSUERef sue_ref) Nothing [] ni

exportEnumTypeDecl :: EnumTypeRef -> [CTypeSpec]
exportEnumTypeDecl ty = [CEnumType (exportEnum ty) ni]
    where
    exportEnum (EnumTypeRef sue_ref _n) =
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
exportCompTypeRef (CompType sue_ref com_tag  _ _ node_info) = exportCompTypeDecl (CompTypeRef sue_ref com_tag node_info)

exportEnumTypeRef :: EnumType -> [CTypeSpec]
exportEnumTypeRef (EnumType sue_ref _ _ node_info) = exportEnumTypeDecl (EnumTypeRef sue_ref node_info)

exportSUERef :: SUERef -> Maybe Ident
exportSUERef = Just . internalIdent . show -- relies on a the source program not having any $'s in it

exportMemberDecl :: MemberDecl -> CDecl
exportMemberDecl (AnonBitField ty expr node_info) =
    CDecl (map CTypeSpec $ exportTypeSpec $ fromDirectType ty) [(Nothing,Nothing,Just expr)] node_info
exportMemberDecl (MemberDecl vardecl bitfieldsz node_info) =
    let (specs,declarator) = exportVarDecl vardecl
    in  CDecl specs [(Just declarator, Nothing, bitfieldsz)] node_info
exportVarDecl :: VarDecl -> ([CDeclSpec],CDeclr)

-- NOTE: that there is an ambiguity between two possible places for __attributes__ s here
exportVarDecl (VarDecl name attrs ty) = exportDeclr (exportDeclAttrs attrs) ty [] name
exportParamDecl :: ParamDecl -> CDecl
exportParamDecl paramdecl = 
    let (specs,declr) = exportVarDecl (getVarDecl paramdecl)
    in CDecl specs [(Just declr, Nothing , Nothing) ] (nodeInfo paramdecl)

exportDeclAttrs :: DeclAttrs -> [CDeclSpec]
exportDeclAttrs (DeclAttrs inline storage attrs) =
       (if inline then [CTypeQual (CInlineQual ni)] else [])
    ++ map (CStorageSpec) (exportStorage storage)
    ++ map (CTypeQual . CAttrQual) (exportAttrs attrs)

-- | express storage in terms of storage specifiers.
--
-- This isn't always possible and depends on the context the identifier is declared.
-- Most importantly, if there is a /conflicting/ declaration in scope, export is impossible.
-- Furthermore, automatic storage is impossible in file scope.
-- If the storage can actually be specified, the export is correct.
exportStorage :: Storage -> [CStorageSpec]
exportStorage NoStorage = []
exportStorage (Auto reg) = if reg then [CRegister ni] else []
exportStorage (Static InternalLinkage thread_local) = threadLocal thread_local [CStatic ni]
exportStorage (Static ExternalLinkage thread_local) = threadLocal thread_local [CExtern ni]
exportStorage (FunLinkage ExternalLinkage) = []
exportStorage (FunLinkage InternalLinkage) = [CStatic ni]

threadLocal :: Bool -> [CStorageSpec] -> [CStorageSpec]
threadLocal False = id
threadLocal True = ((CThread ni) :)

exportAttrs :: [Attr] -> [CAttr]
exportAttrs = map exportAttr where
    exportAttr (Attr ident es n) = CAttr ident es n

fromDirectType :: Type -> TypeName
fromDirectType (DirectType ty _) = ty
fromDirectType (TypeDefType (TypeDefRef _ ref _)) = maybe (error "undefined typeDef") fromDirectType ref
fromDirectType _ = error "fromDirectType"

ni :: NodeInfo
ni = internalNode
