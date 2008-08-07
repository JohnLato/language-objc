{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis.DeclAnalysis
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  experimental
-- Portability :  unspecified
--
-- This module performs the analysis of declarations and the translation of
-- type specifications in the AST.
-----------------------------------------------------------------------------
module Language.C.Analysis.DeclAnalysis (
  -- * translating types
  analyseTypeDecl,
  tType,tDirectType,tNumType,tArraySize,tTypeQuals,
  mergeOldStyle,
  -- * dissecting type specs
  canonicalTypeSpec, NumBaseType(..),SignSpec(..),SizeMod(..),NumTypeSpec(..),TypeSpecAnalysis(..),
  canonicalStorageSpec, StorageSpec(..),isThreadLocalSpec,
  -- * helpers
  VarDeclInfo(..),
  tAttr,mkVarName,getOnlyDeclr,nameOfDecl,convertStringLit,
)
where
import Language.C.Data.Error
import Language.C.Data.Node
import Language.C.Data.Ident
import Language.C.Syntax
import Language.C.Analysis.SemError
import Language.C.Analysis.SemRep
import Language.C.Analysis.TravMonad

import Data.Foldable as F (foldrM)
import qualified Data.Traversable as T
import Control.Monad (liftM,when,ap)
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map

-- TODO: register or no storage
computeParamStorage _ = NoStorage

-- * handling declarations

-- TODO: handle void parameter ?
tParamDecl :: (MonadTrav m) => CDecl -> m ParamDecl
tParamDecl (CDecl declspecs declrs node) =
    case declrs' of
        [(Just declr, Nothing, Nothing)] ->
            do  (VarDeclInfo name is_inline storage_spec attrs ty declr_node) <- analyseVarDecl True declspecs declr [] Nothing

                -- TODO: check assembler name
                when (is_inline) $ astError node "parameter declaration with inline specifier"
                -- TODO: compute storage of parameter declaration
                let storage = computeParamStorage storage_spec
                return $ ParamDecl (VarDecl name (DeclAttrs False storage attrs) ty) declr_node
        _   -> astError node "bad parameter declaration: multiple decls / bitfield or initializer present"
    where
    -- hacking away missing declarator
    declrs' = case declrs of
                  [] -> [(Just$ emptyDeclr node,Nothing,Nothing)]
                  _  -> declrs
tMemberDecls :: (MonadTrav m) => CDecl -> m [MemberDecl]
tMemberDecls (CDecl declspecs declrs node) = mapM (uncurry tMemberDecl) (zip (True:repeat False) declrs)
    where
    tMemberDecl handle_sue_def (Just member_declr,Nothing,bit_field_size_opt) =
        do var_decl <- analyseVarDecl handle_sue_def declspecs member_declr [] Nothing
           let (VarDeclInfo name is_inline storage_spec attrs ty declr_node) = var_decl
           --
           checkValidMemberSpec is_inline storage_spec
           return $ MemberDecl (VarDecl name (DeclAttrs False NoStorage attrs) ty) bit_field_size_opt node
    tMemberDecl handle_sue_def (Nothing,Nothing,Just bit_field_size) =
        do let (storage_specs, _attrs, typequals, typespecs, is_inline) = partitionDeclSpecs declspecs
           storage_spec  <- canonicalStorageSpec storage_specs
           typ           <- tType handle_sue_def node typequals typespecs [] []
           --
           return $ AnonBitField typ bit_field_size node
    tMemberDecl _ _ = astError node "Bad member declaration"
    checkValidMemberSpec is_inline storage_spec =
        do  when (is_inline)                     $ astError node "member declaration with inline specifier"
            when (storage_spec /= NoStorageSpec) $ astError node "storage specifier for member"
            return ()

data StorageSpec = NoStorageSpec | AutoSpec | RegSpec | ThreadSpec | StaticSpec Bool | ExternSpec Bool
                    deriving (Eq,Ord,Show,Read)
isThreadLocalSpec :: StorageSpec -> Bool
isThreadLocalSpec ThreadSpec = True
isThreadLocalSpec (StaticSpec b) = b
isThreadLocalSpec (ExternSpec b) = b
isThreadLocalSpec _  = False

data VarDeclInfo = VarDeclInfo VarName Bool {- is-inline? -} StorageSpec Attributes Type NodeInfo

-- | analyse declarators
analyseVarDecl :: (MonadTrav m) => Bool -> [CDeclSpec] -> CDeclr -> [CDecl] -> (Maybe CInit) -> m VarDeclInfo
analyseVarDecl handle_sue_def declspecs
               (CDeclr name_opt derived_declrs asmname_opt declr_attrs node)
               oldstyle_params init_opt
    = do let (storage_specs, decl_attrs, typequals, typespecs, inline) = partitionDeclSpecs declspecs
         -- analyse the storage specifiers
         storage_spec  <- canonicalStorageSpec storage_specs
         -- translate the type into semantic representation
         typ          <- tType handle_sue_def node typequals typespecs derived_declrs oldstyle_params
         -- translate attributes
         attrs'       <- mapM tAttr (decl_attrs ++ declr_attrs)
         -- make name
         name         <- mkVarName node name_opt (fmap convertStringLit asmname_opt)
         return $ VarDeclInfo name inline storage_spec attrs' typ node
    where
        isInlineSpec (CInlineQual _) = True
        isInlineSpec _ = False


-- return @True@ if the declarations is a type def
isTypeDef :: [CDeclSpec] -> Bool
isTypeDef declspecs = not $ null [ n | (CStorageSpec (CTypedef n)) <- declspecs ]


-- | analysis of constant expressions
--   see C99 6.6
--  /FIXME/: stub
analyseConstExpr :: (MonadTrav m) => CExpr -> m Expr
analyseConstExpr = return

-- * translation

-- | get the type of a /type declaration/
--
-- A type declaration @T@ may appear in thre forms:
--
--  * @typeof(T)@
--
--  * as abstract declarator in a function prototype, as in @f(int)@
--
--  * in a declaration without declarators, as in @struct x { int a } ;@
--
-- Currently, @analyseTypeDecl@ is exlusively used for analysing types for GNU's @typeof(T)@.
--
-- We move attributes to the type, as they have no meaning for the abstract declarator
analyseTypeDecl :: (MonadTrav m) => CDecl -> m Type
analyseTypeDecl (CDecl declspecs declrs node)
    | [] <- declrs = analyseTyDeclr (emptyDeclr node)
    | [(Just declr,Nothing,Nothing)] <- declrs = analyseTyDeclr declr
    | otherwise = astError node "Bad declarator for type declaration"
    where
    analyseTyDeclr (CDeclr Nothing derived_declrs Nothing attrs _declrnode)
        | (not (null storagespec) || inline) = astError node "storage specifier for type declaration"
        | otherwise                          = tType True node (map CAttrQual (attrs++attrs_decl) ++ typequals)
                                                     typespecs derived_declrs []
        where
        (storagespec, attrs_decl, typequals, typespecs, inline) = partitionDeclSpecs declspecs
    analyseTyDeclr _ = astError node "Non-abstract declarator in type declaration"


-- | translate a type
tType :: (MonadTrav m) => Bool -> NodeInfo -> [CTypeQual] -> [CTypeSpec] -> [CDerivedDeclr] -> [CDecl] -> m Type
tType handle_sue_def top_node typequals typespecs derived_declrs oldstyle_params
    = mergeOldStyle top_node oldstyle_params derived_declrs >>= buildType
    where
    buildType [] = tDirectType handle_sue_def top_node typequals typespecs
    buildType (CPtrDeclr ptrquals node : dds) =
        buildType dds >>= buildPointerType ptrquals node
    buildType (CArrDeclr arrquals size node : dds)
        = buildType dds >>= buildArrayType arrquals size node
    buildType (CFunDeclr ~(Right (params, isVariadic)) attrs node : dds)
        = buildType dds >>= (liftM FunctionType . buildFunctionType params isVariadic attrs node)
    buildPointerType ptrquals _node inner_ty
        = liftM (\(quals,attrs) -> PtrType inner_ty quals attrs) (tTypeQuals ptrquals)
    buildArrayType arr_quals size _node inner_ty
        = do (quals,attrs) <- tTypeQuals arr_quals
             arr_sz        <- tArraySize size
             return$ ArrayType inner_ty arr_sz quals attrs
    buildFunctionType params is_variadic attrs _node return_ty
        = do params' <- mapM tParamDecl params
             attrs'   <- mapM tAttr attrs
             return$ FunType return_ty params' is_variadic attrs'

-- | translate a type without (syntactic) indirections
-- Due to the GNU @typeof@ extension and typeDefs, this can be an arbitrary type
tDirectType :: (MonadTrav m) => Bool -> NodeInfo -> [CTypeQual] -> [CTypeSpec] -> m Type
tDirectType handle_sue_def node ty_quals ty_specs = do
    (quals,attrs) <- tTypeQuals ty_quals
    canonTySpec <- canonicalTypeSpec ty_specs
    let baseType ty_name = DirectType ty_name quals
    case canonTySpec of
        TSNone -> return$ baseType (TyIntegral TyInt)
        TSVoid -> return$ baseType TyVoid
        TSBool -> return$ baseType (TyIntegral TyBool)
        TSNum tsnum -> do
            numType <- tNumType tsnum
            return . baseType $
                case numType of
                    Left (floatType,iscomplex) | iscomplex -> TyComplex floatType
                                               | otherwise -> TyFloating floatType
                    Right intType  -> TyIntegral intType
        TSNonBasic (CSUType su _tnode)      -> liftM (baseType . TyComp) $ tCompTypeDecl handle_sue_def su
        TSNonBasic (CEnumType enum _tnode)   -> liftM (baseType . TyEnum) $ tEnumTypeDecl handle_sue_def enum
        TSNonBasic (CTypeDef name t_node)    -> liftM TypeDefType $ typeDefRef t_node name
        TSNonBasic (CTypeOfExpr expr _tnode) -> liftM (TypeOfExpr) (analyseConstExpr expr)
        TSNonBasic (CTypeOfType decl t_node) ->  analyseTypeDecl decl >>= mergeTypeAttributes t_node quals attrs
        TSNonBasic _ -> astError node "Unexpected typespec"

-- | Merge type attributes
--
-- This handles for example the form
--
-- > /* tyqual attr typeof(type) */
-- > const typeof(char volatile) x;
mergeTypeAttributes :: (MonadTrav m) => NodeInfo -> TypeQuals -> [Attr] -> Type -> m Type
mergeTypeAttributes node_info quals attrs typ =
    case typ of
        DirectType ty_name quals' -> merge quals' [] $ mkDirect ty_name
        PtrType ty quals' attrs'  -> merge quals' attrs' $ PtrType ty
        ArrayType ty array_sz quals' attrs' -> merge quals' attrs' $ ArrayType ty array_sz
        FunctionType (FunType return_ty params inline attrs')
            | not (null attrs) -> astError node_info "type qualifiers for function type"
            | otherwise        -> return$ FunctionType (FunType return_ty params inline (attrs' ++ attrs))
    where
    mkDirect ty_name quals' attrs' | null attrs' = DirectType ty_name quals'
                                   | otherwise   = error "_attribute__ s for DirectType"
    merge quals' attrs' tyf = return $ tyf (mergeTypeQuals quals quals') (attrs' ++ attrs)

typeDefRef :: (MonadTrav m) => NodeInfo -> Ident -> m TypeDefRef
typeDefRef t_node name = lookupTypeDef name >>= \ty -> return (TypeDefRef name (Just ty) t_node)

-- extract a struct\/union
-- we emit @declStructUnion@ and @defStructUnion@ actions
--
-- TODO: should attributes be part of declarartions too ?
tCompTypeDecl :: (MonadTrav m) => Bool -> CStructUnion -> m CompTypeRef
tCompTypeDecl handle_def (CStruct tag ident_opt member_decls_opt attrs node_info) = do
    sue_ref <- createSUERef node_info ident_opt                           -- create name
    let tag' = tTag tag
    attrs' <- mapM tAttr attrs
    let decl = CompTypeRef sue_ref tag' node_info
    when (handle_def) $ do
        maybeM member_decls_opt $ \decls ->
                tCompType sue_ref tag' decls (attrs') node_info
            >>= (handleTagDef.CompDef)                                      -- handle comp type definition
    return decl

tTag :: CStructTag -> CompTyKind
tTag CStructTag = StructTag
tTag CUnionTag  = UnionTag

tCompType :: (MonadTrav m) => SUERef -> CompTyKind -> [CDecl] -> Attributes -> NodeInfo -> m CompType
tCompType tag sue_ref member_decls attrs node
    = return (CompType tag sue_ref) `ap`
        (concatMapM tMemberDecls member_decls) `ap`
        (return attrs) `ap`
        (return node)

-- | translate a enum type decl
--
--  > enum my_enum
--  > enum your_enum { x, y=3 }
--
tEnumTypeDecl :: (MonadTrav m) => Bool -> CEnum -> m EnumTypeRef
tEnumTypeDecl handle_def (CEnum ident_opt enumerators_opt attrs node_info)
    | (Nothing, Nothing) <- (ident_opt, enumerators_opt) = astError node_info "both definition and name of enum missing"
    | Just [] <- enumerators_opt                         = astError node_info "empty enumerator list"
    | otherwise
        = do sue_ref <- createSUERef node_info ident_opt
             attrs' <- mapM tAttr attrs
             let decl = EnumTypeRef sue_ref node_info
             when handle_def $ do
                 maybeM enumerators_opt $ \enumerators ->
                         tEnumType sue_ref enumerators attrs' node_info
                    >>=  (handleTagDef . EnumDef)
             return decl

tEnumType :: (MonadTrav m) => SUERef -> [(Ident, Maybe CExpr)] -> Attributes -> NodeInfo -> m EnumType
tEnumType sue_ref enumerators attrs node = do
    enumerators' <- mapM (mapSndM (T.mapM analyseConstExpr)) enumerators
    let ty = EnumType sue_ref enumerators' attrs node
    mapM_ (flip handleEnumeratorDef ty) enumerators'
    return ty


-- | Mapping from num type specs to C types (C99 6.7.2-2), ignoring the complex qualifier
tNumType :: (MonadTrav m) => NumTypeSpec -> m (Either (FloatType,Bool) IntType)
tNumType (NumTypeSpec basetype sgn sz iscomplex) =
    case (basetype,sgn,sz) of
        (BaseChar,_,NoSizeMod)      | Signed <- sgn   -> intType TySChar
                                      | Unsigned <- sgn -> intType TyUChar
                                      | otherwise       -> intType TyChar
        (intbase, _, NoSizeMod)  | optBase BaseInt intbase ->
            intType$ case sgn of
                            Unsigned -> TyUInt
                            _        -> TyInt
        (intbase, signed, sizemod)    | optBase BaseInt intbase, optSign Signed signed ->
            intType$ case sizemod of ShortMod    -> TyShort
                                     LongMod     -> TyLong
                                     LongLongMod -> TyLLong
                                     _ -> internalErr "numTypeMapping: unexpected pattern matching error"
        (intbase, Unsigned, sizemod) | optBase BaseInt intbase ->
            intType$ case sizemod of ShortMod    -> TyUShort
                                     LongMod     -> TyULong
                                     LongLongMod -> TyULLong
                                     _ -> internalErr "numTypeMapping: unexpected pattern matching error"
        (BaseFloat, NoSignSpec, NoSizeMod)  -> floatType TyFloat
        (BaseDouble, NoSignSpec, NoSizeMod) -> floatType TyDouble
        (BaseDouble, NoSignSpec, LongMod)   -> floatType TyLDouble
        -- TODO: error analysis
        (_,_,_)   -> error "Bad AST analysis"
    where
    optBase _ NoBaseType = True
    optBase expect baseTy = expect == baseTy
    optSign _ NoSignSpec = True
    optSign expect sign = expect == sign
    intType = return . Right
    floatType ft = return (Left (ft,iscomplex))

-- TODO: currently bogus
tArraySize :: (MonadTrav m) => CArrSize -> m ArraySize
tArraySize (CNoArrSize False) = return (UnknownArraySize False)
tArraySize (CNoArrSize True) = return (UnknownArraySize True)
tArraySize (CArrSize static szexpr) = liftM (ArraySize static) (return szexpr)

tTypeQuals :: (MonadTrav m) => [CTypeQual] -> m (TypeQuals,Attributes)
tTypeQuals = foldrM go (noTypeQuals,[]) where
    go (CConstQual _) (tq,attrs) = return$ (tq { constant = True },attrs)
    go (CVolatQual _) (tq,attrs) = return$ (tq { volatile = True },attrs)
    go (CRestrQual _) (tq,attrs) = return$ (tq { restrict = True },attrs)
    go (CAttrQual attr) (tq,attrs) = liftM (\attr' -> (tq,attr':attrs)) (tAttr attr)
    go (CInlineQual node) (_tq,_attrs) = astError node "unexpected inline qualifier"


-- * analysis


{-
To canoicalize type specifiers, we define a canonical form:
void | bool | (char|int|float|double) (signed|unsigned)? (long long?)? complex? | othertype
-}
data NumBaseType = NoBaseType | BaseChar | BaseInt | BaseFloat | BaseDouble deriving (Eq,Ord)
data SignSpec    = NoSignSpec | Signed | Unsigned deriving (Eq,Ord)
data SizeMod     = NoSizeMod | ShortMod | LongMod | LongLongMod deriving (Eq,Ord)
data NumTypeSpec = NumTypeSpec { base :: NumBaseType, signSpec :: SignSpec, sizeMod :: SizeMod, isComplex :: Bool  }
emptyNumTypeSpec :: NumTypeSpec
emptyNumTypeSpec = NumTypeSpec { base = NoBaseType, signSpec = NoSignSpec, sizeMod = NoSizeMod, isComplex = False }
data TypeSpecAnalysis = TSNone | TSVoid | TSBool | TSNum NumTypeSpec | TSNonBasic CTypeSpec

canonicalTypeSpec :: (MonadTrav m) => [CTypeSpec] -> m TypeSpecAnalysis
canonicalTypeSpec = foldrM go TSNone where
    getNTS TSNone = Just emptyNumTypeSpec
    getNTS (TSNum nts) = Just nts
    getNTS _ = Nothing
    updLongMod NoSizeMod = Just LongMod
    updLongMod LongMod   = Just LongLongMod
    updLongMod _         = Nothing
    go (CVoidType _)    TSNone = return$  TSVoid
    go (CBoolType _)    TSNone = return$  TSBool
    go (CCharType _)    tsa | (Just nts@(NumTypeSpec { base = NoBaseType })) <- getNTS tsa
                            = return$  TSNum$ nts { base = BaseChar }
    go (CIntType _)     tsa | (Just nts@(NumTypeSpec { base = NoBaseType })) <- getNTS tsa
                            = return$  TSNum$ nts { base = BaseInt }
    go (CFloatType _)   tsa | (Just nts@(NumTypeSpec { base = NoBaseType })) <- getNTS tsa
                            = return$  TSNum$ nts { base = BaseFloat }
    go (CDoubleType _)  tsa | (Just nts@(NumTypeSpec { base = NoBaseType })) <- getNTS tsa
                            = return$  TSNum$ nts { base = BaseDouble }
    go (CShortType _)   tsa | (Just nts@(NumTypeSpec { sizeMod = NoSizeMod })) <- getNTS tsa
                            = return$  TSNum$nts { sizeMod = ShortMod }
    go (CLongType _)    tsa | (Just nts@(NumTypeSpec { sizeMod = szMod })) <- getNTS tsa,
                              (Just szMod') <- updLongMod szMod
                            = return$  TSNum$ nts { sizeMod = szMod' }
    go (CSignedType _)  tsa | (Just nts@(NumTypeSpec { signSpec = NoSignSpec })) <- getNTS tsa
                            = return$  TSNum$ nts { signSpec = Signed }
    go (CUnsigType _)   tsa | (Just nts@(NumTypeSpec { signSpec = NoSignSpec })) <- getNTS tsa
                            = return$  TSNum$ nts { signSpec = Unsigned }
    go (CComplexType _) tsa | (Just nts@(NumTypeSpec { isComplex = False })) <- getNTS tsa
                            = return$  TSNum$ nts { isComplex = True }
    go otherType  TSNone    = return$  TSNonBasic otherType
    go ty _ts = astError (nodeInfo ty) "Invalid type specifier"

-- compute storage given storage specifiers
canonicalStorageSpec :: (MonadTrav m) =>[CStorageSpec] -> m StorageSpec
canonicalStorageSpec storagespecs = liftM elideAuto $ foldrM updStorage NoStorageSpec storagespecs where
        updStorage (CAuto _) NoStorageSpec     = return$ AutoSpec
        updStorage (CRegister _) NoStorageSpec = return$ RegSpec
        updStorage (CThread _) NoStorageSpec   = return$ ThreadSpec
        updStorage (CThread _) (StaticSpec _)  = return$ StaticSpec True
        updStorage (CThread _) (ExternSpec _)  = return$ ExternSpec True
        updStorage (CStatic _) NoStorageSpec   = return$ StaticSpec False
        updStorage (CExtern _) NoStorageSpec   = return$ ExternSpec False
        updStorage (CStatic _) ThreadSpec      = return$ StaticSpec True
        updStorage (CExtern _) ThreadSpec      = return$ ExternSpec True
        updStorage badSpec old
            = astError (nodeInfo badSpec) $ "Invalid storage specifier "++show badSpec++" in combination with "++show old
        elideAuto AutoSpec = NoStorageSpec
        elideAuto spec = spec

-- | convert old style parameters
--
-- This requires matching parameter names and declarations, as in the following example:
--
-- > int f(d,c,a,b)
-- > char a,*b;
-- > int c;
-- > { }
--
-- is converted to
--
-- > int f(int d, int c, char a, char* b)
--
-- TODO: This could be moved to syntax, as it operates on the AST only
mergeOldStyle :: (MonadTrav m) => NodeInfo -> [CDecl] -> [CDerivedDeclr] -> m [CDerivedDeclr]
mergeOldStyle _node [] declrs = return declrs
mergeOldStyle node oldstyle_params (CFunDeclr params attrs fdnode : dds) =
    case params of
        Left list -> do
            -- FIXME: This translation doesn't work in the following example
            -- [| int f(b,a) struct x { }; int b,a; { struct x local; return local.x } |]
            oldstyle_params' <- liftM concat $ mapM splitCDecl oldstyle_params
            param_map <- liftM Map.fromList $ mapM attachNameOfDecl oldstyle_params'
            (newstyle_params,param_map') <- foldrM insertParamDecl ([],param_map) list
            when (not $ Map.null param_map') $
                astError node $ "declarations for parameter(s) "++ showParamMap param_map' ++" but no such parameter"
            return $ (CFunDeclr (Right (newstyle_params, False)) attrs fdnode : dds)
        Right _newstyle -> astError node "oldstyle parameter list, but newstyle function declaration"
    where
        attachNameOfDecl decl = nameOfDecl decl >>= \n -> return (n,decl)
        insertParamDecl param_name (ps, param_map)
            = case Map.lookup param_name param_map of
                Just p -> return (p:ps, Map.delete param_name param_map)
                Nothing -> return (implicitIntParam param_name : ps, param_map)
        implicitIntParam param_name =
            let node = (nodeInfo param_name) in
            CDecl [CTypeSpec (CIntType node)] [(Just (CDeclr (Just param_name) [] Nothing [] node),Nothing,Nothing)] node
        showParamMap = concat . intersperse ", " . map identToString . Map.keys
mergeOldStyle node _ _ = astError node "oldstyle parameter list, but not function type"

-- | split a CDecl into declarators, hereby eliding SUE defs from the second declarator on.
--
--   There are several reasons why this isn;t the preferred way for handling multiple-declarator declarations,
--   but it can be convinient some times.
--
-- > splitCDecl [d| struct x { int z; } a,*b; |]
-- > [ [d| struct x { int z; } a, struct x *b |] ]
--
-- /TODO/: This could be moved to syntax, as it operates on the AST only
splitCDecl :: (MonadTrav m) => CDecl -> m [CDecl]
splitCDecl decl@(CDecl declspecs declrs node) =
    case declrs of
        []      -> internalErr "splitCDecl applied to empty declaration"
        [declr] -> return [decl]
        (d1:ds) ->
            let declspecs' = map elideSUEDef declspecs in
            return$ (CDecl declspecs [d1] node) : [ CDecl declspecs' [declr] node | declr <- ds ]
    where
    elideSUEDef declspec@(CTypeSpec tyspec) =
        case tyspec of
            (CEnumType (CEnum name def attrs enum_node) node) ->
                CTypeSpec (CEnumType (CEnum name Nothing [] enum_node) node)
            (CSUType (CStruct tag name def attrs su_node) node) ->
                CTypeSpec (CSUType (CStruct tag name Nothing [] su_node) node)
            _ -> declspec
    elideSUEDef declspec = declspec


-- translate __attribute__ annotations
-- TODO: bogus
tAttr :: (MonadTrav m) => CAttr -> m Attr
tAttr (CAttr name cexpr node) = liftM (\e -> Attr name e node) $ mapM analyseConstExpr cexpr

-- convert string literals
-- TODO: bogus
convertStringLit :: CStrLit -> CStrLit
convertStringLit = id

-- | construct a name for a variable
-- TODO: more or less bogus
mkVarName :: (MonadTrav m) => NodeInfo -> Maybe Ident -> Maybe AsmName -> m VarName
mkVarName  node Nothing _ = return NoName
mkVarName  node (Just n) asm = return $ VarName n Nothing

-- helpers
nameOfDecl :: (MonadTrav m) => CDecl -> m Ident
nameOfDecl d = getOnlyDeclr d >>= \declr ->
    case declr of
        (CDeclr (Just name) _ _ _ _) -> return name
        (CDeclr Nothing _ _ _ node) -> internalErr "nameOfDecl: abstract declarator"
emptyDeclr :: NodeInfo -> CDeclr
emptyDeclr node = CDeclr Nothing [] Nothing [] node
getOnlyDeclr :: (MonadTrav m) => CDecl -> m CDeclr
getOnlyDeclr (CDecl _ [(Just declr,_,_)] _) = return declr
getOnlyDeclr (CDecl _ _ node) = internalErr "getOnlyDeclr: declaration doesn't have a unique declarator"
