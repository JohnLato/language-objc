{-# LANGUAGE FlexibleInstances #-}
module Language.C.Analysis.TypeCheck where

import Control.Monad
import Data.Either
import Data.Maybe
import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Pretty
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Syntax.Ops
import Language.C.Analysis.Debug
import Language.C.Analysis.DefTable
import Language.C.Analysis.SemRep
import Language.C.Analysis.TravMonad
import Language.C.Analysis.TypeConversions
import Language.C.Analysis.TypeUtils
import Text.PrettyPrint.HughesPJ

instance Monad (Either String) where
    return        = Right
    Left  l >>= _ = Left l
    Right r >>= k = k r
    fail msg      = Left msg

pType :: Type -> String
pType = render . pretty

typeErrorOnLeft :: (MonadTrav m) => NodeInfo -> Either String a -> m a
typeErrorOnLeft ni (Left err) = typeError ni err
typeErrorOnLeft _  (Right v)  = return v

-- XXX: this should use a custom error type, but typeMismatch isn't always right
typeError :: MonadTrav m => NodeInfo -> String -> m a
typeError = astError

notFound :: Ident -> Either String a
notFound i = fail $ "not found: " ++ identToString i

checkScalar' :: MonadTrav m => NodeInfo -> Type -> m ()
checkScalar' ni = typeErrorOnLeft ni . checkScalar

checkIntegral' :: MonadTrav m => NodeInfo -> Type -> m ()
checkIntegral' ni = typeErrorOnLeft ni . checkIntegral

assignCompatible' :: MonadTrav m =>
                     NodeInfo -> CAssignOp -> Type -> Type -> m ()
assignCompatible' ni op t1 t2 = typeErrorOnLeft ni (assignCompatible op t1 t2)

binopType' :: MonadTrav m =>
                     NodeInfo -> CBinaryOp -> Type -> Type -> m Type
binopType' ni op t1 t2 = typeErrorOnLeft ni (binopType op t1 t2)

conditionalType' :: MonadTrav m => NodeInfo -> Type -> Type -> m Type
conditionalType' ni t1 t2 = typeErrorOnLeft ni $ conditionalType t1 t2

checkScalar :: Type -> Either String ()
checkScalar t =
  case canonicalType t of
    DirectType _ _    -> return ()
    PtrType _ _ _     -> return ()
    ArrayType _ _ _ _ -> return () -- because it's just a pointer
    t' -> fail $
          "expected scalar type, got: "
          ++ pType t ++ " (" ++ pType t' ++ ")"

checkIntegral :: Type -> Either String ()
checkIntegral t | isIntegralType (canonicalType t) = return ()
                | otherwise = fail $
                              "expected integral type, got: " ++
                              pType t ++ " (" ++
                              pType (canonicalType t) ++ ")"

handleArray :: Type -> Type
handleArray (ArrayType bt _ q a) = PtrType bt q a -- XXX: q and a correct?
handleArray t = t


-- | Determine the type of a constant.
constType :: MonadTrav m => CConst -> m Type
constType (CIntConst (CInteger _ _ flags) _) =
  return $ DirectType (TyIntegral (getIntType flags)) noTypeQuals
constType (CCharConst (CChar _ True) _) =
  return $ DirectType (TyIntegral TyInt) noTypeQuals
constType (CCharConst (CChar _ False) _) =
  return $ DirectType (TyIntegral TyChar) noTypeQuals
constType (CCharConst (CChars _ _) _)  =
  return $ DirectType (TyIntegral TyInt) noTypeQuals -- XXX
constType (CFloatConst (CFloat fs) _) =
  return $ DirectType (TyFloating (getFloatType fs)) noTypeQuals
-- XXX: should strings have any type qualifiers or attributes?
constType (CStrConst (CString chars wide) ni) =
  do n <- genName
     let charType | wide      = TyInt -- XXX: this isn't universal
                  | otherwise = TyChar
         ni' = mkNodeInfo (posOf ni) n
         arraySize = ArraySize
                     True -- XXX: is it static?
                     (CConst
                      (CIntConst
                       (cInteger (toInteger (length chars))) ni'))
     return $ ArrayType (DirectType (TyIntegral charType) noTypeQuals)
                        arraySize noTypeQuals []

-- | Determine whether two types are compatible.
compatible :: Type -> Type -> Either String ()
compatible t1 t2 = compositeType t1 t2 >> return ()

-- | Determine the composite type of two compatible types.
compositeType :: Type -> Type -> Either String Type
compositeType t1 (DirectType (TyBuiltin TyAny) _) = return t1
compositeType (DirectType (TyBuiltin TyAny) _) t2 = return t2
compositeType t1@(DirectType tn1 q1) t2@(DirectType tn2 q2) =
  do tn <- case (tn1, tn2) of
             (TyVoid, TyVoid) -> return TyVoid
             (TyIntegral _, TyEnum _) -> return tn1
             (TyEnum _, TyIntegral _) -> return tn2
             (TyIntegral i1, TyIntegral i2) ->
               return $ TyIntegral (intConversion i1 i2)
             (TyFloating f1, TyFloating f2) ->
               return $ TyFloating (floatConversion f1 f2)
             (TyComplex f1, TyComplex f2) ->
               return $ TyComplex (floatConversion f1 f2)
             (TyComp c1, TyComp c2) ->
               do when (sueRef c1 /= sueRef c2) $
                       fail $ "incompatible composite types: "
                              ++ pType t1 ++ ", " ++ pType t2
                  return tn1
             (TyEnum e1, TyEnum e2) ->
               do when (sueRef e1 /= sueRef e2) $
                       fail $ "incompatible enumeration types: "
                              ++ pType t1 ++ ", " ++ pType t2
                  return $ TyEnum e1
             (TyBuiltin TyVaList, TyBuiltin TyVaList) ->
               return $ TyBuiltin TyVaList
             (TyBuiltin _, TyBuiltin _) ->
               fail $ "incompatible builtin types: "
                      ++ pType t1 ++ ", " ++ pType t2
             (_, _) -> fail $ "incompatible direct types: "
                       ++ pType t1 ++ ", " ++ pType t2
     return $ DirectType tn (mergeTypeQuals q1 q2)
compositeType (PtrType t1 q1 a1) t2 | isIntegralType t2 =
  return $ PtrType t1 (mergeTypeQuals q1 (typeQuals t2)) a1
compositeType t1 (PtrType t2 q2 a2) | isIntegralType t1 =
  return $ PtrType t2 (mergeTypeQuals (typeQuals t1) q2) a2
compositeType (ArrayType t1 s1 q1 a1) (ArrayType t2 s2 q2 a2) =
  do t <- compositeType t1 t2
     s <- compositeSize s1 s2
     let quals = mergeTypeQuals q1 q2
         attrs = mergeAttrs a1 a2
     return (ArrayType t s quals attrs)
compositeType t1 t2 | isPointerType t1 && isPointerType t2 =
  do t <- compositeType (baseType t1) (baseType t2)
     let quals = mergeTypeQuals (typeQuals t1) (typeQuals t2)
         attrs = mergeAttrs (typeAttrs t1) (typeAttrs t2)
     return (PtrType t quals attrs)
compositeType (TypeDefType tdr1) (TypeDefType tdr2) =
  case (tdr1, tdr2) of
    (TypeDefRef i1 Nothing _, TypeDefRef i2 _ _) -> doTypeDef i1 i2 tdr1
    (TypeDefRef i1 _ _, TypeDefRef i2 Nothing _) -> doTypeDef i1 i2 tdr2
    (TypeDefRef _ (Just t1) _, TypeDefRef _ (Just t2) _) ->
      compositeType t1 t2
  where doTypeDef i1 i2 tdr =
          do when (i1 /= i2) $ fail $ "incompatible typedef types: "
                               ++ identToString i1 ++ ", " ++ identToString i2
             return (TypeDefType tdr)
compositeType (FunctionType ft1) (FunctionType ft2) =
  case (ft1, ft2) of
    (FunType rt1 args1 varargs1 attrs1, FunType rt2 args2 varargs2 attrs2) ->
      do when (length args1 /= length args2) $
              fail "different numbers of arguments in function types"
         args <- mapM (uncurry compositeParamDecl) (zip args1 args2)
         when (varargs1 /= varargs2) $
              fail "incompatible varargs declarations"
         doFunType rt1 rt2 args varargs1 attrs1 attrs2
    (FunType rt1 args1 varargs1 attrs1, FunTypeIncomplete rt2 attrs2) ->
      doFunType rt1 rt2 args1 varargs1 attrs1 attrs2
    (FunTypeIncomplete rt1 attrs1, FunType rt2 args2 varargs2 attrs2) ->
      doFunType rt1 rt2 args2 varargs2 attrs1 attrs2
    (FunTypeIncomplete rt1 attrs1, FunTypeIncomplete rt2 attrs2) ->
      do rt <- compositeType rt1 rt2
         return (FunctionType (FunTypeIncomplete rt (mergeAttrs attrs1 attrs2)))
  where doFunType rt1 rt2 args varargs attrs1 attrs2 =
          do rt <- compositeType rt1 rt2
             return (FunctionType
                     (FunType rt args varargs (mergeAttrs attrs1 attrs2)))
compositeType t1 t2 = fail $ "incompatible types: "
                         ++ pType t1 ++ ", " ++ pType t2

-- XXX: this may not be correct
compositeSize :: ArraySize -> ArraySize -> Either String ArraySize
compositeSize (UnknownArraySize _) s2 = return s2
compositeSize s1 (UnknownArraySize _) = return s1
compositeSize (ArraySize s1 e1) (ArraySize s2 e2)
  | s1 == s2 && sizeEqual e1 e2 = return $ ArraySize s1 e1
  | otherwise =
    fail $ "incompatible array sizes: "
           ++ (render . pretty) e1 ++ ", " ++ (render . pretty) e2

sizeEqual :: CExpr -> CExpr -> Bool
sizeEqual (CConst (CIntConst i1 _)) (CConst (CIntConst i2 _)) = i1 == i2
sizeEqual e1 e2 = nodeInfo e1 == nodeInfo e2

mergeAttrs :: Attributes -> Attributes -> Attributes
mergeAttrs = (++) -- XXX: ultimately this should be smarter

compositeParamDecl :: ParamDecl -> ParamDecl -> Either String ParamDecl
compositeParamDecl (ParamDecl vd1 ni1) (ParamDecl vd2 _) =
  compositeParamDecl' ParamDecl vd1 vd2 ni1
compositeParamDecl (AbstractParamDecl vd1 _) (ParamDecl vd2 ni2) =
  compositeParamDecl' ParamDecl vd1 vd2 ni2
compositeParamDecl (ParamDecl vd1 ni1) (AbstractParamDecl vd2 _) =
  compositeParamDecl' ParamDecl vd1 vd2 ni1
compositeParamDecl (AbstractParamDecl vd1 ni1) (AbstractParamDecl vd2 _) =
  compositeParamDecl' AbstractParamDecl vd1 vd2 ni1

compositeParamDecl' :: (VarDecl -> NodeInfo -> ParamDecl)
                    -> VarDecl
                    -> VarDecl
                    -> NodeInfo
                    -> Either String ParamDecl
compositeParamDecl' f vd1 vd2 dni =
  do vd <- compositeVarDecl vd1 vd2
     return $ f vd dni

compositeVarDecl :: VarDecl -> VarDecl -> Either String VarDecl
compositeVarDecl (VarDecl n1 attrs1 t1) (VarDecl _ attrs2 t2) =
  do t <- compositeType t1 t2
     return (VarDecl n1 (compositeDeclAttrs attrs1 attrs2) t)

-- XXX: bad treatement of inline and storage
compositeDeclAttrs :: DeclAttrs -> DeclAttrs -> DeclAttrs
compositeDeclAttrs (DeclAttrs inl stor attrs1) (DeclAttrs _ _ attrs2) =
  DeclAttrs inl stor (mergeAttrs attrs1 attrs2)

castCompatible :: Type -> Type -> Either String ()
castCompatible t1 t2 =
  case (canonicalType t1, canonicalType t2) of
    (DirectType TyVoid _, _) -> return ()
    (_, _) -> checkScalar t1 >> checkScalar t2

-- | Determine whether two types are compatible in an assignment expression.
assignCompatible :: CAssignOp -> Type -> Type -> Either String ()
assignCompatible CAssignOp t1 t2 =
  case (canonicalType t1, canonicalType t2) of
    (DirectType (TyBuiltin TyAny) _, _) -> return ()
    (_, DirectType (TyBuiltin TyAny) _) -> return ()
    -- XXX: check qualifiers
    (PtrType (DirectType TyVoid _) _ _, t2') | isPointerType t2' -> return ()
    -- XXX: check qualifiers
    (t1', PtrType (DirectType TyVoid _) _ _) | isPointerType t1' -> return ()
    (PtrType _ _ _, t2') | isIntegralType t2' -> return ()
    (t1', t2') | isPointerType t1' && isPointerType t2' ->
                 do compatible (baseType t1') (baseType t2')
                --unless (typeQuals t2 <= typeQuals t1) $
                --       fail $
                --       "incompatible qualifiers in pointer assignment: "
                --       ++ pType t1 ++ ", " ++ pType t2
    (DirectType (TyComp c1) _, DirectType (TyComp c2) _)
      | sueRef c1 == sueRef c2 -> return ()
      | otherwise -> fail $
                     "incompatible compound types in assignment: "
                     ++ pType t1 ++ ", " ++ pType t2
    (DirectType (TyBuiltin TyVaList) _, DirectType (TyBuiltin TyVaList) _) ->
      return ()
    (DirectType tn1 _, DirectType tn2 _)
      | isJust (arithmeticConversion tn1 tn2) -> return ()
      | otherwise -> fail $ "incompatible direct types in assignment: "
                     ++ pType t1 ++ ", " ++ pType t2
    (t1', t2') -> compatible t1' t2'
assignCompatible op t1 t2 = binopType (assignBinop op) t1 t2 >> return ()

-- | Determine the type of a binary operation.
binopType :: CBinaryOp -> Type -> Type -> Either String Type
binopType op t1 t2 =
  case (op, canonicalType t1, canonicalType t2) of
    (_, t1', t2')
      | isLogicOp op ->
        checkScalar t1' >> checkScalar t2' >> return boolType
      | isCmpOp op ->
        case (t1', t2') of
          (DirectType tn1 _, DirectType tn2 _) ->
                case arithmeticConversion tn1 tn2 of
                  Just _ -> return boolType
                  Nothing -> fail
                             "incompatible arithmetic types in comparison"
          (PtrType (DirectType TyVoid _) _ _, _)
            | isPointerType t2' -> return boolType
          (_, PtrType (DirectType TyVoid _) _ _)
            | isPointerType t1' -> return boolType
          (_, _)
            | isPointerType t1' && isIntegralType t2' -> return boolType
            | isIntegralType t1' && isPointerType t2' -> return boolType
            | isPointerType t1' && isPointerType t2' ->
              compatible t1' t2' >> return boolType
          (_, _) -> fail "incompatible types in comparison"
    (CSubOp, PtrType t1' _ _, PtrType t2' _ _) ->
      do compatible t1' t2'
         return ptrDiffType
    (_, PtrType _ _ _, t2')
      | isPtrOp op && isIntegralType t2' -> return t1
      | otherwise -> fail $ "invalid pointer operation: " ++ show op
    (CAddOp, t1', PtrType _ _ _) | isIntegralType t1' -> return t2
    (_, ArrayType _ _ _ _, t2')
      | isPtrOp op && isIntegralType t2' -> return t1
      | otherwise -> fail $ "invalid pointer operation: " ++ show op
    (CAddOp, t1', ArrayType _ _ _ _) | isIntegralType t1' -> return t2
    (_, DirectType tn1 q1, DirectType tn2 q2) ->
        do when (isBitOp op) (checkIntegral t1 >> checkIntegral t2)
           case arithmeticConversion tn1 tn2 of
             Just tn -> return $ DirectType tn (mergeTypeQuals q1 q2)
             Nothing -> fail $ "invalid binary operation: " ++
                        show op ++ ", " ++ pType t1 ++ ", " ++ pType t2
    (_, _, _) -> fail $ "unhandled binary operation: "
                 ++ pType t1 ++ show op ++ pType t2

-- | Determine the type of a conditional expression.
conditionalType :: Type -> Type -> Either String Type
conditionalType t1 t2 =
  case (canonicalType t1, canonicalType t2) of
    (PtrType (DirectType TyVoid _) _ _, t2') | isPointerType t2' -> return t2
    (t1', PtrType (DirectType TyVoid _) _ _) | isPointerType t1' -> return t1
    (ArrayType t1' _ q1 a1, ArrayType t2' _ q2 a2) ->
      do t <- compositeType t1' t2'
         return $ ArrayType t (UnknownArraySize False)
                  (mergeTypeQuals q1 q2) (mergeAttrs a1 a2)
    (t1'@(DirectType tn1 q1), t2'@(DirectType tn2 q2)) ->
      case arithmeticConversion tn1 tn2 of
        Just tn -> return $ DirectType tn (mergeTypeQuals q1 q2)
        Nothing -> compositeType t1' t2'
    (t1', t2') -> compositeType t1' t2'

derefType :: Type -> Either String Type
derefType t =
  -- XXX: is it good to use canonicalType here?
  case canonicalType t of
    PtrType t' _ _ -> return t'
    ArrayType t' _ _ _ -> return t'
    _ -> fail $ "dereferencing non-pointer: " ++ pType t

varAddrType :: IdentDecl -> Either String Type
varAddrType d =
  do case declStorage d of
       Auto True -> fail "address of register variable"
       _         -> return ()
     case t of
       ArrayType _ _ q a -> return $ PtrType t q a
       _                 -> return $ simplePtr t
  where t = declType d

-- | Get the type of field @m@ of type @t@
fieldType :: MonadTrav m => NodeInfo -> Ident -> Type -> m Type
fieldType ni m t =
  case canonicalType t of
    DirectType (TyComp ctr) _ ->
      do td <- lookupSUE ni (sueRef ctr)
         ms <- tagMembers ni td
         case lookup m ms of
           Just ft -> return ft
           Nothing -> typeError ni $ "field not found: " ++ identToString m
    t' -> astError ni $
          "field of non-composite type: " ++ identToString m
          ++ ", " ++ pType t

-- | Get all members of a struct, union, or enum, with their
--   types. Collapse fields of anonymous members.
tagMembers :: MonadTrav m => NodeInfo -> TagDef -> m [(Ident, Type)]
tagMembers ni td =
  case td of
    CompDef (CompType _ _ ms _ _) -> getMembers ms
    EnumDef (EnumType _ es _ _) -> getMembers es
  where getMembers ds =
          do let ts = map declType ds
                 ns = map declName ds
             concat `liftM` mapM (expandAnonymous ni) (zip ns ts)

-- | Expand an anonymous composite type into a list of member names
--   and their associated types.
expandAnonymous :: MonadTrav m =>
                   NodeInfo -> (VarName, Type)
                -> m [(Ident, Type)]
expandAnonymous ni (NoName, DirectType (TyComp ctr) _) =
  lookupSUE ni (sueRef ctr) >>= tagMembers ni
expandAnonymous _ (NoName, _) = return []
expandAnonymous _ (n, t) = return [(identOfVarName n, t)]

lookupSUE :: MonadTrav m => NodeInfo -> SUERef -> m TagDef
lookupSUE ni sue =
  do dt <- getDefTable
     case lookupTag sue dt of
       Just (Right td) -> return td
       _               ->
         typeError ni $ "unknown composite type: " ++ (render . pretty) sue
