{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Parser.Translation
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  alpha
-- Portability :  ghc
--
-- Analyse the parse tree
--
-- Traverses the AST, analyses declarations and invokes handlers.
-----------------------------------------------------------------------------
module Language.C.Analysis.AstAnalysis (
    -- * Top-level analysis
    analyseAST,
    analyseExt,analyseFunDef,analyseDecl,
    -- * Type checking
    tExpr, ExprSide(..),
    tStmt, StmtCtx(..)
)
where
import Language.C.Analysis.SemError
import Language.C.Analysis.SemRep
import Language.C.Analysis.TravMonad
import Language.C.Analysis.Debug
import Language.C.Analysis.DefTable (globalDefs, defineScopedIdent,
                                     defineLabel, inFileScope, lookupTag,
                                     lookupLabel)
import Language.C.Analysis.DeclAnalysis
import Language.C.Analysis.TypeUtils

import Language.C.Data
import Language.C.Pretty
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Syntax.Ops
import Language.C.Syntax.Utils
import Text.PrettyPrint.HughesPJ


import Control.Monad
import Prelude hiding (reverse)
import Data.Foldable (foldrM)
import Data.List hiding (reverse)
import Data.Maybe

-- * analysis

-- | Analyse the given AST
--
-- @analyseAST ast@ results in global declaration dictionaries.
-- If you want to perform specific actions on declarations or definitions, you may provide
-- callbacks in the @MonadTrav@ @m@.
--
-- Returns the set of global declarations and definitions which where successfully translated.
-- It is the users responsibility to check whether any hard errors occured (@runTrav@ does this for you).
analyseAST :: (MonadTrav m) => CTranslUnit -> m GlobalDecls
analyseAST (CTranslUnit decls _file_node) = do
    -- analyse all declarations, but recover from errors
    mapRecoverM_ analyseExt decls
    -- check we are in global scope afterwards
    getDefTable >>= \dt -> when (not (inFileScope dt)) $
        error "Internal Error: Not in filescope after analysis"
    -- get the global definition table (XXX: remove ?)
    liftM globalDefs getDefTable
    where
    mapRecoverM_ f = mapM_ (handleTravError . f)

-- | Analyse an top-level declaration
analyseExt :: (MonadTrav m) => CExtDecl -> m ()
analyseExt (CAsmExt asm _)
    = handleAsmBlock asm
analyseExt (CFDefExt fundef)
    = analyseFunDef fundef
analyseExt (CDeclExt decl)
    = analyseDecl False decl

-- | Analyse a function definition
analyseFunDef :: (MonadTrav m) => CFunDef -> m ()
analyseFunDef (CFunDef declspecs declr oldstyle_decls stmt node_info) = do
    -- analyse the declarator
    var_decl_info <- analyseVarDecl True declspecs declr oldstyle_decls Nothing
    let (VarDeclInfo name is_inline storage_spec attrs ty declr_node) = var_decl_info
    let ident = identOfVarName name
    -- improve incomplete type
    ty' <- improveFunDefType ty
    -- compute storage
    fun_storage <- computeFunDefStorage ident storage_spec
    let var_decl = VarDecl name (DeclAttrs is_inline fun_storage attrs) ty'
    -- callback for declaration
    handleVarDecl False (Decl var_decl node_info)
    -- process body
    stmt' <- analyseFunctionBody node_info var_decl stmt
    -- callback for definition
    handleFunDef ident (FunDef var_decl stmt' node_info)
    where
    improveFunDefType (FunctionType (FunTypeIncomplete return_ty attrs)) =
      return . FunctionType $ FunType return_ty [] False attrs
    improveFunDefType ty = return $ ty

-- | Analyse a declaration other than a function definition
analyseDecl :: (MonadTrav m) => Bool -> CDecl -> m ()
analyseDecl is_local decl@(CDecl declspecs declrs node)
    | null declrs =
        case typedef_spec of Just _  -> astError node "bad typedef declaration: missing declarator"
                             Nothing -> analyseTypeDecl decl >> return ()
    | (Just declspecs') <- typedef_spec = mapM_ (uncurry (analyseTyDef declspecs')) declr_list
    | otherwise   = mapM_ (uncurry analyseVarDeclr) declr_list
    where
    declr_list = zip (True : repeat False) declrs
    typedef_spec = hasTypeDef declspecs

    analyseTyDef declspecs' handle_sue_def declr =
        case declr of
            (Just tydeclr, Nothing , Nothing) -> analyseTypeDef handle_sue_def declspecs' tydeclr node
            _ -> astError node "bad typdef declaration: bitfieldsize or initializer present"

    analyseVarDeclr handle_sue_def (Just declr, init_opt, Nothing) = do
        -- analyse the declarator
        vardeclInfo@(VarDeclInfo _ _ _ _ typ _) <- analyseVarDecl handle_sue_def declspecs declr [] Nothing
        -- declare / define the object
        if (isFunctionType typ)
            then extFunProto vardeclInfo
            else (if is_local then localVarDecl else extVarDecl)
                 -- XXX: if Initializer becomes different from CInit, this
                 -- will have to change.
                 vardeclInfo init_opt
        init_opt' <- mapMaybeM init_opt (tInit typ)
        return ()
    analyseVarDeclr _ (Nothing,_,_)         = astError node "abstract declarator in object declaration"
    analyseVarDeclr _ (_,_,Just bitfieldSz) = astError node "bitfield size in object declaration"

-- | Analyse a typedef
analyseTypeDef :: (MonadTrav m) => Bool -> [CDeclSpec] -> CDeclr -> NodeInfo -> m ()
analyseTypeDef handle_sue_def declspecs declr node_info = do
    -- analyse the declarator
    (VarDeclInfo name is_inline storage_spec attrs ty declr_node) <- analyseVarDecl handle_sue_def declspecs declr [] Nothing
    checkValidTypeDef is_inline storage_spec attrs
    let ident = identOfVarName name
    handleTypeDef (TypeDef ident ty attrs node_info)
    where
    checkValidTypeDef True _ _ = astError node_info "inline specifier for typeDef"
    checkValidTypeDef _ NoStorageSpec _ = return ()
    checkValidTypeDef _ bad_storage _ = astError node_info $ "storage specified for typeDef: " ++ show bad_storage

-- | compute storage of a function definition
--
-- a function definition has static storage with internal linkage if specified `static`,
-- the previously declared linkage if any if 'extern' or no specifier are present. (See C99 6.2.2, clause 5)
--
-- This function won't raise an Trav error if the declaration is incompatible with the existing one,
-- this case is handled in 'handleFunDef'.
computeFunDefStorage :: (MonadTrav m) => Ident -> StorageSpec -> m Storage
computeFunDefStorage _ (StaticSpec b)  = return$ FunLinkage InternalLinkage
computeFunDefStorage ident other_spec  = do
  obj_opt <- lookupObject ident
  let defaultSpec = FunLinkage ExternalLinkage
  case other_spec of
    NoStorageSpec  -> return$ maybe defaultSpec declStorage obj_opt
    (ExternSpec False) -> return$ maybe defaultSpec declStorage obj_opt
    bad_spec -> throwTravError $ badSpecifierError (nodeInfo ident)
                  $ "unexpected function storage specifier (only static or extern is allowed)" ++ show bad_spec

-- (private) Get parameters of a function type
getParams :: Type -> Maybe [ParamDecl]
getParams (FunctionType (FunType _ params _ _)) = Just params
getParams _ = Nothing

-- | handle a function prototype
extFunProto :: (MonadTrav m) => VarDeclInfo -> m ()
extFunProto (VarDeclInfo var_name is_inline storage_spec attrs ty node_info) =
    do  old_fun <- lookupObject (identOfVarName var_name)
        checkValidSpecs
        let decl = VarDecl var_name (DeclAttrs is_inline (funDeclLinkage old_fun) attrs) ty
        handleVarDecl False (Decl decl node_info)
        -- XXX: structs should be handled in 'function prototype scope' too
        enterPrototypeScope
        maybe (return ()) (mapM_ handleParamDecl) (getParams ty)
        leavePrototypeScope
    where
    funDeclLinkage old_fun =
        case storage_spec of
            NoStorageSpec    -> FunLinkage ExternalLinkage -- prototype declaration / external linkage
            StaticSpec False -> FunLinkage InternalLinkage -- prototype declaration / internal linkage
            ExternSpec False -> case old_fun of
                                    Nothing -> FunLinkage ExternalLinkage
                                    Just f  -> declStorage f
            _ -> error $ "funDeclLinkage: " ++ show storage_spec
    checkValidSpecs
        | hasThreadLocalSpec storage_spec = astError node_info "thread local storage specified for function"
        | RegSpec <- storage_spec         = astError node_info "invalid `register' storage specified for function"
        | otherwise                       = return ()

-- | handle a object declaration \/ definition
--
-- We have to check the storage specifiers here, as they determine wheter we're dealing with decalartions
-- or definitions
-- see [http://www.sivity.net/projects/language.c/wiki/ExternalDefinitions]
extVarDecl :: (MonadTrav m) => VarDeclInfo -> (Maybe Initializer) -> m ()
extVarDecl (VarDeclInfo var_name is_inline storage_spec attrs typ node_info) init_opt =
    do (storage,is_def) <- globalStorage storage_spec
       let vardecl = VarDecl var_name (DeclAttrs is_inline storage attrs) typ
       if is_def
           then handleObjectDef False ident $ ObjDef vardecl init_opt node_info
           else handleVarDecl False $ Decl vardecl node_info
    where
       ident = identOfVarName var_name
       globalStorage _ | is_inline = astError node_info "invalid `inline' specifier external variable"
       globalStorage RegSpec       = astError node_info "invalid `register' storage specified for external object"
       -- tentative if there is no initializer, external
       globalStorage NoStorageSpec = return $ (Static ExternalLinkage False, True)
       -- tentative if there is no initializer, internal
       globalStorage (StaticSpec thread_local) = return $ (Static InternalLinkage thread_local, True)
       globalStorage (ExternSpec thread_local) =
           case init_opt of
               -- declaration with either external or old storage
               Nothing -> do old_decl <- lookupObject ident
                             return $ (maybe (Static ExternalLinkage thread_local) declStorage old_decl,False)
               -- warning, external definition
               Just _  -> do warn $ badSpecifierError node_info "Both initializer and `extern` specifier given - treating as definition"
                             return $ (Static ExternalLinkage thread_local, True)

-- | handle a function-scope object declaration \/ definition
-- see [http://www.sivity.net/projects/language.c/wiki/LocalDefinitions]
localVarDecl :: (MonadTrav m) => VarDeclInfo -> (Maybe Initializer) -> m ()
localVarDecl (VarDeclInfo var_name is_inline storage_spec attrs typ node_info) init_opt =
    do (storage,is_def) <- localStorage storage_spec
       let vardecl = VarDecl var_name (DeclAttrs is_inline storage attrs) typ
       if is_def
           then handleObjectDef True ident (ObjDef vardecl init_opt node_info)
           else handleVarDecl True (Decl vardecl node_info)
    where
    ident = identOfVarName var_name
    localStorage _
      | is_inline = astError node_info "invalid `inline' specifier for local variable"
    localStorage NoStorageSpec = return $ (Auto False,True)
    localStorage RegSpec = return $ (Auto True,True)
    -- static no linkage
    localStorage (StaticSpec thread_local) =
      return $ (Static NoLinkage thread_local,True)
    localStorage (ExternSpec thread_local)
      | isJust init_opt = astError node_info "extern keyword and initializer for local"
      | otherwise =
          do old_decl <- lookupObject ident
             return (maybe (Static ExternalLinkage thread_local) declStorage old_decl,False)
    localStorage s = astError node_info "bad storage specifier for local"

analyseFunctionBody :: (MonadTrav m) => NodeInfo -> VarDecl -> CStat -> m Stmt
analyseFunctionBody node_info decl s@(CCompound localLabels items _) =
  do enterFunctionScope
     mapM (withDefTable . defineLabel) (localLabels ++ getLabels s)
     -- record parameters
     case (getParams $ declType decl) of
         Nothing -> astError node_info "expecting complete function type in function definition"
         Just params -> mapM handleParamDecl params
     mapM_ (tBlockItem [FunCtx decl]) items
     leaveFunctionScope
     return s -- XXX: bogus

analyseFunctionBody _ _ s = astError (nodeInfo s) "Function body is no compound statement"

-- TODO:
--
-- check that local labels aren't used outside their declared scope
-- enter declarations with initializers into the symbol table before
--   typechecking their initializers. Things like this are valid:
--     struct s *x = malloc(sizeof(*x));

pType :: Type -> String
pType = render . pretty

-- XXX: this should use a custom error type, but typeMismatch isn't always right
typeError :: MonadTrav m => NodeInfo -> String -> m a
typeError = astError

notFound :: MonadTrav m => NodeInfo -> Ident -> m a
notFound ni i = typeError ni $ "not found: " ++ identToString i

data StmtCtx = FunCtx VarDecl
             | LoopCtx
             | SwitchCtx

-- | Given a context, determine the type declaration for the enclosing
--   function, if possible, given a context.
enclosingFunctionType :: [StmtCtx] -> Maybe Type
enclosingFunctionType [] = Nothing
enclosingFunctionType (FunCtx vd : _) = Just $ declType vd
enclosingFunctionType (_ : cs) = enclosingFunctionType cs

inLoop :: [StmtCtx] -> Bool
inLoop c = any isLoop c
  where isLoop LoopCtx = True
        isLoop _ = False

inSwitch :: [StmtCtx] -> Bool
inSwitch c = any isSwitch c
  where isSwitch SwitchCtx = True
        isSwitch _ = False

data ExprSide = LValue | RValue
                deriving (Eq, Show)

-- | Typecheck a statement, given a statement context. The type of a
--   statement is usually @void@, but expression statements and blocks
--   can sometimes have other types.
tStmt :: MonadTrav m => [StmtCtx] -> CStat -> m Type
tStmt c (CLabel _ s _ _)         = tStmt c s
tStmt _ (CExpr e _)              =
  maybe (return voidType) (tExpr RValue) e
tStmt c (CCompound ls body _)    =
  do enterBlockScope
     mapM_ (withDefTable . defineLabel) ls
     t <- foldM (const $ tBlockItem c) voidType body
     leaveBlockScope
     return t
tStmt c (CIf e sthen selse _)    =
  checkGuard e >> tStmt c sthen
               >> maybe (return ()) (\s -> tStmt c s >> return ()) selse
               >> return voidType
tStmt c (CSwitch e s ni)         =
  tExpr RValue e >>= checkIntegral ni >>
  tStmt (SwitchCtx : c) s
tStmt c (CWhile e s _ _)         =
  checkGuard e >> tStmt (LoopCtx : c) s
tStmt _ (CGoto l ni)             =
  do dt <- getDefTable
     case lookupLabel l dt of
       Just _ -> return voidType
       Nothing -> typeError ni $ "undefined label in goto: " ++ identToString l
tStmt c (CCont ni)               =
  do unless (inLoop c) $ astError ni "continue statement outside of loop"
     return voidType
tStmt c (CBreak ni)              =
  do unless (inLoop c || inSwitch c) $
            astError ni "break statement outside of loop or switch statement"
     return voidType
tStmt c (CReturn (Just e) ni)    =
  do t <- tExpr RValue e
     rt <- case enclosingFunctionType c of
             Just (FunctionType (FunType rt _ _ _)) -> return rt
             Just (FunctionType (FunTypeIncomplete rt _)) -> return rt
             Just ft -> astError ni $ "bad function type: " ++ pType ft
             Nothing -> astError ni "return statement outside function"
     assignCompatible ni CAssignOp rt t
     return voidType
tStmt _ (CReturn Nothing _)      = return voidType
-- XXX: anything to do for assembly?
tStmt _ (CAsm _ _)               = return voidType
tStmt c (CCase e s ni)           =
  do unless (inSwitch c) $
            astError ni "case statement outside of switch statement"
     tExpr RValue e >>= checkIntegral ni
     tStmt c s
tStmt c (CCases e1 e2 s ni)      =
  do unless (inSwitch c) $
            astError ni "case statement outside of switch statement"
     tExpr RValue e1 >>= checkIntegral ni
     tExpr RValue e2 >>= checkIntegral ni
     tStmt c s
tStmt c (CDefault s ni)          =
  do unless (inSwitch c) $
            astError ni "default statement outside of switch statement"
     tStmt c s
tStmt c (CFor i g inc s _)       =
  do enterBlockScope
     either (maybe (return ()) checkExpr) (analyseDecl True) i
     maybe (return ()) (checkGuard) g
     maybe (return ()) checkExpr inc
     tStmt (LoopCtx : c) s
     leaveBlockScope
     return voidType
  where checkExpr e = tExpr RValue e >> return ()
tStmt _ (CGotoPtr e ni)          =
  do t <- tExpr RValue e
     case t of
       (PtrType _ _ _) -> return voidType
       _               -> typeError ni "can't goto non-pointer"

-- | Typecheck a block item. When statement expressions are blocks,
--   they have the type of their last expression statement, so this
--   needs to return a type.
tBlockItem :: MonadTrav m => [StmtCtx] -> CBlockItem -> m Type
tBlockItem c (CBlockStmt s) = tStmt c s
tBlockItem _ (CBlockDecl d) = analyseDecl True d >> return voidType
-- TODO: fixup analyseFunDef to handle nested functions
tBlockItem _ (CNestedFunDef fd) = analyseFunDef fd >> return voidType

checkGuard :: MonadTrav m => CExpr -> m ()
checkGuard e = tExpr RValue e >>= checkScalar (nodeInfo e)

checkScalar :: MonadTrav m => NodeInfo -> Type -> m ()
checkScalar ni t =
  case deepDerefTypeDef t of
    DirectType _ _    -> return ()
    PtrType _ _ _     -> return ()
    ArrayType _ _ _ _ -> return () -- because it's just a pointer
    t' -> typeError ni $ "expected scalar type, got: "
                         ++ pType t ++ " (" ++ pType t' ++ ")"

checkIntegral :: MonadTrav m => NodeInfo -> Type -> m ()
checkIntegral ni t | isIntegralType (deepDerefTypeDef t) = return ()
                   | otherwise = typeError ni "expected integral type"

handleFunction :: Type -> Type
handleFunction t@(FunctionType _) = simplePtr t
handleFunction t = t

handleArray :: Type -> Type
handleArray (ArrayType bt _ q a) = PtrType bt q a -- XXX: q and a correct?
handleArray t = t

varAddrType :: MonadTrav m => NodeInfo -> IdentDecl -> m Type
varAddrType ni d =
  do case s of
       Auto True -> typeError ni "address of register variable"
       _         -> return ()
     case t of
       ArrayType _ _ _ _ -> return $ handleArray t
       _                 -> return $ simplePtr t
  where s = declStorage d
        t = declType d

-- | Typecheck an expression, with information about whether it
--   appears as an lvalue or an rvalue.
tExpr :: MonadTrav m => ExprSide -> CExpr -> m Type
tExpr side (CBinary op le re ni)    =
  do when (side == LValue) $ typeError ni "binary operator as lvalue"
     lt <- tExpr RValue le
     rt <- tExpr RValue re
     binopType ni op lt rt
tExpr side (CUnary CAdrOp e ni)     =
  do when (side == LValue) $
          typeError ni "address-of operator as lvalue"
     case e of
       CVar i _ -> lookupObject i >>=
                   maybe (notFound ni i) (varAddrType ni)
       _        -> simplePtr `liftM` tExpr LValue e
tExpr _ (CUnary CIndOp e ni)     =
  tExpr RValue e >>= derefType ni
tExpr _ (CUnary CCompOp e ni)    =
  do t <- tExpr RValue e
     checkIntegral ni t
     return t
tExpr side (CUnary CNegOp e ni)      =
  do when (side == LValue) $
          typeError ni "logical negation used as lvalue"
     tExpr RValue e >>= checkScalar ni
     return boolType
tExpr side (CUnary op e _)          =
  tExpr (if isEffectfulOp op then LValue else side) e
tExpr _ (CIndex b i ni)             =
  do bt <- tExpr RValue b
     it <- tExpr RValue i
     addrTy <- binopType ni CAddOp bt it
     derefType ni addrTy
tExpr side (CCond e1 me2 e3 ni)     =
  do t1 <- tExpr RValue e1
     checkScalar ni t1
     t3 <- tExpr side e3
     case me2 of
       Just e2 ->
         do t2 <- tExpr side e2
            conditionalType ni t2 t3
       Nothing -> conditionalType ni t1 t3
tExpr side (CMember e m deref ni)   =
  do t <- tExpr RValue e
     bt <- if deref then derefType ni t else return t
     fixup `liftM` fieldType ni bt m
  where fixup | side == RValue = handleArray . handleFunction
              | otherwise = id
tExpr side (CComma es _)            =
  mapM (tExpr side) es >>= return . last
tExpr side (CCast d e ni)           =
  do dt <- analyseTypeDecl d
     et <- tExpr side e
     castCompatible ni dt et
     return $ handleArray dt
tExpr side (CSizeofExpr e ni)       =
  do when (side == LValue) $ typeError ni "sizeof as lvalue"
     tExpr RValue e
     return sizeofType
tExpr side (CAlignofExpr e ni)      =
  do when (side == LValue) $ typeError ni "alignof as lvalue"
     tExpr RValue e
     return sizeofType
tExpr side (CComplexReal e ni)      = complexBaseType ni side e
tExpr side (CComplexImag e ni)      = complexBaseType ni side e
tExpr side (CLabAddrExpr _ ni)      =
  do when (side == LValue) $ typeError ni "label address as lvalue"
     return $ PtrType voidType noTypeQuals []
tExpr side (CCompoundLit d initList ni) =
  do when (side == LValue) $ typeError ni "compound literal as lvalue"
     lt <- analyseTypeDecl d
     tInitList (deepDerefTypeDef lt) initList
     return lt
tExpr RValue (CAlignofType _ _)     = return sizeofType
tExpr RValue (CSizeofType _ _)      = return sizeofType
tExpr LValue (CAlignofType _ ni)    =
  typeError ni "alignoftype as lvalue"
tExpr LValue (CSizeofType _ ni)     =
  typeError ni "sizeoftype as lvalue"
tExpr side (CVar i ni)              =
  lookupObject i >>= maybe (notFound ni i) fixup
  where fixup d | side == RValue =
                  return $
                  handleArray $
                  handleFunction $
                  declType d
                | otherwise = return $ declType d
tExpr _ (CConst c)                  = constType c
tExpr _ (CBuiltinExpr b)            = builtinType b
tExpr _ (CCall fe args ni)          =
  do t <- tExpr RValue fe
     atys <- mapM (tExpr RValue) args
     let t' = case deepDerefTypeDef t of
                PtrType t'' _ _ -> t''
                t''             -> t''
     case t' of
       FunctionType (FunType rt pdecls varargs _) ->
         do let ptys = map declType pdecls
            mapM_ (uncurry (assignCompatible ni CAssignOp)) (zip ptys atys)
            unless varargs $ when (length atys /= length ptys) $
                   typeError ni "incorrect number of arguments"
            return $ deepDerefTypeDef rt
       FunctionType (FunTypeIncomplete rt _)   ->
         return $ deepDerefTypeDef rt
       _  -> typeError ni $ "attempt to call non-function of type " ++ pType t
tExpr _ (CAssign op le re ni)       =
  do lt <- tExpr LValue le
     rt <- tExpr RValue re
     when (constant $ typeQuals lt) $
          typeError ni $ "assignment to lvalue with `constant' qualifier: "
                         ++ (render . pretty) le
     assignCompatible ni op lt rt
     return lt
tExpr _ (CStatExpr s _)             = tStmt [] s

tInitList :: MonadTrav m => Type -> CInitList -> m ()
tInitList t initList = mapM_ checkInit initList
  where checkInit (desigs, init') =
          do dts <- mapM (designatorType t) desigs
             mapM_ (\dt -> tInit dt init') dts

designatorType :: MonadTrav m => Type -> CDesignator -> m Type
designatorType (ArrayType bt _ _ _) (CArrDesig e ni) =
  do tExpr RValue e >>= checkIntegral ni
     return bt
designatorType (ArrayType bt _ _ _) (CRangeDesig e1 e2 ni) =
  do tExpr RValue e1 >>= checkIntegral ni
     tExpr RValue e2 >>= checkIntegral ni
     return bt
designatorType t@(DirectType (TyComp _) _) (CMemberDesig m ni) =
  fieldType ni t m
designatorType _ d = typeError (nodeInfo d) "invalid designator in initializer"

tInit :: MonadTrav m => Type -> CInit -> m Initializer
tInit t i@(CInitExpr e ni) =
  do it <- tExpr RValue e
     assignCompatible ni CAssignOp t it
     return i
tInit t i@(CInitList initList _) = tInitList t initList >> return i

derefType :: MonadTrav m => NodeInfo -> Type -> m Type
derefType ni t =
  case deepDerefTypeDef t of
    PtrType t' _ _ -> return t'
    ArrayType t' _ _ _ -> return t'
    _ -> typeError ni $ "dereferencing non-pointer: " ++ pType t

fieldType :: MonadTrav m => NodeInfo -> Type -> Ident -> m Type
fieldType ni t m =
  case deepDerefTypeDef t of
    DirectType (TyComp ctr) _ ->
      do dt <- getDefTable
         case lookupTag (sueRef ctr) dt of
           Just (Left _)   -> typeError ni "composite declared but not defined"
           Just (Right td) -> tagDefType ni td m
           Nothing         -> typeError ni "unknown composite"
    _ -> typeError ni $ "field of non-composite object: " ++ pType t

tagDefType :: MonadTrav m => NodeInfo -> TagDef -> Ident -> m Type
tagDefType ni td field =
  case td of
    (CompDef (CompType _ _ ms _ _)) -> getMatchingMember field ms
    (EnumDef (EnumType _ es _ _))   -> getMatchingMember field es
  where getMatchingMember m ds =
          case filter (hasIdent m . declName) ds of
            [d] -> return $ declType d
            []  -> typeError ni $ "field not found: " ++ identToString m
            _   -> typeError ni $
                   "field defined multiple times: " ++ identToString m
        hasIdent i (VarName i' _) = i == i'
        hasIdent _ NoName = False

complexBaseType :: MonadTrav m => NodeInfo -> ExprSide -> CExpr -> m Type
complexBaseType ni side e =
  do t <- tExpr side e
     case deepDerefTypeDef t of
       DirectType (TyComplex ft) quals ->
         return $ DirectType (TyFloating ft) quals
       _ -> typeError ni $ "expected complex type, got: " ++ pType t

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
constType (CStrConst (CString chars wide) _) =
  do n <- genName
     let charType | wide      = TyInt -- XXX: this isn't universal
                  | otherwise = TyChar
         ni = mkNodeInfo nopos n
         arraySize = ArraySize
                     True -- XXX: is it static?
                     (CConst
                      (CIntConst
                       (cInteger (toInteger (length chars))) ni))
     return $ ArrayType (DirectType (TyIntegral charType) noTypeQuals)
                        arraySize noTypeQuals []

-- | Determine the type of a binary operation.
binopType :: MonadTrav m => NodeInfo -> CBinaryOp -> Type -> Type -> m Type
binopType ni op t1 t2 =
  case (op, deepDerefTypeDef t1, deepDerefTypeDef t2) of
    (_, t1', t2')
      | isLogicOp op ->
        checkScalar ni t1' >> checkScalar ni t2' >> return boolType
      | isCmpOp op ->
        case (t1', t2') of
          (DirectType tn1 _, DirectType tn2 _) ->
                case arithmeticConversion tn1 tn2 of
                  Just _ -> return boolType
                  Nothing -> typeError ni
                             "incompatible arithmetic types in comparison"
          (PtrType (DirectType TyVoid _) _ _, _)
            | isPointerType t2' -> return boolType
          (_, PtrType (DirectType TyVoid _) _ _)
            | isPointerType t1' -> return boolType
          (_, _)
            | isPointerType t1' && isIntegralType t2' -> return boolType
            | isIntegralType t1' && isPointerType t2' -> return boolType
            | isPointerType t1' && isPointerType t2' ->
              compatible ni t1' t2' >> return boolType
          (_, _) -> typeError ni "incompatible types in comparison"
    (CSubOp, PtrType t1' _ _, PtrType t2' _ _) ->
      do compatible ni t1' t2'
         return ptrDiffType
    (_, PtrType _ _ _, t2')
      | isPtrOp op && isIntegralType t2' -> return t1
      | otherwise -> typeError ni $ "invalid pointer operation: " ++ show op
    (CAddOp, t1', PtrType _ _ _) | isIntegralType t1' -> return t2
    (_, ArrayType _ _ _ _, t2')
      | isPtrOp op && isIntegralType t2' -> return t1
      | otherwise -> typeError ni $ "invalid pointer operation: " ++ show op
    (CAddOp, t1', ArrayType _ _ _ _) | isIntegralType t1' -> return t2
    (_, DirectType tn1 q1, DirectType tn2 q2) ->
        do when (isBitOp op) (checkIntegral ni t1 >> checkIntegral ni t2)
           case arithmeticConversion tn1 tn2 of
             Just tn -> return $ DirectType tn (mergeTypeQuals q1 q2)
             Nothing -> typeError ni $ "invalid binary operation: " ++
                        show op ++ ", " ++ pType t1 ++ ", " ++ pType t2
    (_, _, _) -> typeError ni $ "unhandled binary operation: "
                 ++ pType t1 ++ show op ++ pType t2

-- | Determine the type of a conditional expression.
conditionalType :: MonadTrav m => NodeInfo -> Type -> Type -> m Type
conditionalType ni t1 t2 =
  case (deepDerefTypeDef t1, deepDerefTypeDef t2) of
    (DirectType TyVoid q1, DirectType TyVoid q2) ->
      return $ DirectType TyVoid (mergeTypeQuals q1 q2)
    (DirectType (TyComp (CompTypeRef sue1 _ _)) _,
     DirectType (TyComp (CompTypeRef sue2 _ _)) _)
      | sue1 == sue2 -> return t1
      | otherwise -> typeError ni "different composite types in conditional"
    (DirectType tn1 q1, DirectType tn2 q2) ->
      case arithmeticConversion tn1 tn2 of
        Just tn -> return $ DirectType tn (mergeTypeQuals q1 q2)
        Nothing -> typeError ni
                   "incompatible arithmetic operands in conditional"
    (PtrType (DirectType TyVoid _) _ _, t2') | isPointerType t2' -> return t2
    (t1', PtrType (DirectType TyVoid _) _ _) | isPointerType t1' -> return t1
    (ArrayType t1' _ q1 a1, ArrayType t2' _ q2 a2) ->
      do t <- compositeType ni t1' t2'
         return $ ArrayType t (UnknownArraySize False)
                  (mergeTypeQuals q1 q2) (mergeAttrs a1 a2)
    (t1', t2') -> compositeType ni t1' t2'

-- | For an arithmetic operator, if the arguments are of the given
--   types, return the type of the full expression.
arithmeticConversion :: TypeName -> TypeName -> Maybe TypeName
-- XXX: I'm assuming that double `op` complex float = complex
-- double. The standard seems somewhat unclear on whether this is
-- really the case.
arithmeticConversion (TyComplex t1) (TyComplex t2) =
  Just $ TyComplex $ floatConversion t1 t2
arithmeticConversion (TyComplex t1) (TyFloating t2) =
  Just $ TyComplex $ floatConversion t1 t2
arithmeticConversion (TyFloating t1) (TyComplex t2) =
  Just $ TyComplex $ floatConversion t1 t2
arithmeticConversion t1@(TyComplex _) (TyIntegral _) = Just t1
arithmeticConversion (TyIntegral _) t2@(TyComplex _) = Just t2
arithmeticConversion (TyFloating t1) (TyFloating t2) =
  Just $ TyFloating $ floatConversion t1 t2
arithmeticConversion t1@(TyFloating _) (TyIntegral _) = Just t1
arithmeticConversion (TyIntegral _) t2@(TyFloating _) = Just t2
arithmeticConversion (TyIntegral t1) (TyIntegral t2) =
  Just $ TyIntegral $ intConversion t1 t2
arithmeticConversion (TyEnum _) (TyEnum _) = Just $ TyIntegral TyInt
arithmeticConversion (TyEnum _) t2 = Just $ t2
arithmeticConversion t1 (TyEnum _) = Just $ t1
arithmeticConversion _ _ = Nothing

floatConversion :: FloatType -> FloatType -> FloatType
floatConversion = max

intConversion :: IntType -> IntType -> IntType
intConversion t1 t2 = max TyInt (max t1 t2)

-- | Return the type of a builtin.
builtinType :: MonadTrav m => CBuiltin -> m Type
builtinType (CBuiltinVaArg _ d _)           = analyseTypeDecl d
builtinType (CBuiltinOffsetOf _ _ _)        = return sizeofType
builtinType (CBuiltinTypesCompatible _ _ _) = return boolType

castCompatible :: MonadTrav m => NodeInfo -> Type -> Type -> m ()
castCompatible _ (DirectType TyVoid _) _ = return ()
castCompatible ni t1 t2 =
  case (deepDerefTypeDef t1, deepDerefTypeDef t2) of
    (DirectType TyVoid _, _) -> return ()
    (_, _) -> checkScalar ni t1 >> checkScalar ni t2


-- | Determine whether two types are compatible in an assignment expression.
assignCompatible :: MonadTrav m => NodeInfo -> CAssignOp -> Type -> Type -> m ()
assignCompatible ni CAssignOp t1 t2 =
  case (deepDerefTypeDef t1, deepDerefTypeDef t2) of
    (DirectType (TyBuiltin TyAny) _, _) -> return ()
    (_, DirectType (TyBuiltin TyAny) _) -> return ()
    -- XXX: check qualifiers
    (PtrType (DirectType TyVoid _) _ _, t2') | isPointerType t2' -> return ()
    -- XXX: check qualifiers
    (t1', PtrType (DirectType TyVoid _) _ _) | isPointerType t1' -> return ()
    (PtrType _ _ _, t2') | isIntegralType t2' -> return ()
    (_, _) | isPointerType t1 && isPointerType t2 ->
             do compatible ni (baseType t1) (baseType t2)
                --unless (typeQuals t2 <= typeQuals t1) $
                --       typeError ni $
                --       "incompatible qualifiers in pointer assignment: "
                --       ++ pType t1 ++ ", " ++ pType t2
    (DirectType (TyComp c1) _, DirectType (TyComp c2) _)
      | sueRef c1 == sueRef c2 -> return ()
      | otherwise -> typeError ni $
                     "incompatible compound types in assignment: "
                     ++ pType t1 ++ ", " ++ pType t2
    (DirectType (TyBuiltin TyVaList) _, DirectType (TyBuiltin TyVaList) _) ->
      return ()
    (DirectType tn1 _, DirectType tn2 _)
      | isJust (arithmeticConversion tn1 tn2) -> return ()
      | otherwise -> typeError ni $ "incompatible direct types in assignment: "
                     ++ pType t1 ++ ", " ++ pType t2
    (t1', t2') -> compatible ni t1' t2'
assignCompatible ni op t1 t2 = binopType ni (assignBinop op) t1 t2 >> return ()

-- | Determine whether two types are compatible.
compatible :: MonadTrav m => NodeInfo -> Type -> Type -> m ()
compatible ni t1 t2 =
  compositeType ni (deepDerefTypeDef t1) (deepDerefTypeDef t2) >> return ()

-- | Determine the composite type of two compatible types.
compositeType :: MonadTrav m => NodeInfo -> Type -> Type -> m Type
compositeType _ t1 (DirectType (TyBuiltin TyAny) _) = return t1
compositeType _ (DirectType (TyBuiltin TyAny) _) t2 = return t2
compositeType ni t1@(DirectType tn1 q1) t2@(DirectType tn2 q2) =
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
                       typeError ni $ "incompatible composite types"
                                      ++ pType t1 ++ ", " ++ pType t2
                  return tn1
             (TyEnum e1, TyEnum e2) ->
               do when (sueRef e1 /= sueRef e2) $
                       typeError ni $ "incompatible enumeration types"
                                      ++ pType t1 ++ ", " ++ pType t2
                  return $ TyEnum e1
             (TyBuiltin TyVaList, TyBuiltin TyVaList) ->
               return $ TyBuiltin TyVaList
             (TyBuiltin _, TyBuiltin _) ->
               typeError ni $ "incompatible builtin types"
                              ++ pType t1 ++ ", " ++ pType t2
             (_, _) -> typeError ni $ "incompatible direct types: "
                       ++ pType t1 ++ ", " ++ pType t2
     return $ DirectType tn (mergeTypeQuals q1 q2)
compositeType _ (PtrType t1 q1 a1) t2 | isIntegralType t2 =
  return $ PtrType t1 (mergeTypeQuals q1 (typeQuals t2)) a1
compositeType _ t1 (PtrType t2 q2 a2) | isIntegralType t1 =
  return $ PtrType t2 (mergeTypeQuals (typeQuals t1) q2) a2
compositeType ni (ArrayType t1 s1 q1 a1) (ArrayType t2 s2 q2 a2) =
  do t <- compositeType ni t1 t2
     s <- compositeSize ni s1 s2
     let quals = mergeTypeQuals q1 q2
         attrs = mergeAttrs a1 a2
     return (ArrayType t s quals attrs)
compositeType ni t1 t2 | isPointerType t1 && isPointerType t2 =
  do t <- compositeType ni (baseType t1) (baseType t2)
     let quals = mergeTypeQuals (typeQuals t1) (typeQuals t2)
         attrs = mergeAttrs (typeAttrs t1) (typeAttrs t2)
     return (PtrType t quals attrs)
compositeType ni (TypeDefType tdr1) (TypeDefType tdr2) =
  case (tdr1, tdr2) of
    (TypeDefRef i1 Nothing _, TypeDefRef i2 _ _) -> doTypeDef i1 i2 tdr1
    (TypeDefRef i1 _ _, TypeDefRef i2 Nothing _) -> doTypeDef i1 i2 tdr2
    (TypeDefRef _ (Just t1) _, TypeDefRef _ (Just t2) _) ->
      compositeType ni t1 t2
  where doTypeDef i1 i2 tdr =
          do when (i1 /= i2) $ typeError ni $ "incompatible typedef types: "
                               ++ identToString i1 ++ ", " ++ identToString i2
             return (TypeDefType tdr)
compositeType ni (FunctionType ft1) (FunctionType ft2) =
  case (ft1, ft2) of
    (FunType rt1 args1 varargs1 attrs1, FunType rt2 args2 varargs2 attrs2) ->
      do when (length args1 /= length args2) $
              typeError ni "different numbers of arguments in function types"
         args <- mapM (uncurry (compositeParamDecl ni)) (zip args1 args2)
         when (varargs1 /= varargs2) $
              typeError ni "incompatible varargs declarations"
         doFunType rt1 rt2 args varargs1 attrs1 attrs2
    (FunType rt1 args1 varargs1 attrs1, FunTypeIncomplete rt2 attrs2) ->
      doFunType rt1 rt2 args1 varargs1 attrs1 attrs2
    (FunTypeIncomplete rt1 attrs1, FunType rt2 args2 varargs2 attrs2) ->
      doFunType rt1 rt2 args2 varargs2 attrs1 attrs2
    (FunTypeIncomplete rt1 attrs1, FunTypeIncomplete rt2 attrs2) ->
      do rt <- compositeType ni rt1 rt2
         return (FunctionType (FunTypeIncomplete rt (mergeAttrs attrs1 attrs2)))
  where doFunType rt1 rt2 args varargs attrs1 attrs2 =
          do rt <- compositeType ni rt1 rt2
             return (FunctionType
                     (FunType rt args varargs (mergeAttrs attrs1 attrs2)))
compositeType ni t1 t2 = typeError ni $ "incompatible types: "
                         ++ pType t1 ++ ", " ++ pType t2

-- XXX: this may not be correct
compositeSize :: MonadTrav m =>
                 NodeInfo -> ArraySize -> ArraySize -> m ArraySize
compositeSize _ (UnknownArraySize _) s2 = return s2
compositeSize _ s1 (UnknownArraySize _) = return s1
compositeSize ni (ArraySize s1 e1) (ArraySize s2 e2)
  | s1 == s2 && sizeEqual e1 e2 = return $ ArraySize s1 e1
  | otherwise =
    typeError ni $ "incompatible array sizes: "
                   ++ (render . pretty) e1 ++ ", " ++ (render . pretty) e2

sizeEqual :: CExpr -> CExpr -> Bool
sizeEqual (CConst (CIntConst i1 _)) (CConst (CIntConst i2 _)) = i1 == i2
sizeEqual _ _ = False

mergeAttrs :: Attributes -> Attributes -> Attributes
mergeAttrs = (++) -- XXX: ultimately this should be smarter

compositeParamDecl :: MonadTrav m =>
                      NodeInfo -> ParamDecl -> ParamDecl -> m ParamDecl
compositeParamDecl ni (ParamDecl vd1 ni1) (ParamDecl vd2 _) =
  compositeParamDecl' ni ParamDecl vd1 vd2 ni1
compositeParamDecl ni (AbstractParamDecl vd1 _) (ParamDecl vd2 ni2) =
  compositeParamDecl' ni ParamDecl vd1 vd2 ni2
compositeParamDecl ni (ParamDecl vd1 ni1) (AbstractParamDecl vd2 _) =
  compositeParamDecl' ni ParamDecl vd1 vd2 ni1
compositeParamDecl ni (AbstractParamDecl vd1 ni1) (AbstractParamDecl vd2 _) =
  compositeParamDecl' ni AbstractParamDecl vd1 vd2 ni1

compositeParamDecl' :: MonadTrav m =>
                       NodeInfo
                    -> (VarDecl -> NodeInfo -> ParamDecl)
                    -> VarDecl
                    -> VarDecl
                    -> NodeInfo
                    -> m ParamDecl
compositeParamDecl' ni f vd1 vd2 dni =
  do vd <- compositeVarDecl ni vd1 vd2
     return $ f vd dni

compositeVarDecl :: MonadTrav m => NodeInfo -> VarDecl -> VarDecl -> m VarDecl
compositeVarDecl ni (VarDecl n1 attrs1 t1) (VarDecl _ attrs2 t2) =
  do t <- compositeType ni t1 t2
     return (VarDecl n1 (compositeDeclAttrs attrs1 attrs2) t)

-- XXX: bad treatement of inline and storage
compositeDeclAttrs :: DeclAttrs -> DeclAttrs -> DeclAttrs
compositeDeclAttrs (DeclAttrs inl stor attrs1) (DeclAttrs _ _ attrs2) =
  DeclAttrs inl stor (mergeAttrs attrs1 attrs2)

-- return @Just declspecs@ without @CTypedef@ if the declaration specifier contain @typedef@
hasTypeDef :: [CDeclSpec] -> Maybe [CDeclSpec]
hasTypeDef declspecs =
    case foldr hasTypeDefSpec (False,[]) declspecs of
        (True,specs') -> Just specs'
        (False,_)     -> Nothing
    where
    hasTypeDefSpec (CStorageSpec (CTypedef n)) (_,specs) = (True, specs)
    hasTypeDefSpec spec (b,specs) = (b,spec:specs)
