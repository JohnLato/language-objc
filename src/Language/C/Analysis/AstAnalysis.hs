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
    -- * Building blocks for additional analyses
    analyseFunctionBody,
    defineParams,
    -- * Type checking
    tExpr, ExprSide(..),
    tStmt, StmtCtx(..)
)
where
import Language.C.Analysis.SemError
import Language.C.Analysis.SemRep
import Language.C.Analysis.TravMonad
import Language.C.Analysis.Debug
import Language.C.Analysis.DefTable (DefTable, globalDefs, defineScopedIdent,
                                     defineLabel, inFileScope, lookupTag,
                                     lookupLabel)
import Language.C.Analysis.DeclAnalysis
import Language.C.Analysis.TypeUtils
import Language.C.Analysis.TypeCheck
import Language.C.Analysis.TypeConversions

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
import qualified Data.Map as Map
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
    when (isNoName name) $ astError node_info "NoName in analyseFunDef"
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
    when (isNoName name) $ astError node_info "NoName in analyseTypeDef"
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
    do  when (isNoName var_name) $ astError node_info "NoName in extFunProto"
        old_fun <- lookupObject (identOfVarName var_name)
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
    do when (isNoName var_name) $ astError node_info "NoName in extVarDecl"
       (storage,is_def) <- globalStorage storage_spec
       let vardecl = VarDecl var_name (DeclAttrs is_inline storage attrs) typ
       if is_def
           then handleObjectDef False ident $ ObjDef vardecl init_opt node_info
           else handleVarDecl False $ Decl vardecl node_info
    where
       ident = identOfVarName var_name
       globalStorage _ | is_inline = astError node_info "invalid `inline' specifier external variable"
       globalStorage RegSpec       =
         do when (isJust init_opt) $ astError node_info "initializer given for global register variable"
            case var_name of
              NoName -> astError node_info "global register variable has no name"
              VarName _ Nothing -> astError node_info "no register specified for global register variable"
              _ -> return ()
            dt <- getDefTable
            when (hasFunDef dt) $ astError node_info "global register variable appears after a function definition"
            return (Static InternalLinkage False, False)
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
       hasFunDef dt = any (isFuncDef . snd) (Map.toList $ gObjs $ globalDefs dt)
       isFuncDef (FunctionDef fd) = not $ isInline $ declAttrs fd
       isFuncDef _ = False
       isInline (DeclAttrs inl _ _) = inl

-- | handle a function-scope object declaration \/ definition
-- see [http://www.sivity.net/projects/language.c/wiki/LocalDefinitions]
localVarDecl :: (MonadTrav m) => VarDeclInfo -> (Maybe Initializer) -> m ()
localVarDecl (VarDeclInfo var_name is_inline storage_spec attrs typ node_info) init_opt =
    do when (isNoName var_name) $ astError node_info "NoName in localVarDecl"
       (storage,is_def) <- localStorage storage_spec
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

defineParams :: MonadTrav m => NodeInfo -> VarDecl -> m ()
defineParams ni decl =
  case (getParams $ declType decl) of
    Nothing -> astError ni
               "expecting complete function type in function definition"
    Just params -> mapM_ handleParamDecl params

analyseFunctionBody :: (MonadTrav m) => NodeInfo -> VarDecl -> CStat -> m Stmt
analyseFunctionBody node_info decl s@(CCompound localLabels items _) =
  do enterFunctionScope
     mapM_ (withDefTable . defineLabel) (localLabels ++ getLabels s)
     defineParams node_info decl
     -- record parameters
     mapM_ (tBlockItem [FunCtx decl]) items
     leaveFunctionScope
     return s -- XXX: bogus

analyseFunctionBody _ _ s = astError (nodeInfo s) "Function body is no compound statement"

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
     case (rt, t) of
       -- apparently it's ok to return void from a void function?
       (DirectType TyVoid _, DirectType TyVoid _) -> return ()
       _ -> assignCompatible ni CAssignOp rt t
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

varAddrType :: MonadTrav m => NodeInfo -> IdentDecl -> m Type
varAddrType ni d =
  do case s of
       Auto True -> typeError ni "address of register variable"
       _         -> return ()
     case t of
       ArrayType _ _ q a -> return $ PtrType t q a
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
       CCompoundLit _ _ _ -> simplePtr `liftM` tExpr RValue e
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
     checkScalar (nodeInfo e1) t1
     t3 <- tExpr side e3
     case me2 of
       Just e2 ->
         do t2 <- tExpr side e2
            conditionalType ni t2 t3
       Nothing -> conditionalType ni t1 t3
tExpr side (CMember e m deref ni)   =
  do t <- tExpr RValue e
     bt <- if deref then derefType ni t else return t
     ft <- fieldType ni m bt
     return $ fixup ft
  where fixup | side == RValue = handleArray
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
     tInitList ni (canonicalType lt) initList
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
                  declType d
                | otherwise = return $ declType d
tExpr _ (CConst c)                  = constType c
tExpr _ (CBuiltinExpr b)            = builtinType b
tExpr side (CCall (CVar i _) args ni)
  | identToString i == "__builtin_choose_expr" =
    case args of
      [g, e1, e2] -> do checkGuard g
                        t1 <- tExpr RValue e1
                        t2 <- tExpr RValue e2
                        conditionalType ni t1 t2
-- XXX: the following is the proper way to typecheck this, but it depends
-- on constant expression evaluation.
{-
      [g, e1, e2] -> c <- constEval g
                     case boolValue c of
                       Just True -> tExpr side e1
                       Just False -> tExpr side e2
                       Nothing ->
                         astError ni
                         "non-constant argument to __builtin_choose_expr"
-}
      _ -> astError ni "wrong number of arguments to __builtin_choose_expr"
tExpr _ (CCall fe args ni)          =
  do let defType = FunctionType
                   (FunTypeIncomplete
                    (DirectType (TyIntegral TyInt) noTypeQuals) [])
         fallback i = do warn $ invalidAST ni $
                                "unknown function: " ++ identToString i
                         return defType
     t <- case fe of
            CVar i _ -> lookupObject i >>=
                        maybe (fallback i) (const $ tExpr RValue fe)
            _ -> tExpr RValue fe
     atys <- mapM (tExpr RValue) args
     -- XXX: we don't actually want to return the canonical return type here
     case canonicalType t of
       PtrType (FunctionType (FunType rt pdecls varargs _)) _ _ ->
         do let ptys = map declType pdecls
            mapM_ (uncurry (assignCompatible ni CAssignOp)) (zip ptys atys)
            unless varargs $ when (length atys /= length ptys) $
                   typeError ni "incorrect number of arguments"
            return $ canonicalType rt
       PtrType (FunctionType (FunTypeIncomplete rt _)) _ _ ->
         do -- warn $ invalidAST ni "incomplete function type"
            return $ canonicalType rt
       _  -> typeError ni $ "attempt to call non-function of type " ++ pType t
tExpr _ (CAssign op le re ni)       =
  do lt <- tExpr LValue le
     rt <- tExpr RValue re
     when (constant $ typeQuals lt) $
          typeError ni $ "assignment to lvalue with `constant' qualifier: "
                         ++ (render . pretty) le
     case (canonicalType lt, re) of
       (lt', CConst (CIntConst i _))
         | isPointerType lt' && getCInteger i == 0 -> return ()
       (_, _) -> assignCompatible ni op lt rt
     return lt
tExpr _ (CStatExpr s _)             = tStmt [] s

tInitList :: MonadTrav m => NodeInfo -> Type -> CInitList -> m ()
tInitList ni t@(ArrayType (DirectType (TyIntegral TyChar) _) _ _ _)
             [([], CInitExpr e@(CConst (CStrConst _ _)) _)] =
  tExpr RValue e >> return ()
tInitList ni t@(ArrayType _ _ _ _) initList =
  do let default_ds =
           repeat (CArrDesig (CConst (CIntConst (cInteger 0) ni)) ni)
     checkInits t default_ds initList
tInitList ni t@(DirectType (TyComp ctr) _) initList =
  do td <- lookupSUE ni (sueRef ctr)
     ms <- tagMembers ni td
     let default_ds = map (\m -> CMemberDesig (fst m) ni) ms
     checkInits t default_ds initList
tInitList ni (PtrType (DirectType TyVoid _) _ _ ) _ =
          return () -- XXX: more checking
tInitList _ t [([], i)] = tInit t i >> return ()
tInitList ni t _ = typeError ni $ "initializer list for type: " ++ pType t

checkInits :: MonadTrav m => Type -> [CDesignator] -> CInitList -> m ()
checkInits _ _ [] = return ()
checkInits t dds ((ds, i) : is) =
  do (dds', ds') <- case (dds, ds) of
                      ([], []) ->
                        typeError (nodeInfo i) "excess elements in initializer"
                      (dd' : rest, []) -> return (rest, [dd'])
                      (_, d : _) -> return (advanceDesigList dds d, ds)
     t' <- tDesignator t ds'
     tInit t' i
     checkInits t dds' is

advanceDesigList :: [CDesignator] -> CDesignator -> [CDesignator]
advanceDesigList ds d = drop 1 $ dropWhile (not . matchDesignator d) ds

matchDesignator :: CDesignator -> CDesignator -> Bool
matchDesignator (CMemberDesig m1 _) (CMemberDesig m2 _) = m1 == m2
matchDesignator _ _ = True -- XXX: for now, array ranges aren't checked

tDesignator :: MonadTrav m => Type -> [CDesignator] -> m Type
-- XXX: check that initializers are within array size
tDesignator (ArrayType bt _ _ _) (CArrDesig e ni : ds) =
  do tExpr RValue e >>= checkIntegral ni
     tDesignator bt ds
tDesignator (ArrayType bt _ _ _) (CRangeDesig e1 e2 ni : ds) =
  do tExpr RValue e1 >>= checkIntegral ni
     tExpr RValue e2 >>= checkIntegral ni
     tDesignator bt ds
tDesignator (ArrayType _ _ _ _) (d : ds) =
  typeError (nodeInfo d) "member designator in array initializer"
tDesignator t@(DirectType (TyComp _) _) (CMemberDesig m ni : ds) =
  do mt <- fieldType ni m t
     tDesignator (canonicalType mt) ds
tDesignator t@(DirectType (TyComp _) _) (d : _) =
  typeError (nodeInfo d) "array designator in compound initializer"
tDesignator t [] = return t

tInit :: MonadTrav m => Type -> CInit -> m Initializer
tInit t i@(CInitExpr e ni) =
  do it <- tExpr RValue e
     assignCompatible ni CAssignOp t it
     return i
tInit t i@(CInitList initList ni) =
  tInitList ni (canonicalType t) initList >> return i

complexBaseType :: MonadTrav m => NodeInfo -> ExprSide -> CExpr -> m Type
complexBaseType ni side e =
  do t <- tExpr side e
     case canonicalType t of
       DirectType (TyComplex ft) quals ->
         return $ DirectType (TyFloating ft) quals
       _ -> typeError ni $ "expected complex type, got: " ++ pType t


-- | Return the type of a builtin.
builtinType :: MonadTrav m => CBuiltin -> m Type
builtinType (CBuiltinVaArg _ d _)           = analyseTypeDecl d
builtinType (CBuiltinOffsetOf _ _ _)        = return sizeofType
builtinType (CBuiltinTypesCompatible _ _ _) = return boolType

-- return @Just declspecs@ without @CTypedef@ if the declaration specifier contain @typedef@
hasTypeDef :: [CDeclSpec] -> Maybe [CDeclSpec]
hasTypeDef declspecs =
    case foldr hasTypeDefSpec (False,[]) declspecs of
        (True,specs') -> Just specs'
        (False,_)     -> Nothing
    where
    hasTypeDefSpec (CStorageSpec (CTypedef n)) (_,specs) = (True, specs)
    hasTypeDefSpec spec (b,specs) = (b,spec:specs)
