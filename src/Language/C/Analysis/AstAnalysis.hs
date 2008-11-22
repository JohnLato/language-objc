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
    analyseAST,
    analyseExt,analyseFunDef,analyseDecl,
)
where
import Language.C.Analysis.SemError
import Language.C.Analysis.SemRep
import Language.C.Analysis.TravMonad
import Language.C.Analysis.DefTable (globalDefs, defineScopedIdent, defineLabel)
import Language.C.Analysis.DeclAnalysis

import Language.C.Data
import Language.C.Syntax

import Control.Monad
import Prelude hiding (reverse)
import Data.Foldable (foldrM)
import Data.List hiding (reverse)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace


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
    mapRecoverM_ analyseExt decls
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
    -- compute storage
    fun_storage <- computeFunDefStorage ident storage_spec
    let var_decl = VarDecl name (DeclAttrs is_inline fun_storage attrs) ty
    -- improve incomplete type
    ty' <- improveFunDefType ty
    -- callback for declaration
    handleVarDecl False (Decl var_decl node_info)
    -- process body
    enterFunctionScope
    -- add parameters again
    case ty' of
      FunctionType (FunType _ params _ _) -> mapM_ addParam params
      _                                   -> astError node_info
                                             "function has non-function type"
                                             -- XXX: internal error
    -- translate the body
    stmt' <- analyseFunctionBody var_decl stmt
    -- callback for definition
    handleFunDef ident (FunDef var_decl stmt' node_info)
    leaveFunctionScope
    where
    improveFunDefType (FunctionType (FunTypeIncomplete return_ty attrs)) =
      return . FunctionType $ FunType return_ty [] False attrs
    improveFunDefType ty = return $ ty
    -- XXX: defineScopedIdentWhen?
    addDecl d = withDefTable $ defineScopedIdent (declIdent d) d
    addParam (ParamDecl d ni) = addDecl $ Declaration $ Decl d ni
    addParam (AbstractParamDecl d ni) = addDecl $ Declaration $ Decl d ni

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
        init_opt' <- mapMaybeM init_opt tInit
        when (isTypeOfExpr typ) $ astError node "we cannot analyse typeof(expr) yet"
        if (isFunctionType typ)
            then extFunProto vardeclInfo
            else (if is_local then localVarDecl else extVarDecl)
                 vardeclInfo init_opt'
    analyseVarDeclr _ (Nothing,_,_)         = astError node "abstract declarator in object declaration"
    analyseVarDeclr _ (_,_,Just bitfieldSz) = astError node "bitfield size in object declaration"

    isTypeOfExpr (TypeOfExpr _) = True
    isTypeOfExpr _ = False

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

-- | handle a function prototype
extFunProto :: (MonadTrav m) => VarDeclInfo -> m ()
extFunProto (VarDeclInfo var_name is_inline storage_spec attrs ty node_info) =
    do  old_fun <- lookupObject (identOfVarName var_name)
        checkValidSpecs
        let decl = VarDecl var_name (DeclAttrs is_inline (funDeclLinkage old_fun) attrs) ty
        handleVarDecl False (Decl decl node_info)
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
        | RegSpec <- storage_spec        = astError node_info "invalid `register' storage specified for function"
        | otherwise                      = return ()

-- | handle a object declaration \/ definition
--
-- We have to check the storage specifiers here, as they determine wheter we're dealing with decalartions
-- or definitions
-- see [doc\/ExternalDefinitions.txt]
extVarDecl :: (MonadTrav m) => VarDeclInfo -> (Maybe Initializer) -> m ()
extVarDecl (VarDeclInfo var_name is_inline storage_spec attrs typ node_info) init_opt =
    do let ident = identOfVarName var_name
       old_decl <- lookupObject ident
       checkValidVarDeclStorage
       let vardecl linkage = VarDecl var_name (DeclAttrs is_inline linkage attrs) typ
       let decl linkage = Decl (vardecl linkage) node_info
       case storage_spec of
           NoStorageSpec           -- tentative if there is no initializer, external
               -> handleObjectDef False ident $ ObjDef (vardecl (Static ExternalLinkage False)) init_opt node_info
           StaticSpec thread_local -- tentative if there is no initializer, internal
               -> handleObjectDef False ident $ ObjDef (vardecl (Static InternalLinkage thread_local)) init_opt node_info
           ExternSpec thread_local
             | Nothing <- init_opt  -- declaration with either external or old storage
               -> handleVarDecl False $ decl $ maybe (Static ExternalLinkage thread_local) declStorage old_decl
             | otherwise            -- warning, external definition
               -> do warn $ badSpecifierError node_info
                            "Both initializer and `extern` specifier given - treating as definition"
                     handleObjectDef False ident $ ObjDef (vardecl (Static ExternalLinkage thread_local)) init_opt node_info
           _ -> error$ "extVarDecl: storage_spec: "++show storage_spec
    where
    checkValidVarDeclStorage
        | is_inline               = astError node_info "invalid `inline' specifier for non-function"
        | RegSpec <- storage_spec = astError node_info "invalid `register' storage specified for external object"
        | otherwise               = return ()

-- | handle a function-scope object declaration \/ definition
localVarDecl :: (MonadTrav m) => VarDeclInfo -> (Maybe Initializer) -> m ()
localVarDecl (VarDeclInfo var_name is_inline storage_spec attrs typ node_info) init_opt =
    do storage <- localStorage storage_spec
       let vardecl = VarDecl var_name (DeclAttrs is_inline storage attrs) typ
       case init_opt of
           Nothing -> handleVarDecl True (Decl vardecl node_info)
           Just _  -> handleObjectDef True ident (ObjDef vardecl init_opt node_info)
    where
    ident = identOfVarName var_name
    localStorage _
      | is_inline = astError node_info "invalid `inline' specifier for local variable"
    localStorage NoStorageSpec = return $ Auto False
    localStorage RegSpec = return $ Auto True
    localStorage (StaticSpec thread_local) =
      -- all local variables have internal linkage
      -- XXX: is this true?
      return $ Static InternalLinkage thread_local
    localStorage _ = astError node_info "bad storage specifier for local"

analyseFunctionBody :: (MonadTrav m) => VarDecl -> CStat -> m Stmt
analyseFunctionBody _ s =
  do mapM (withDefTable . defineLabel) (getLabels s)
     tStmt s

getLabels :: CStat -> [Ident]
getLabels (CLabel l s _ _)      = l : getLabels s
getLabels (CCase _ s _)         = getLabels s
getLabels (CCases _ _ s _)      = getLabels s
getLabels (CDefault s _)        = getLabels s
getLabels (CExpr _ _)           = []
getLabels (CCompound _ body _)  = concatMap getBILabels body
  where getBILabels (CBlockStmt s)    = getLabels s
        getBILabels (CBlockDecl _)    = []
        getBILabels (CNestedFunDef _) = []
getLabels (CIf _ sthen selse _) = getLabels sthen ++ maybe [] getLabels selse
getLabels (CSwitch _ s _)       = getLabels s
getLabels (CWhile _ s _ _)      = getLabels s
getLabels (CFor _ _ _ s _)      = getLabels s
getLabels (CGoto _ _)           = []
getLabels (CGotoPtr _ _)        = []
getLabels (CCont _)             = []
getLabels (CBreak _)            = []
getLabels (CReturn _ _)         = []
getLabels (CAsm _ _)            = []

analyseBlockItem :: (MonadTrav m) => CBlockItem -> m ()
analyseBlockItem (CBlockStmt s) = tStmt s >> return ()
analyseBlockItem (CBlockDecl d) = analyseDecl True d
-- TODO: fixup analyseFunDef to handle nested functions
analyseBlockItem (CNestedFunDef fd) = analyseFunDef fd

-- | /TODO/: Bogus: only handles compound blocks, to get at declarations
tStmt :: (MonadTrav m) => CStat -> m Stmt
tStmt s@(CCompound _ items _) =
  do enterBlockScope
     mapM_ analyseBlockItem items
     leaveBlockScope
     return s
tStmt s = return s

-- | /TODO/: Bogus
tExpr :: (MonadTrav m) => CExpr -> m Expr
tExpr = return

-- | /TODO/: Bogus
tInit :: (MonadTrav m) => CInit -> m Initializer
tInit = return


-- return @Just declspecs@ without @CTypedef@ if the declaration specifier contain @typedef@
hasTypeDef :: [CDeclSpec] -> Maybe [CDeclSpec]
hasTypeDef declspecs =
    case foldr hasTypeDefSpec (False,[]) declspecs of
        (True,specs') -> Just specs'
        (False,_)     -> Nothing
    where
    hasTypeDefSpec (CStorageSpec (CTypedef n)) (_,specs) = (True, specs)
    hasTypeDefSpec spec (b,specs) = (b,spec:specs)
