{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Parser.Translation
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable (ScopedTypeVariables,PatternGuards)
-- Stability   :  alpha
--
-- Analyse the parse tree
--
-- Traverses the AST, analyses declarations and invokes handlers.
-----------------------------------------------------------------------------
module Language.C.Analysis.AstAnalysis (
    analyseAST,
    analyseExt,analyseFunDef,analyseExtDecls,
)
where
import Language.C.Analysis.SemError
import Language.C.Analysis.SemRep
import Language.C.Analysis.TravMonad
import Language.C.Analysis.DefTable
import Language.C.Analysis.DeclAnalysis

import Language.C.Data.Node
import Language.C.Data.Ident
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
analyseExt (CAsmExt asm)
    = handleAsmBlock (convertStringLit asm)
analyseExt (CFDefExt fundef)
    = analyseFunDef fundef
analyseExt (CDeclExt decls)
    = analyseExtDecls decls

-- | Analyse a function definition
-- data VarDeclInfo = VarDeclInfo VarName Bool {- is-inline? -} StorageSpec Attributes Type NodeInfo

analyseFunDef :: (MonadTrav m) => CFunDef -> m ()
analyseFunDef (CFunDef declspecs declr oldstyle_decls stmt node_info) = do
    -- analyse the declarator
    var_decl_info <- analyseVarDecl True declspecs declr oldstyle_decls
    let (VarDeclInfo name is_inline storage_spec attrs ty declr_node) = var_decl_info
    let ident = identOfVarName name
    fun_storage <- computeFunDefStorage ident storage_spec
    let var_decl = VarDecl name (DeclAttrs is_inline fun_storage attrs) ty
    handleVarDecl (Decl var_decl node_info)
    -- translate the body
    stmt' <- analyseFunctionBody var_decl stmt
    -- define the function
    handleFunDef ident (FunDef var_decl stmt' node_info)

-- | Analyse a top-level declaration other than a function definition
analyseExtDecls :: (MonadTrav m) => CDecl -> m ()
analyseExtDecls decl@(CDecl declspecs declrs node)
    | (Just declspecs') <- isTypeDef declspecs =
        case declrs of
            [(Just tydeclr,Nothing,Nothing)] -> analyseTypeDef declspecs' tydeclr node
            _ -> astError node "bad typdef declaration: declarator missing or bitfieldsize/initializer present"
    | null declrs = analyseTypeDecl decl >> return ()
    | otherwise   = mapM_ (uncurry convertVarDeclr) $ zip (True : repeat False) declrs
    where
    convertVarDeclr handle_sue_def (Just declr, init_opt, Nothing) = do
        -- analyse the declarator
        vardeclInfo@(VarDeclInfo _ _ _ _ typ _) <- analyseVarDecl handle_sue_def declspecs declr []
        -- declare / define the object
        init_opt' <- mapMaybeM init_opt tInit
        when (isTypeOfExpr typ) $ astError node "we cannot analyse typeof(expr) yet"
        if (isFunctionType typ)
            then extFunProto vardeclInfo
            else extVarDecl vardeclInfo init_opt'
    convertVarDeclr _ (Nothing,_,_)         = astError node "abstract declarator in object declaration"
    convertVarDeclr _ (_,_,Just bitfieldSz) = astError node "bitfield size in object declaration"
    isTypeOfExpr (TypeOfExpr _) = True
    isTypeOfExpr _ = False

-- convert typeDef
analyseTypeDef :: (MonadTrav m) => [CDeclSpec] -> CDeclr -> NodeInfo -> m ()
analyseTypeDef declspecs declr node_info = do
    -- analyse the declarator
    (VarDeclInfo name is_inline storage_spec attrs ty declr_node) <- analyseVarDecl True declspecs declr []
    -- TODO: move attribute to the type
    checkValidTypeDef is_inline storage_spec attrs
    let ident = identOfVarName name
    handleTypeDef (TypeDef ident ty attrs node_info)
    where
    checkValidTypeDef True _ _ = astError node_info "inline specifier for typeDef"
    checkValidTypeDef _ _ (_:_) = astError node_info "attributes for typeDefs aren't supported"
    checkValidTypeDef _ NoStorageSpec _ = return ()
    checkValidTypeDef _ bad_storage _ = astError node_info $ "storage specified for typeDef: " ++ show bad_storage

-- | analyse declarators
analyseVarDecl :: (MonadTrav m) => Bool -> [CDeclSpec] -> CDeclr -> [CDecl] -> m VarDeclInfo
analyseVarDecl handle_sue_def declspecs declr oldstyle_params = do
    let (storagespecs, decl_attrs, typequals, typespecs, inline) = partitionDeclSpecs declspecs
    -- analyse the storage specifiers
    storage_spec  <- canonicalStorageSpec storagespecs
    -- translate the type into semantic representation
    typ          <- tType handle_sue_def node typequals typespecs derived_declrs oldstyle_params
    -- translate attributes
    attrs'       <- mapM tAttr (decl_attrs ++ declr_attrs)
    -- create the variable name
    name         <- mkVarName node nameOpt (fmap convertStringLit asmname_opt)
    return $ VarDeclInfo name inline storage_spec attrs' typ node
    where
    (CDeclr nameOpt derived_declrs asmname_opt declr_attrs node) = declr
    isInlineSpec (CInlineQual _) = True
    isInlineSpec _ = False

-- | compute storage of a function definition
--
-- a function definition has static storage with internal linkage if specified `static`,
-- the previously declared linkage if any if 'extern' and 'extern' otherwise
--
-- This function won't raise an error, even if the function is already declared, because
-- the error handling should be done in 'handleFunDef'
computeFunDefStorage :: (MonadTrav m) => Ident -> StorageSpec -> m Storage
computeFunDefStorage _ NoStorageSpec = return$ Static ExternalLinkage False
computeFunDefStorage _ ThreadSpec    = return$ Static ExternalLinkage True
computeFunDefStorage _ (StaticSpec b)  = return$ Static InternalLinkage b
computeFunDefStorage ident (ExternSpec b)  = do
     obj_opt <- lookupObject ident
     return$ case obj_opt of
                Just vardecl -> declStorage $ vardecl
                _ -> Static ExternalLinkage b
computeFunDefStorage _ bad_spec = error $ "unexpected storage specifier: "++show bad_spec

-- | handle a function prototype
extFunProto :: (MonadTrav m) => VarDeclInfo -> m ()
extFunProto (VarDeclInfo var_name is_inline storage_spec attrs typ node_info) =
    do  old_fun <- lookupObject (identOfVarName var_name)
        checkValidFunDecl
        let decl = VarDecl var_name (DeclAttrs is_inline (funDeclLinkage old_fun) attrs) typ
        handleVarDecl (Decl decl node_info)
    where
    funDeclLinkage old_fun =
        case storage_spec of
            NoStorageSpec    -> FunLinkage ExternalLinkage -- prototype declaration / external linkage
            StaticSpec False -> FunLinkage InternalLinkage -- prototype declaration / internal linkage
            ExternSpec False -> case old_fun of
                                    Nothing -> FunLinkage ExternalLinkage
                                    Just f  -> declStorage f
            _ -> error $ "funDeclLinkage: " ++ show storage_spec
    checkValidFunDecl
        | isThreadLocalSpec storage_spec = astError node_info "thread local storage specified for function"
        | RegSpec <- storage_spec        = astError node_info "invalid `register' storage specified for function"
        | (not $ isFunctionType typ)     = error "function prototype without function type"
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
               -> handleObjectDef ident $ ObjDef (vardecl (Static ExternalLinkage False)) init_opt node_info
           StaticSpec thread_local -- tentative if there is no initializer, internal
               -> handleObjectDef ident $ ObjDef (vardecl (Static InternalLinkage thread_local)) init_opt node_info
           ExternSpec thread_local
             | Nothing <- init_opt  -- declaration with either external or old storage
               -> handleVarDecl $ decl $ maybe (Static ExternalLinkage thread_local) declStorage old_decl
             | otherwise            -- warning, external definition
               -> do warn $ badSpecifierError node_info
                            "Both initializer and `extern` specifier given - treating as definition"
                     handleObjectDef ident $ ObjDef (vardecl (Static ExternalLinkage thread_local)) init_opt node_info
           _ -> error$ "extVarDecl: storage_spec: "++show storage_spec
    where
    checkValidVarDeclStorage
        | is_inline               = astError node_info "invalide `inline' specifier for non-function"
        | RegSpec <- storage_spec = astError node_info "invalid `register' storage specified for external object"
        | otherwise               = return ()

-- | /TODO/: Bogus
analyseFunctionBody :: (MonadTrav m) => VarDecl -> CStat -> m Stmt
analyseFunctionBody _ = return
-- | /TODO/: Bogus
tStmt :: (MonadTrav m) => CStat -> m Stmt
tStmt = return
-- | /TODO/: Bogus
tExpr :: (MonadTrav m) => CExpr -> m Expr
tExpr = return
-- | /TODO/: Bogus
tInit :: (MonadTrav m) => CInit -> m Initializer
tInit = return


-- return @Just declspecs\\typeDef@ if the declaration is a typeDef
isTypeDef :: [CDeclSpec] -> Maybe [CDeclSpec]
isTypeDef declspecs =
    case foldr isTypeDefSpec (False,[]) declspecs of
        (True,specs') -> Just specs'
        (False,_)     -> Nothing
    where
    isTypeDefSpec (CStorageSpec (CTypedef n)) (_,specs) = (True, specs)
    isTypeDefSpec spec (b,specs) = (b,spec:specs)
