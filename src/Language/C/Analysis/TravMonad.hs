{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts,FlexibleInstances,
             PatternSignatures, PatternGuards, RankNTypes, ScopedTypeVariables #-} 
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis.TravMonad
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  non-portable (mtl)
--
-- Monad for Traversals of the C AST.
-- 
-- For the traversal, we maintain a symboltable and need MonadError and unique
-- name generation facilities.
-- Furthermore, the user may provide callbacks to handle declarations and definitions.
-----------------------------------------------------------------------------
module Language.C.Analysis.TravMonad (
    -- * AST traversal monad
    MonadTrav(..),
    -- * handling declarations
    handleTagDef,handleEnumeratorDef,handleTypeDef,
    handleFunDef,handleVarDecl,handleObjectDef,
    handleAsmBlock,
    -- * symbol table scope modification
    enterPrototypeScope,leavePrototypeScope,
    enterFunctionScope,leaveFunctionScope,
    enterBlockScope,leaveBlockScope,
    -- * symbol table lookup (delegate)
    lookupTypeDef, lookupVarDecl, lookupObject, lookupFun,
    -- * symbol table modification
    createSUERef,
    -- * additional error handling facilities
    hadHardErrors,handleTravError,throwOnLeft,
    astError, warn,
    -- * Trav - default MonadTrav implementation
    Trav(..),
    runTrav,runTrav_,
    TravState,initTravState,withExtDeclHandler,modifyUserState,userState,
    -- * helpers
    mapMaybeM,maybeM,mapSndM,concatMapM,
)
where
import Language.C.Data
import Language.C.Data.RList as RList
import Language.C.Syntax

import Language.C.Analysis.SemError
import Language.C.Analysis.SemRep
import Language.C.Analysis.DefTable hiding (enterBlockScope,leaveBlockScope,
                                            enterFunctionScope,leaveFunctionScope)
import qualified Language.C.Analysis.DefTable as ST

import Data.Maybe
import qualified Control.Monad.Error as MtlError
import Control.Monad.State
import Control.Monad.Reader

-- | Traversal monad
class (Monad m) => MonadTrav m where

    -- error handling facilities
    throwTravError :: Error e => e -> m a
    -- we could implement dynamically-typed catch here (deferred)
    catchTravError :: m a -> (CError -> m a) -> m a
    recordError    :: Error e => e -> m ()
    getErrors      :: m [CError]

    -- symbol table handling
    getDefTable :: m DefTable
    withDefTable :: (DefTable -> (a, DefTable)) -> m a

    -- unique name generation
    genName :: m Name

    -- handling declarations and definitions
    handleDecl :: DeclEvent -> m ()
    
-- * handling declarations
checkRedef :: (MonadTrav m, CNode t, CNode t1) => String -> t -> (DeclarationStatus t1) -> m ()
checkRedef subject new_decl redecl_status =
    case redecl_status of
        NewDecl -> return ()
        Redeclared old_def   -> throwTravError $
            redefinition LevelError subject DuplicateDef (nodeInfo new_decl) (nodeInfo old_def)
        KindMismatch old_def -> throwTravError $
            redefinition LevelError subject DiffKindRedecl (nodeInfo new_decl) (nodeInfo old_def)
        Shadowed old_def     ->  return ()
            -- warn $ 
            -- redefinition LevelWarn subject ShadowedDef (nodeInfo new_decl) (nodeInfo old_def)
        KeepDef _old_def      -> return ()

-- | define the given composite type or enumeration in the current namespace
-- If there is already a definition present, yield an error (redeclaration)
handleTagDef :: (MonadTrav m) => TagDef -> m ()
handleTagDef def = do
    redecl <- withDefTable $ defineTag (sueRef def) def
    checkRedef (show $ sueRef def) def redecl
    handleDecl (TagEvent def)

handleEnumeratorDef :: (MonadTrav m) => Enumerator -> SUERef -> m ()
handleEnumeratorDef enumerator@(ident,_) enum_ref = do
    redecl <- withDefTable $ defineScopedIdent ident (EnumDef enumerator enum_ref)
    checkRedef (show ident) ident redecl
    return ()
    
handleTypeDef :: (MonadTrav m) => TypeDef -> m ()
handleTypeDef typedef@(TypeDef' ident _ _ _) = do
    let def = TypeDef typedef
    redecl <- withDefTable $ defineScopedIdent ident def
    checkRedef (show ident) def redecl
    handleDecl (DeclEvent def)
    return ()
    
handleAsmBlock :: (MonadTrav m) => AsmBlock -> m ()
handleAsmBlock asm = handleDecl (AsmEvent asm)

checkVarRedef :: (MonadTrav m) => IdentDecl -> (DeclarationStatus IdentDecl) -> m ()
checkVarRedef def redecl =
    case redecl of 
        -- always an error
        KindMismatch old_def -> throwTravError $ redef LevelError old_def DiffKindRedecl
        -- types have to match, it is pointless though to declare something already defined
        KeepDef old_def -> do -- when (isDecl def) $ warn (redef LevelWarn old_def DuplicateDef)
                              throwOnLeft $ checkCompatibleTypes new_ty (getTy old_def)
        -- redeclaration: old entry has to be a declaration or tentative, type have to match
        Redeclared old_def | isTentativeG old_def -> 
                             throwOnLeft $ checkCompatibleTypes new_ty (getTy old_def)                                   
                           | otherwise -> throwTravError $ redef LevelError old_def DuplicateDef
        -- NewDecl/Shadowed is ok
        _ -> return ()
    where
    redef lvl old_def kind = redefinition lvl (show$ identOfDecl def) kind (nodeInfo def) (nodeInfo old_def)
    new_ty = getTy def
    getTy (Declaration vd) = declType vd
    getTy (ObjectDef od) = declType od
    getTy (FunctionDef fd) = declType fd
    getTy _ = error "checkVarRedef: not an object or function"
    isDecl (Declaration _) = True
    isDecl _ = False
    isTentativeG (Declaration _) = True
    isTentativeG (ObjectDef od)  = isTentative od
    isTentativeG _               = False
    
-- | handle variable declarations (external object declarations and function prototypes)
-- variable declarations are either function prototypes, or external declarations, and not very
-- interesting on their own. we only put them in the symbol table and call the handle.
-- declarations never override definitions
handleVarDecl :: (MonadTrav m) => Decl -> m ()
handleVarDecl decl = do
    let def = Declaration decl
    redecl <- withDefTable $ 
              defineScopedIdentWhen (const False) (identOfDecl def) def
    checkVarRedef def redecl
    handleDecl (DeclEvent def)
    
-- | handle function definitions
handleFunDef :: (MonadTrav m) => Ident -> FunDef -> m ()
handleFunDef ident fun_def = do
    let def = FunctionDef fun_def
    redecl <- withDefTable $
              defineScopedIdentWhen isDeclaration ident def
    checkVarRedef def redecl
    handleDecl (DeclEvent def)

isDeclaration :: IdentDecl -> Bool
isDeclaration (Declaration _) = True
isDeclaration _ = False

checkCompatibleTypes :: Type -> Type -> Either TypeMismatch ()
checkCompatibleTypes _ _ = Right ()

-- | handle object defintions (maybe tentative)    
handleObjectDef :: (MonadTrav m) => Ident -> ObjDef -> m ()
handleObjectDef ident obj_def = do
    let def = ObjectDef obj_def
    redecl <- withDefTable $
              defineScopedIdentWhen (\o -> shouldOverride def o) ident def
    checkVarRedef def redecl          
    handleDecl (DeclEvent def)
    where
    isTentativeDef (ObjectDef object_def) = isTentative object_def
    isTentativeDef _ = False
    shouldOverride def o | isDeclaration o = True
                         | not (isTentativeDef def) = True
                         | isTentativeDef o = True
                         | otherwise = False
    
-- * scope manipulation
--
--  * file scope: outside of parameter lists and blocks (outermost)
--
--  * function prototype scope
--
--  * function scope: labels are visible within the entire function, and declared implicitely
--
--  * block scope
updDefTable :: (MonadTrav m) => (DefTable -> DefTable) -> m ()
updDefTable f = withDefTable (\st -> ((),f st))

enterPrototypeScope :: (MonadTrav m) => m ()
enterPrototypeScope = updDefTable (ST.enterBlockScope)

leavePrototypeScope :: (MonadTrav m) => m ()
leavePrototypeScope = updDefTable (ST.leaveBlockScope)

enterFunctionScope :: (MonadTrav m) => m ()
enterFunctionScope = updDefTable (ST.enterFunctionScope)

leaveFunctionScope :: (MonadTrav m) => m ()
leaveFunctionScope = updDefTable (ST.leaveFunctionScope)

enterBlockScope :: (MonadTrav m) => m ()
enterBlockScope = updDefTable (ST.enterBlockScope)

leaveBlockScope :: (MonadTrav m) => m ()
leaveBlockScope = updDefTable (ST.leaveBlockScope)

-- * Lookup

-- | lookup a type definition
-- the 'wrong kind of object' is an internal error here,
-- because the parser should distinguish typedefs and other
-- objects
lookupTypeDef :: (MonadTrav m) => Ident -> m Type
lookupTypeDef ident =
    getDefTable >>= \symt ->
    case lookupIdent ident symt of
        Nothing                                 -> astError (nodeInfo ident) "unbound typedef"
        Just (TypeDef (TypeDef' _ident ty _ _)) -> return ty
        Just d                                  -> astError (nodeInfo ident) (wrongKindErrMsg d)
    where
    wrongKindErrMsg d = "wrong kind of object: excepcted typedef but found: "++(objKindDescr d)

-- | lookup an arbitrary variable declaration (delegates to symboltable)
lookupVarDecl :: (MonadTrav m) => Ident -> m (Maybe IdentDecl)
lookupVarDecl ident = liftM (lookupIdent ident) getDefTable

-- | lookup an object declaration or definition
lookupObject :: (MonadTrav m) => Ident -> m (Maybe (Either VarDecl ObjDef))
lookupObject ident = do
    old_decl <- lookupVarDecl ident
    mapMaybeM old_decl $ \obj ->
        let noFunDecl = not . isFunctionType . declType in
        case obj of
           Declaration (Decl vardecl _) | noFunDecl vardecl -> return (Left vardecl)
           ObjectDef objdef                                 -> return (Right objdef)
           bad_obj -> astError (nodeInfo ident) (mismatchErr "lookupObject" "an object" bad_obj) 

lookupFun :: (MonadTrav m) => Ident -> m (Maybe (Either VarDecl FunDef))
lookupFun ident = do
    old_decl <- lookupVarDecl ident
    mapMaybeM old_decl $ \obj ->
        let isFunDecl = isFunctionType . declType in
        case obj of
           Declaration (Decl vardecl _) | isFunDecl vardecl -> return (Left vardecl)
           FunctionDef fundef                               -> return (Right fundef)
           bad_obj -> astError (nodeInfo ident) (mismatchErr "lookupFun" "a function" bad_obj)

mismatchErr :: String -> String -> IdentDecl -> String
mismatchErr ctx expect found = ctx ++ ": Expected " ++ expect ++ ", but found: " ++ objKindDescr found
            
-- * inserting declarations

-- | create a reference to a struct\/union\/enum
--
-- /TODO/ This currently depends on the fact the structs are tagged with unique names
-- we rather would want to use the name generation of TravMonad, but this requires
-- some restructuring of the analysis code
createSUERef :: (MonadTrav m) => NodeInfo -> Maybe Ident -> m SUERef
createSUERef _node_info (Just ident) = return$ NamedType ident
createSUERef node_info Nothing | (Just name) <- nameOfNode node_info = return $ AnonymousType name
                               | otherwise = astError node_info "struct/union/enum definition without unique name"

-- * error handling facilities

handleTravError :: (MonadTrav m) => m a -> m (Maybe a)
handleTravError a = liftM Just a `catchTravError` (\e -> recordError e >> return Nothing)

-- | check wheter non-recoverable errors occured
hadHardErrors :: [CError] -> Bool
hadHardErrors = (not . null . filter isHardError)

-- | raise an error caused by a malformed AST
astError :: (MonadTrav m) => NodeInfo -> String -> m a
astError node msg = throwTravError $ invalidAST node msg

-- | raise an error based on an Either argument
throwOnLeft :: (MonadTrav m, Error e) => Either e a -> m a
throwOnLeft (Left err) = throwTravError err
throwOnLeft (Right v)  = return v

warn :: (Error e, MonadTrav m) => e -> m ()
warn err = recordError (changeErrorLevel err LevelWarn)

-- * The Trav datatype

-- | simple MTL-based traversal monad, providing user state and callbacks
newtype Trav s a = Trav { unTrav :: (StateT (TravState s) (Either CError)) a }

runTrav :: forall s a. s -> Trav s a -> Either [CError] (a, TravState s)
runTrav state traversal = 
    case runStateT (unTrav action) (initTravState state) of
        Left trav_err                                 -> Left [trav_err]
        Right (v, ts) | hadHardErrors (travErrors ts) -> Left (travErrors ts)
                      | otherwise                     -> Right (v,ts)
    where
    action = do withDefTable $ defineScopedIdent (identOfDecl va_list) va_list
                traversal
    va_list = TypeDef (TypeDef' (internalIdent "__builtin_va_list") 
                                (DirectType (TyBuiltin TyVaList) noTypeQuals [])
                                []
                                (internalNode))

runTrav_ :: Trav () a -> Either [CError] (a,[CError])
runTrav_ t = fmap fst . runTrav () $
    do r <- t
       es <- getErrors
       return (r,es)

withExtDeclHandler :: Trav s a -> (DeclEvent -> Trav s ()) -> Trav s a
withExtDeclHandler action handler =
    do modify $ \st -> st { doHandleExtDecl = handler }
       action

instance Monad (Trav s) where
    return     = Trav . return
    (>>=) a fb = Trav $ unTrav a >>= (unTrav . fb)
instance MonadState (TravState s) (Trav s) where
    get = Trav get
    put = Trav . put
instance MtlError.Error CError where
    strMsg _msg = internalErr "CError: strMsg"
instance MtlError.MonadError CError (Trav s) where
    throwError           = Trav . MtlError.throwError
    catchError a handler = Trav $ (unTrav a) `MtlError.catchError` (unTrav . handler)

instance MonadTrav (Trav s) where
    -- error handling facilities
    throwTravError = MtlError.throwError . toError
    catchTravError a handler =  a `MtlError.catchError` handler
    recordError e = modify $ \st -> st { rerrors = (rerrors st) `snoc` toError e } 
    getErrors = gets (RList.reverse . rerrors)
    -- symbol table handling
    getDefTable = gets symbolTable
    withDefTable f = do
        ts <- get
        let (r,symt') = f (symbolTable ts)
        put $ ts { symbolTable = symt' }
        return r
    -- unique name generation
    genName = generateName
    -- handling declarations and definitions
    handleDecl d = ($ d) =<< gets doHandleExtDecl

data TravState s = 
    TravState {
        symbolTable :: DefTable, 
        rerrors :: RList CError, 
        nameGenerator :: [Name],
        doHandleExtDecl :: (DeclEvent -> Trav s ()),
        userState :: s 
      }
travErrors :: TravState s -> [CError]
travErrors = RList.reverse . rerrors
initTravState :: s -> TravState s
initTravState userst = 
    TravState { 
        symbolTable = emptyDefTable, 
        rerrors = RList.empty, 
        nameGenerator = namesStartingFrom 0,
        doHandleExtDecl = const (return ()),
        userState = userst 
      }
initTravState_ :: TravState ()
initTravState_ = initTravState ()

-- * Trav specific operations
modifyUserState :: (s -> s) -> Trav s ()
modifyUserState f = modify $ \ts -> ts { userState = f (userState ts) }

generateName :: Trav s Name
generateName = 
    get >>= \ts -> 
    do let (new_name : gen') = nameGenerator ts 
       put $ ts { nameGenerator = gen'}
       return new_name

-- * helpers
mapMaybeM :: (Monad m) => (Maybe a) -> (a -> m b) -> m (Maybe b)
mapMaybeM m f = maybe (return Nothing) (liftM Just . f) m

maybeM :: (Monad m) => (Maybe a) -> (a -> m ()) -> m ()
maybeM m f = maybe (return ()) f m

mapSndM :: (Monad m) => (b -> m c) -> (a,b) -> m (a,c)
mapSndM f (a,b) = liftM ((,) a) (f b)

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

getNodeName :: NodeInfo -> Name
getNodeName (NodeInfo _ name) = name
getNodeName (OnlyPos _)       = error "getNodeName: unnamed"
