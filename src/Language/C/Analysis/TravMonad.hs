{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts,FlexibleInstances,
             PatternSignatures, PatternGuards, RankNTypes, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis.TravMonad
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  alpha
-- Portability :  ghc
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
    -- * Handling declarations
    handleTagDecl, handleTagDef, handleEnumeratorDef, handleTypeDef,
    handleObjectDef,handleFunDef,handleVarDecl,handleParamDecl,
    handleAsmBlock,
    -- * Symbol table scope modification
    enterPrototypeScope,leavePrototypeScope,
    enterFunctionScope,leaveFunctionScope,
    enterBlockScope,leaveBlockScope,
    -- * Symbol table lookup (delegate)
    lookupTypeDef, lookupObject,
    -- * Symbol table modification
    createSUERef,
    -- * Additional error handling facilities
    hadHardErrors,handleTravError,throwOnLeft,
    astError, warn,
    -- * Trav - default MonadTrav implementation
    Trav,
    runTrav,runTrav_,
    TravState,initTravState,withExtDeclHandler,modifyUserState,userState,
    TravOptions(..),modifyOptions,
    travErrors,
    -- * Language options
    CLanguage(..),
    -- * Helpers
    mapMaybeM,maybeM,mapSndM,concatMapM,
)
where
import Language.C.Data
import Language.C.Data.RList as RList
import Language.C.Syntax

import Language.C.Analysis.Builtins
import Language.C.Analysis.SemError
import Language.C.Analysis.SemRep
import Language.C.Analysis.DefTable hiding (enterBlockScope,leaveBlockScope,
                                            enterFunctionScope,leaveFunctionScope)
import qualified Language.C.Analysis.DefTable as ST

import Data.Maybe
import Control.Monad(liftM)

-- | Traversal monad
class (Monad m) => MonadTrav m where
    -- error handling facilities

    -- | throw an 'Error'
    throwTravError :: Error e => e -> m a
    -- | catch an 'Error' (we could implement dynamically-typed catch here)
    catchTravError :: m a -> (CError -> m a) -> m a
    -- | remember that an 'Error' occured (without throwing it)
    recordError    :: Error e => e -> m ()
    -- | return the list of recorded errors
    getErrors      :: m [CError]

    -- symbol table handling

    -- | return the definition table
    getDefTable :: m DefTable
    -- | perform an action modifying the definition table
    withDefTable :: (DefTable -> (a, DefTable)) -> m a

    -- | unique name generation
    genName :: m Name

    -- | handling declarations and definitions
    handleDecl :: DeclEvent -> m ()

-- * handling declarations

-- check wheter a redefinition is ok
checkRedef :: (MonadTrav m, CNode t, CNode t1) => String -> t -> (DeclarationStatus t1) -> m ()
checkRedef subject new_decl redecl_status =
    case redecl_status of
        NewDecl -> return ()
        Redeclared old_def   -> throwTravError $
            redefinition LevelError subject DuplicateDef (nodeInfo new_decl) (nodeInfo old_def)
        KindMismatch old_def -> throwTravError $
            redefinition LevelError subject DiffKindRedecl (nodeInfo new_decl) (nodeInfo old_def)
        Shadowed _old_def     ->  return ()
            -- warn $
            -- redefinition LevelWarn subject ShadowedDef (nodeInfo new_decl) (nodeInfo old_def)
        KeepDef _old_def      -> return ()

-- | forward declaration of a tag. Only neccessary for name analysis, but otherwise no semantic
-- consequences.
handleTagDecl :: (MonadTrav m) => TagFwdDecl -> m ()
handleTagDecl decl = do
    redecl <- withDefTable $ declareTag (sueRef decl) decl
    checkRedef (show $ sueRef decl) decl redecl

-- | define the given composite type or enumeration
-- If there is a declaration visible, overwrite it with the definition.
-- Otherwise, enter a new definition in the current namespace.
-- If there is already a definition present, yield an error (redeclaration).
handleTagDef :: (MonadTrav m) => TagDef -> m ()
handleTagDef def = do
    redecl <- withDefTable $ defineTag (sueRef def) def
    checkRedef (show $ sueRef def) def redecl
    handleDecl (TagEvent def)

handleEnumeratorDef :: (MonadTrav m) => Enumerator ->  m ()
handleEnumeratorDef enumerator = do
    let ident = declIdent enumerator
    redecl <- withDefTable $ defineScopedIdent ident (EnumeratorDef enumerator)
    checkRedef (show ident) ident redecl
    return ()

handleTypeDef :: (MonadTrav m) => TypeDef -> m ()
handleTypeDef typeDef@(TypeDef ident _ _ _) = do
    redecl <- withDefTable $ defineTypeDef ident typeDef
    checkRedef (show ident) typeDef redecl
    handleDecl (TypeDefEvent typeDef)
    return ()

handleAsmBlock :: (MonadTrav m) => AsmBlock -> m ()
handleAsmBlock asm = handleDecl (AsmEvent asm)

redefErr :: (MonadTrav m, CNode old, CNode new) => Ident -> ErrorLevel -> new -> old -> RedefKind -> m ()
redefErr name lvl new old kind =
  throwTravError $ redefinition lvl (show name) kind (nodeInfo new) (nodeInfo old)

-- TODO: unused
checkIdentTyRedef :: (MonadTrav m) => IdentEntry -> (DeclarationStatus IdentEntry) -> m ()
checkIdentTyRedef (Right decl) status = checkVarRedef decl status
checkIdentTyRedef (Left tydef) (KindMismatch old_def) =
  redefErr (identOfTypeDef tydef) LevelError tydef old_def DiffKindRedecl
checkIdentTyRedef (Left tydef) (Redeclared old_def) =
  redefErr (identOfTypeDef tydef) LevelError tydef old_def DuplicateDef
checkIdentTyRedef (Left _tydef) _ = return ()

-- Check whether it is ok to declare a variable already in scope
checkVarRedef :: (MonadTrav m) => IdentDecl -> (DeclarationStatus IdentEntry) -> m ()
checkVarRedef def redecl =
    case redecl of
        -- always an error
        KindMismatch old_def -> redefVarErr old_def DiffKindRedecl
        -- Declaration referencing definition:
        --   * new entry has to be a declaration
        --   * old entry and new entry have to have linkage and agree on linkage
        --   * types have to match
        KeepDef (Right old_def) | not (agreeOnLinkage def old_def) -> linkageErr def old_def
                                | otherwise -> throwOnLeft $ checkCompatibleTypes new_ty (declType old_def)
        -- redefinition:
        --   * old entry has to be a declaration or tentative definition
        --   * old entry and new entry have to have linkage and agree on linkage
        --   * types have to match
        Redeclared (Right old_def) | not (agreeOnLinkage def old_def) -> linkageErr def old_def
                                   | not(canBeOverwritten old_def) -> redefVarErr old_def DuplicateDef
                                   | otherwise -> throwOnLeft $ checkCompatibleTypes new_ty (declType old_def)
        -- NewDecl/Shadowed is ok
        _ -> return ()
    where
    redefVarErr old_def kind = redefErr (declIdent def) LevelError def old_def kind
    linkageErr def old_def =
        case (declLinkage def, declLinkage old_def) of
            (NoLinkage, _) -> redefErr (declIdent def) LevelError  def old_def NoLinkageOld
            otherwise      -> redefErr (declIdent def) LevelError  def old_def DisagreeLinkage

    new_ty = declType def
    canBeOverwritten (Declaration _) = True
    canBeOverwritten (ObjectDef od)  = isTentative od
    canBeOverwritten _               = False
    agreeOnLinkage def old_def
        | declStorage old_def == FunLinkage InternalLinkage = True
        | not (hasLinkage $ declStorage def) || not (hasLinkage $ declStorage old_def) = False
        | (declLinkage def) /= (declLinkage old_def) = False
        | otherwise = True

-- | handle variable declarations (external object declarations and function prototypes)
-- variable declarations are either function prototypes, or external declarations, and not very
-- interesting on their own. we only put them in the symbol table and call the handle.
-- declarations never override definitions
handleVarDecl :: (MonadTrav m) => Bool -> Decl -> m ()
handleVarDecl is_local decl = do
    def <- enterDecl decl (const False)
    handleDecl ((if is_local then LocalEvent else DeclEvent) def)

-- | handle parameter declaration. The interesting part is that parameters can be abstract
-- (if they are part of a type). If they have a name, we enter the name (usually in function prototype or function scope),
-- checking if there are duplicate definitions.
-- FIXME: I think it would be more transparent to handle parameter declarations in a special way
handleParamDecl :: (MonadTrav m) => ParamDecl -> m ()
handleParamDecl pd@(AbstractParamDecl _ _) = handleDecl (ParamEvent pd)
handleParamDecl pd@(ParamDecl vardecl node) = do
    let def = ObjectDef (ObjDef vardecl Nothing node)
    redecl <- withDefTable $ defineScopedIdent (declIdent def) def
    checkVarRedef def redecl
    handleDecl (ParamEvent pd)

-- shared impl
enterDecl :: (MonadTrav m) => Decl -> (IdentDecl -> Bool) -> m IdentDecl
enterDecl decl cond = do
    let def = Declaration decl
    redecl <- withDefTable $
              defineScopedIdentWhen cond (declIdent def) def
    checkVarRedef def redecl
    return def

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
handleObjectDef :: (MonadTrav m) => Bool -> Ident -> ObjDef -> m ()
handleObjectDef local ident obj_def = do
    let def = ObjectDef obj_def
    redecl <- withDefTable $
              defineScopedIdentWhen (\old -> shouldOverride def old) ident def
    checkVarRedef def redecl
    handleDecl ((if local then LocalEvent else DeclEvent) def)
    where
    isTentativeDef (ObjectDef object_def) = isTentative object_def
    isTentativeDef _ = False
    shouldOverride def old | isDeclaration old = True
                           | not (isTentativeDef def) = True
                           | isTentativeDef old = True
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
-- because the parser should distinguish typeDefs and other
-- objects
lookupTypeDef :: (MonadTrav m) => Ident -> m Type
lookupTypeDef ident =
    getDefTable >>= \symt ->
    case lookupIdent ident symt of
        Nothing                             ->
          astError (nodeInfo ident) $ "unbound typeDef: " ++ identToString ident
        Just (Left (TypeDef _ident ty _ _)) -> return ty
        Just (Right d)                      -> astError (nodeInfo ident) (wrongKindErrMsg d)
    where
    wrongKindErrMsg d = "wrong kind of object: excepcted typeDef but found: "++(objKindDescr d)


-- | lookup an object, function or enumerator
lookupObject :: (MonadTrav m) => Ident -> m (Maybe IdentDecl)
lookupObject ident = do
    old_decl <- liftM (lookupIdent ident) getDefTable
    mapMaybeM old_decl $ \obj ->
        case obj of
        Right objdef -> return objdef
        Left _tydef  -> astError (nodeInfo ident) (mismatchErr "lookupObject" "an object" "a typeDef")


mismatchErr :: String -> String -> String -> String
mismatchErr ctx expect found = ctx ++ ": Expected " ++ expect ++ ", but found: " ++ found

-- * inserting declarations

-- | create a reference to a struct\/union\/enum
--
-- This currently depends on the fact the structs are tagged with unique names.
-- We could use the name generation of TravMonad as well, which might be the better
-- choice when dealing with autogenerated code.
createSUERef :: (MonadTrav m) => NodeInfo -> Maybe Ident -> m SUERef
createSUERef _node_info (Just ident) = return$ NamedRef ident
createSUERef node_info Nothing | (Just name) <- nameOfNode node_info = return $ AnonymousRef name
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

-- | simple traversal monad, providing user state and callbacks
newtype Trav s a = Trav { unTrav :: TravState s -> Either CError (a, TravState s) }
modify :: (TravState s -> TravState s) -> Trav s ()
modify f = Trav (\s -> Right ((),f s))
gets :: (TravState s -> a) -> Trav s a
gets f   = Trav (\s -> Right (f s, s))
get ::  Trav s (TravState s)
get      = Trav (\s -> Right (s,s))
put :: TravState s -> Trav s ()
put s    = Trav (\_ -> Right ((),s))


runTrav :: forall s a. s -> Trav s a -> Either [CError] (a, TravState s)
runTrav state traversal =
    case unTrav action (initTravState state) of
        Left trav_err                                 -> Left [trav_err]
        Right (v, ts) | hadHardErrors (travErrors ts) -> Left (travErrors ts)
                      | otherwise                     -> Right (v,ts)
    where
    action = do withDefTable (const ((), builtins))
                traversal

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
    return x  = Trav (\s -> Right (x,s))
    m >>= k   = Trav (\s -> case unTrav m s of
                              Right (x,s1) -> unTrav (k x) s1
                              Left e       -> Left e)

instance MonadTrav (Trav s) where
    -- error handling facilities
    throwTravError e = Trav (\_ -> Left (toError e))
    catchTravError a handler = Trav (\s -> case unTrav a s of
                                             Left e  -> unTrav (handler e) s
                                             Right r -> Right r)
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

-- | The variety of the C language to accept. Note: this is not yet enforced.
data CLanguage = C89 | C99 | GNU89 | GNU99

data TravOptions =
    TravOptions {
        language :: CLanguage
    }

data TravState s =
    TravState {
        symbolTable :: DefTable,
        rerrors :: RList CError,
        nameGenerator :: [Name],
        doHandleExtDecl :: (DeclEvent -> Trav s ()),
        userState :: s,
        options :: TravOptions
      }

travErrors :: TravState s -> [CError]
travErrors = RList.reverse . rerrors

initTravState :: s -> TravState s
initTravState userst =
    TravState {
        symbolTable = emptyDefTable,
        rerrors = RList.empty,
        nameGenerator = newNameSupply,
        doHandleExtDecl = const (return ()),
        userState = userst,
        options = TravOptions { language = C99 }
      }

-- * Trav specific operations
modifyUserState :: (s -> s) -> Trav s ()
modifyUserState f = modify $ \ts -> ts { userState = f (userState ts) }

modifyOptions :: (TravOptions -> TravOptions) -> Trav s ()
modifyOptions f = modify $ \ts -> ts { options = f (options ts) }

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
