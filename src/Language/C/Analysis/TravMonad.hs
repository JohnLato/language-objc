{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts,
             PatternSignatures, RankNTypes, ScopedTypeVariables #-} 
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis.TravMonad
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  non-portable (mtl)
--
-- Monad for Traversals of the C AST.
-- mtl based prototype implementation.
-----------------------------------------------------------------------------
module Language.C.Analysis.TravMonad (
    Trav, runTrav, runTrav_,
    TravState, initTravState,
    -- * lifting symbol table functions
    updSymbolTable,withSymbolTable,
    symbolTable,
    -- * symbol table scope modification
    getDeclContext,
    enterPrototypeScope,leavePrototypeScope,
    enterFunctionScope,leaveFunctionScope,
    enterBlockScope,leaveBlockScope,
    -- * user state operations
    modifyUserState,userState,
    -- * unique name generation
    genName,
    -- * error handling
    errors,addError,hasHardErrors,
    throwTravError,throwOnLeft,catchTravError,mapRecoverM,mapRecoverM_,
    astError,mkAstError,
)
where
import Language.C.Common
import Language.C.Parser.AST
import Language.C.Analysis.SymbolTable hiding (enterBlockScope,leaveBlockScope,enterFunctionScope,leaveFunctionScope)
import qualified Language.C.Analysis.SymbolTable as ST

import Data.Maybe
import Control.Monad.Error
import Control.Monad.State

{- as iavor suggested, we could add an interface to make it possible to switch to other monads
-}

newtype Trav s a = Trav { unTrav :: StateT (TravState s) (Either CError) a }
runTrav :: forall s a. s -> Trav s a -> Either [CError] (a, TravState s)
runTrav s t = case runStateT (unTrav t) (initTravState s) of
                Left travErr -> Left [travErr]
                Right (v, ts) | hasHardErrors ts -> Left (errors ts)
                              | otherwise    -> Right (v,ts)
runTrav_ :: Trav () a -> Either [CError] (a, TravState ())
runTrav_ t = runTrav () t

instance Error CError where
   strMsg msg = mkError LevelError nopos (lines msg)

instance Monad (Trav s) where
    return = Trav . return
    (>>=) a fb = Trav $ unTrav a >>= (unTrav . fb)
instance MonadState (TravState s) (Trav s) where
    get = Trav get
    put = Trav . put
state :: (TravState s -> (a, TravState s)) -> Trav s a
state f = Trav . StateT $ \s -> return (f s)

instance MonadError CError (Trav s) where
    throwError = Trav . throwError
    catchError a handler = Trav $ (unTrav a) `catchError` (unTrav . handler)


data TravState s = TravState { 
                        symbolTable :: SymbolTable, 
                        rerrors :: [CError], 
                        nameGenerator :: [Name],
                        userState :: s 
                    }
initTravState :: s -> TravState s
initTravState userst = TravState { 
                           symbolTable = emptySymbolTable, 
                           rerrors = [], 
                           nameGenerator = namesStartingFrom 0,
                           userState = userst 
                       }

-- Lifting symbol table operations
updSymbolTable :: (SymbolTable -> SymbolTable) -> TravState s -> TravState s
updSymbolTable f ts@(TravState { symbolTable = dt }) = ts { symbolTable = f dt }
withSymbolTable :: (SymbolTable -> (SymbolTable, a)) ->  TravState s -> (a, TravState s)
withSymbolTable f ts@(TravState { symbolTable = dt }) = let (dt',r) = f dt in (r, ts{ symbolTable = dt' })

-- * lifting user state operations
modifyUserState :: (s -> s) -> Trav s ()
modifyUserState f = modify $ \ts -> ts { userState = f (userState ts) }

-- * Name generation
genName :: Trav s Name
genName = state $ \ts -> 
    let (newName : gen') = nameGenerator ts 
    in  (newName, ts { nameGenerator = gen'} )

-- * Error handling

-- | return the list of errors and warnings
errors :: TravState s -> [CError]
errors = reverse . rerrors
-- | add an error or warning
addError :: CError -> TravState s -> TravState s
addError e st = st { rerrors = e : (rerrors st) }
-- | check if there where any hard errors
hasHardErrors :: TravState s -> Bool
hasHardErrors ts = (not . null . filter isWarning) (rerrors ts)

-- | raise an error encountered at the given position
throwTravError :: Position -> [String] -> Trav s a
throwTravError pos msgs = throwError (mkError LevelError pos msgs)

-- | raise an error based on an Either argument
throwOnLeft :: Either CError a -> Trav s a
throwOnLeft (Left err) = throwError err
throwOnLeft (Right v) = return v

-- | raise an error caused by a malformed AST
astError :: NodeInfo -> String -> Trav s a
astError node msg = throwError $ mkAstError node msg

mkAstError :: NodeInfo -> String -> CError
mkAstError node msg = mkError LevelError (nodePos node) (lines msg)

-- | catch an error and add it to the list of errors
catchTravError :: Trav s a -> Trav s (Maybe a)
catchTravError a = liftM Just a `catchError` (\e -> modify (addError e) >> return Nothing)

-- | like mapM, but recover from errors
mapRecoverM :: (a -> Trav s b) -> [a] -> Trav s [b]
mapRecoverM f = liftM catMaybes . mapM (catchTravError . f)
-- | like mapM_, but recover from errors
mapRecoverM_ :: (a -> Trav s ()) -> [a] -> Trav s ()
mapRecoverM_ f = mapM_ (catchTravError . f)

-- * scope manipulation
--  * file scope: outside of parameter lists and blocks (outermost)
--  * function prototype scope
--  * function scope: labels are visible within the entire function, and declared implicitely
--  * block scope

getDeclContext :: Trav s DeclContext
getDeclContext = gets (ST.declContext . symbolTable)

enterPrototypeScope :: Trav s ()
enterPrototypeScope = modify $ updSymbolTable (ST.enterBlockScope ProtoCtx)

leavePrototypeScope :: Trav s ()
leavePrototypeScope = modify $ updSymbolTable (ST.leaveBlockScope)

enterFunctionScope :: Trav s ()
enterFunctionScope = modify $ updSymbolTable (ST.enterFunctionScope)

leaveFunctionScope :: Trav s ()
leaveFunctionScope = modify $ updSymbolTable (ST.leaveFunctionScope)

enterBlockScope :: Trav s ()
enterBlockScope = modify $ updSymbolTable (ST.enterBlockScope BlockCtx)

leaveBlockScope :: Trav s ()
leaveBlockScope = modify $ updSymbolTable (ST.leaveBlockScope)


-- | enter an object definition into the object name space
--
-- -- if a definition of the same name was already present, it is returned 
-- defObj         :: Ident -> CObj ->Trav s (DeclarationStatus CObj)
-- defObj ident obj  = state $ withSymbolTable (declareObject ident obj)
-- 
-- -- | lookup an identifier in the current local and in the global scope
-- findObj :: Ident -> Trav s (Maybe CObj)
-- findObj ident = gets (lookupObj ident . symbolTable)
-- 
-- defTag         :: SueRef -> CTag ->Trav s (Maybe CTag)
-- defTag ident obj  = state $ withSymbolTable (declareTag ident obj)
-- 
-- -- | lookup an identifier in the current local and in the global scope
-- findTag :: Ident -> Trav s (Maybe CTag)
-- findTag ident = gets (lookupTag ident . symbolTable)

