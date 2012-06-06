{-# LANGUAGE TupleSections
            ,BangPatterns
            ,NoMonomorphismRestriction
            ,GeneralizedNewtypeDeriving
            ,DeriveFunctor #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.ObjC.Syntax.ParserMonad
-- Copyright   :  (c) [1999..2004] Manuel M T Chakravarty
--                (c) 2005-2007 Duncan Coutts
--                (c) 2012 John W. Lato
-- License     :  BSD-style
-- Maintainer  :  jwlato@gmail.com
-- Portability :  portable
--
-- Monad for the C lexer and parser
--
--  This monad has to be usable with Alex and Happy. Some things in it are
--  dictated by that, eg having to be able to remember the last token.
--
--  The monad also provides a unique name supply (via the Name module)
--
--  For parsing C we have to maintain a set of identifiers that we know to be
--  typedef'ed type identifiers. We also must deal correctly with scope so we
--  keep a list of sets of identifiers so we can save the outer scope when we
--  enter an inner scope.
module Language.ObjC.Parser.ParserMonad (
  LP,
  P,
  IType (..),
  TMap,
  execParser,
  execLazyParser,
  failP,
  getNewName,           -- :: (PMonad p) => p Name
  addTypedef,           -- :: Ident -> P ()
  shadowSymbol,         -- :: Ident -> P ()
  isTypeIdent,          -- :: Ident -> P Bool
  addClass,             -- :: Ident -> P ()
  isClass,              -- :: Ident -> P Bool
  isSpecial,            -- :: Ident -> P IType
  enterScope,           -- :: (PMonad p) => p ()
  leaveScope,           -- :: (PMonad p) => p ()
  setPos,               -- :: Position -> P ()
  getPos,               -- :: (PMonad p) => p Position
  getInput,             -- :: (PMonad p) => p String
  setInput,             -- :: String -> P ()
  getLastToken,         -- :: (PMonad p) => p CToken
  getSavedToken,        -- :: (PMonad p) => p CToken
  setLastToken,         -- :: CToken -> P ()
  handleEofToken,       -- :: (PMonad p) => p ()
  getCurrentPosition,   -- :: (PMonad p) => p Position
  ParseError(..),
  parsedLazily,         -- :: s -> LP [s] s
  ) where

import Language.ObjC.Data.Error (internalErr, showErrorInfo,ErrorInfo(..),ErrorLevel(..))
import Language.ObjC.Data.Position  (Position(..))
import Language.ObjC.Data.InputStream
import Language.ObjC.Data.Name    (Name)
import Language.ObjC.Data.Ident    (Ident)
import Language.ObjC.Parser.Tokens (CToken(CTokEof))
import Language.ObjC.Syntax.AST    (CExtDecl)

import Data.Map  (Map)
import qualified Data.Map as Map

import Control.Applicative

newtype ParseError = ParseError ([String],Position)
instance Show ParseError where
    show (ParseError (msgs,pos)) = showErrorInfo "Syntax Error !" (ErrorInfo LevelError pos msgs)

-- | For typedef'd or classname identifiers, indicate which type they are
data IType = TyDef | CName deriving (Eq, Show, Ord, Enum)

type TMap = Map Ident IType

data ParseResult a
  = POk !PState a
  | PFailed [String] Position   -- The error message and position
 deriving (Functor)

data PState = PState {
  curPos     :: !Position,        -- position at current input location
  curInput   :: !InputStream,     -- the current input
  prevToken  ::  CToken,          -- the previous token
  savedToken ::  CToken,          -- and the token before that
  namesupply :: ![Name],          -- the name unique supply
  tyidents   :: !(TMap),          -- the set of typedef'ed identifiers
  scopes     :: [TMap]            -- the tyident sets for outer scopes
 }

-- | a minimal state-like representation, so we don't need to depend on mtl
class (Functor p, Monad p) => PMonad p where
  get :: p PState
  put :: PState -> p ()
  modify :: (PState -> PState) -> p ()
  modify f = get >>= put . f

failP :: Position -> [String] -> LP s a
failP pos m = LP $ \s pSt -> (PFailed m pos, s)

-- | Default parser type, so CExtDecls can be parsed lazily
type P a = LP [CExtDecl] a

-- | A Lazy Parser Monad.  Highly experimental
newtype LP s a = LP { unLP :: s -> PState -> (ParseResult a, s) }
  deriving (Functor)

instance Monad (LP s) where
  {-# INLINE return #-}
  return a = LP $ \s pSt -> (POk pSt a, s)
  {-# INLINE (>>=) #-}
  (LP m) >>= f = LP $ \s pSt ->
                   let (r1, s1) = m s2 pSt
                       (r2, s2) = case r1 of
                                    POk pSt' a -> unLP (f a) s pSt'
                                    PFailed err pos -> (PFailed err pos, s)
                   in (r2, s1)
  {-# INLINE fail #-}
  fail m = LP $ \s pSt -> (PFailed [m] (curPos pSt), s)

instance PMonad (LP s) where
  get      = LP $ \s pst -> (POk pst pst, s)
  put st   = LP $ \s _   -> (POk st (), s)
  modify f = LP $ \s pst -> (POk (f pst) (), s)

getL :: LP s s
getL = LP $ \s pst -> (POk pst s, s)

modifyL :: (s -> s) -> LP s ()
modifyL f = LP $ \s pst -> (POk pst (),f s)

putL :: s -> LP s ()
putL = modifyL . const

instance Applicative (LP s) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  f <*> m = f >>= \f' -> m >>= \m' -> pure (f' m')

-- | execute the given parser on the supplied input stream.
--   returns 'ParseError' if the parser failed, and a pair of
--   result and remaining name supply otherwise
-- 
--   Lazy parsing results are ignored.
--
-- Synopsis: @execParser parser inputStream initialPos predefinedTypedefs uniqNameSupply@
execParser :: LP [s] a -> InputStream -> Position -> [Ident] -> [Name]
           -> Either ParseError (a,[Name])
execParser (LP parser) input pos builtins names =
  case fst $ parser [] initialState of
    PFailed message errpos -> Left (ParseError (message,errpos))
    POk st result -> Right (result, namesupply st)
  where initialState = PState {
          curPos = pos,
          curInput = input,
          prevToken = internalErr "CLexer.execParser: Touched undefined token!",
          savedToken = internalErr "CLexer.execParser: Touched undefined token (safed token)!",
          namesupply = names,
          tyidents = Map.fromList $ map (,TyDef) builtins,
          scopes   = []
        }

-- | execute the given parser on the supplied input stream.
-- 
--   returns a lazy list of results, and either the parse result
--   or a ParseError if there was an error.
-- 
--   The list should be consumed as far as possible before checking the result is
--   evaluated for maximum laziness.
--
-- Synopsis: @execParser parser inputStream initialPos predefinedTypedefs uniqNameSupply@
execLazyParser
  :: LP [s] a
  -> InputStream
  -> Position
  -> [Ident]
  -> [Name]
  -> ([s], Either ParseError a)
execLazyParser (LP parser) input pos builtins names =
  let (res, lzparse) = parser [] initialState
      procRes        = case res of
                         PFailed message errpos -> Left (ParseError (message,errpos))
                         POk _ result -> Right result
  in  (lzparse, procRes)
  where initialState = PState {
          curPos = pos,
          curInput = input,
          prevToken = internalErr "CLexer.execParser: Touched undefined token!",
          savedToken = internalErr "CLexer.execParser: Touched undefined token (safed token)!",
          namesupply = names,
          tyidents = Map.fromList $ map (,TyDef) builtins,
          scopes   = []
        }

withState :: PMonad p => (PState -> (PState, a)) -> p a
withState f = get >>= \p -> case f p of
                  (pst', a) -> put pst' >> return a
{-# INLINE withState #-}

withState' :: PMonad p => (PState -> (PState, a)) -> p a
withState' f = get >>= \p -> case f p of
                  (pst', !a) -> put pst' >> return a
{-# INLINE withState' #-}

getNewName :: (PMonad p) => p Name
getNewName = withState' $ \s@PState{namesupply=(n:ns)} -> (s{namesupply=ns}, n)

setPos :: (PMonad p) => Position -> p ()
setPos pos = modify $ \s -> s{curPos=pos}

getPos :: (PMonad p) => p Position
getPos = (\st -> curPos st) <$> get

addTypedef :: (PMonad p) => Ident -> p ()
addTypedef ident = modify $ \s@PState{tyidents=tyids} ->
                             s{tyidents = Map.insert ident TyDef tyids}

shadowSymbol :: (PMonad p) => Ident -> p ()
shadowSymbol ident = modify $ \s@PState{tyidents=tyids} ->
                             -- optimisation: mostly the ident will not be in
                             -- the tyident set so do a member lookup to avoid
                             -- churn induced by calling delete
                             -- (JL: I dont follow this reasoning, if it's not present the map
                             --  shouldn't change, hence no churn...)
                             s{tyidents = if ident `Map.member` tyids
                                            then ident `Map.delete` tyids
                                            else tyids }

-- withState' :: PMonad p => (PState -> (PState, a)) -> p a
isTypeIdent :: (PMonad p) => Ident -> p Bool
isTypeIdent ident =  (\s -> maybe False (== TyDef)
                       . Map.lookup ident $ tyidents s)
                     <$> get

addClass :: (PMonad p) => Ident -> p ()
addClass ident = modify $ \s@PState{tyidents=tyids} ->
                             s{tyidents = Map.insert ident CName tyids}

isClass :: (PMonad p) => Ident -> p Bool
isClass ident = (\s -> maybe False (== CName)
                       . Map.lookup ident $ tyidents s)
                <$> get

isSpecial :: (PMonad p) => Ident -> p (Maybe IType)
isSpecial ident = (\s -> Map.lookup ident $ tyidents s) <$> get

enterScope :: (PMonad p) => p ()
enterScope = modify $ \s@PState{tyidents=tyids,scopes=ss} -> s{scopes=tyids:ss}

leaveScope :: (PMonad p) => p ()
leaveScope = modify $ \s@PState{scopes=ss} ->
                     case ss of
                       []          -> error "leaveScope: already in global scope"
                       (tyids:ss') -> s{tyidents=tyids, scopes=ss'}

getInput :: (PMonad p) => p InputStream
getInput = curInput <$> get

setInput :: (PMonad p) => InputStream -> p ()
setInput i = modify (\s -> s{curInput=i})

getLastToken :: (PMonad p) => p CToken
getLastToken = prevToken <$> get

getSavedToken :: (PMonad p) => p CToken
getSavedToken = savedToken <$> get

-- | @setLastToken modifyCache tok@
setLastToken :: (PMonad p) => CToken -> p ()
setLastToken CTokEof = modify $ \s -> s{savedToken=(prevToken s)}
setLastToken tok     = modify $ \s -> s{prevToken=tok,savedToken=(prevToken s)}

-- | handle an End-Of-File token (changes savedToken)
handleEofToken :: (PMonad p) => p ()
handleEofToken = modify $ \s -> s{savedToken=(prevToken s)}

getCurrentPosition :: (PMonad p) => p Position
getCurrentPosition = curPos <$> get

-- | Insert a parsed value into the lazy parsing stream
parsedLazily :: s -> LP [s] s
parsedLazily s = s <$ modifyL (s:)
