{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving,
      DeriveFunctor #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.ObjC.Syntax.ParserMonad
-- Copyright   :  (c) [1999..2004] Manuel M T Chakravarty
--                (c) 2005-2007 Duncan Coutts
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
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
  P,
  IType (..),
  TMap,
  execParser,
  failP,
  getNewName,           -- :: P Name
  addTypedef,           -- :: Ident -> P ()
  shadowSymbol,         -- :: Ident -> P ()
  isTypeIdent,          -- :: Ident -> P Bool
  addClass,             -- :: Ident -> P ()
  isClass,              -- :: Ident -> P Bool
  isSpecial,            -- :: Ident -> P IType
  enterScope,           -- :: P ()
  leaveScope,           -- :: P ()
  setPos,               -- :: Position -> P ()
  getPos,               -- :: P Position
  getInput,             -- :: P String
  setInput,             -- :: String -> P ()
  getLastToken,         -- :: P CToken
  getSavedToken,        -- :: P CToken
  setLastToken,         -- :: CToken -> P ()
  handleEofToken,       -- :: P ()
  getCurrentPosition,   -- :: P Position
  ParseError(..),
  ) where
import Language.ObjC.Data.Error (internalErr, showErrorInfo,ErrorInfo(..),ErrorLevel(..))
import Language.ObjC.Data.Position  (Position(..))
import Language.ObjC.Data.InputStream
import Language.ObjC.Data.Name    (Name)
import Language.ObjC.Data.Ident    (Ident)
import Language.ObjC.Parser.Tokens (CToken(CTokEof))

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
        curInput   :: !InputStream,      -- the current input
        prevToken  ::  CToken,          -- the previous token
        savedToken ::  CToken,          -- and the token before that
        namesupply :: ![Name],          -- the name unique supply
        tyidents   :: !(TMap), -- the set of typedef'ed identifiers
        scopes     :: [TMap]   -- the tyident sets for outer scopes
     }

newtype P a = P { unP :: PState -> ParseResult a }
  deriving (Functor)

instance Applicative P where
  pure = returnP
  f <*> m = f >>= \f' -> m >>= \m' -> pure (f' m')

instance Monad P where
  return = returnP
  (>>=) = thenP
  fail m = getPos >>= \pos -> failP pos [m]


-- | execute the given parser on the supplied input stream.
--   returns 'ParseError' if the parser failed, and a pair of
--   result and remaining name supply otherwise
--
-- Synopsis: @execParser parser inputStream initialPos predefinedTypedefs uniqNameSupply@
execParser :: P a -> InputStream -> Position -> [Ident] -> [Name]
           -> Either ParseError (a,[Name])
execParser (P parser) input pos builtins names =
  case parser initialState of
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

{-# INLINE returnP #-}
returnP :: a -> P a
returnP a = P $ \s -> POk s a

{-# INLINE thenP #-}
thenP :: P a -> (a -> P b) -> P b
(P m) `thenP` k = P $ \s ->
        case m s of
                POk s' a        -> (unP (k a)) s'
                PFailed err pos -> PFailed err pos

failP :: Position -> [String] -> P a
failP pos msg = P $ \_ -> PFailed msg pos

getNewName :: P Name
getNewName = P $ \s@PState{namesupply=(n:ns)} -> n `seq` POk s{namesupply=ns} n

setPos :: Position -> P ()
setPos pos = P $ \s -> POk s{curPos=pos} ()

getPos :: P Position
getPos = P $ \s@PState{curPos=pos} -> POk s pos

addTypedef :: Ident -> P ()
addTypedef ident = (P $ \s@PState{tyidents=tyids} ->
                             POk s{tyidents = Map.insert ident TyDef tyids} ())

shadowSymbol :: Ident -> P ()
shadowSymbol ident = (P $ \s@PState{tyidents=tyids} ->
                             -- optimisation: mostly the ident will not be in
                             -- the tyident set so do a member lookup to avoid
                             --  churn induced by calling delete
                             POk s{tyidents = if ident `Map.member` tyids
                                                then ident `Map.delete` tyids
                                                else tyids } ())

isTypeIdent :: Ident -> P Bool
isTypeIdent ident = P $ \s@PState{tyidents=tyids} ->
                             POk s $! maybe False (== TyDef)
                             $ Map.lookup ident tyids

addClass :: Ident -> P ()
addClass ident = (P $ \s@PState{tyidents=tyids} ->
                             POk s{tyidents = Map.insert ident CName tyids} ())

isClass :: Ident -> P Bool
isClass ident = P $ \s@PState{tyidents=tyids} ->
                             POk s $! maybe False (== CName)
                             $ Map.lookup ident tyids

isSpecial :: Ident -> P (Maybe IType)
isSpecial ident = P $ \s@PState{tyidents=tyids} ->
                             POk s $! Map.lookup ident tyids

enterScope :: P ()
enterScope = P $ \s@PState{tyidents=tyids,scopes=ss} ->
                     POk s{scopes=tyids:ss} ()

leaveScope :: P ()
leaveScope = P $ \s@PState{scopes=ss} ->
                     case ss of
                       []          -> error "leaveScope: already in global scope"
                       (tyids:ss') -> POk s{tyidents=tyids, scopes=ss'} ()

getInput :: P InputStream
getInput = P $ \s@PState{curInput=i} -> POk s i

setInput :: InputStream -> P ()
setInput i = P $ \s -> POk s{curInput=i} ()

getLastToken :: P CToken
getLastToken = P $ \s@PState{prevToken=tok} -> POk s tok

getSavedToken :: P CToken
getSavedToken = P $ \s@PState{savedToken=tok} -> POk s tok

-- | @setLastToken modifyCache tok@
setLastToken :: CToken -> P ()
setLastToken CTokEof = P $ \s -> POk s{savedToken=(prevToken s)} ()
setLastToken tok      = P $ \s -> POk s{prevToken=tok,savedToken=(prevToken s)} ()

-- | handle an End-Of-File token (changes savedToken)
handleEofToken :: P ()
handleEofToken = P $ \s -> POk s{savedToken=(prevToken s)} ()

getCurrentPosition :: P Position
getCurrentPosition = P $ \s@PState{curPos=pos} -> POk s pos
