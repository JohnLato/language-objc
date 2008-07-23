{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Data.Error
-- Copyright   :  (c) 2008 Benedikt Huber, Manuel M. T. Chakravarty
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (ExistentialQuantification)
--
-- Base type for errors occuring in parsing, analysing and pretty-printing.
-- With ideas from Simon Marlow's 
-- "An extensible dynamically-typed hierarchy of execeptions [2006]"
--
-- Errors in the C library do provide a standard way of printing and source
-- code location (if available).
-----------------------------------------------------------------------------
module Language.C.Data.Error (
    ErrorLevel(..), isHardError,
    Error(..), errorPos, errorLevel, errorMsgs,
    CError(..), 
    ErrorInfo(..),showError,showErrorInfo,
    -- * default errors
    UnsupportedFeature, unsupported, unsupported_,
    UserError, userErr,
    -- * internal errors
    internalErr,
)
where
import Data.Typeable
import Data.Generics

import Language.C.Data.Position

-- | Error levels (priorities)
data ErrorLevel = LevelWarn
                | LevelError
                | LevelFatal
              deriving (Eq, Ord)
instance Show ErrorLevel where
    show LevelWarn  = "WARNING"
    show LevelError = "ERROR"
    show LevelFatal = "FATAL ERROR"
    
-- | return @True@ when the given error makes it impossible to continue
--   analysis or compilation.
isHardError :: (Error ex) => ex -> Bool
isHardError = ( > LevelWarn) . errorLevel

-- | information attached to every error in Language.C
data ErrorInfo = ErrorInfo ErrorLevel Position [String] deriving Typeable
-- to faciliate newtype deriving
instance Show ErrorInfo where show = showErrorInfo "error"
instance Error ErrorInfo where
    errorInfo = id
    changeErrorLevel (ErrorInfo _ pos msgs) lvl' = ErrorInfo lvl' pos msgs
-- | `superclass' of all errors
data CError 
    = forall err. (Error err) => CError err 
    deriving Typeable

-- | errors in Language.C are instance of the class 'Error'
class (Typeable e, Show e) => Error e where
    errorInfo        :: e -> ErrorInfo
    toError          :: e -> CError
    fromError     :: CError -> (Maybe e)
    changeErrorLevel :: e -> ErrorLevel -> e
    -- default implementation
    fromError (CError e) = cast e
    toError = CError
    changeErrorLevel e lvl = 
        if errorLevel e == lvl 
            then e 
            else error $ "changeErrorLevel: not possible for " ++ show e
            
instance Show CError where
    show (CError e) = show e
instance Error CError where
    errorInfo (CError err) = errorInfo err
    toError = id
    fromError = Just
    changeErrorLevel (CError e) = CError . changeErrorLevel e

errorPos   :: (Error e) => e -> Position
errorPos = ( \(ErrorInfo _ pos _) -> pos ) . errorInfo
errorLevel :: (Error e) => e -> ErrorLevel
errorLevel = ( \(ErrorInfo lvl _ _) -> lvl ) . errorInfo
errorMsgs   :: (Error e) => e -> [String]
errorMsgs = ( \(ErrorInfo _ _ msgs) -> msgs ) . errorInfo

-- | error raised if a operation requires an unsupported or not yet implemented
--   feature.
data UnsupportedFeature = UnsupportedFeature String Position deriving Typeable
instance Error UnsupportedFeature where
    errorInfo (UnsupportedFeature msg pos) = ErrorInfo LevelError pos (lines msg)
instance Show UnsupportedFeature where show = showError "Unsupported Feature"
unsupported :: String -> Position -> UnsupportedFeature
unsupported = UnsupportedFeature
unsupported_ :: String -> UnsupportedFeature
unsupported_ msg = UnsupportedFeature msg internalPos

-- | unspecified error raised by the user (in case the user does not want to define)
--   her own error types.
newtype UserError     = UserError ErrorInfo deriving Typeable
instance Error UserError where
    errorInfo (UserError info) = info
instance Show UserError where show = showError "User Error"
userErr :: String -> UserError
userErr msg = UserError (ErrorInfo LevelError internalPos (lines msg))
-- other errors to be defined elsewhere

showError :: (Error e) => String -> e -> String
showError short_msg = showErrorInfo short_msg . errorInfo
-- | converts an error into a string using a fixed format
--
-- * either the lines of the long error message or the short message has to be non-empty
--
-- * the format is
--
-- >    <fname>:<row>: (column <col>) [<err lvl>] 
-- >      >>> <line_1>
-- >      <line_2>
-- >        ...
-- >      <line_n>
showErrorInfo :: String -> ErrorInfo -> String
showErrorInfo short_msg (ErrorInfo level pos msgs) = 
    header ++ showMsgLines (if null short_msg then msgs else short_msg:msgs)
    where
    header = (posFile pos) ++ ":" ++ show (posRow pos) ++ ": " ++
             "(column " ++ show (posColumn pos) ++ ") " ++
             "[" ++ show level ++ "]"
    showMsgLines []     = internalErr "No short message or error message provided."
    showMsgLines (x:xs) = indent ++ ">>> " ++ x ++ "\n" ++ unlines (map (indent++) xs)


-- internal errors
internalErrPrefix :: String
internalErrPrefix = unlines [ "Language.C : Internal Error" ,
                              "This is propably a bug, and should be reported at "++     
                              "http://www.sivity.net/projects/language.c/newticket"]

-- | raise a fatal internal error; message may have multiple lines
internalErr     :: String -> a
internalErr msg  = error (internalErrPrefix ++ "\n"
                       ++ indentLines msg
                       ++ "\n")
indent :: String
indent = "  "
indentLines :: String -> String
indentLines = unlines . map (indent++) . lines
