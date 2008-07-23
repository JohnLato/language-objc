{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis.SemError
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  alpha
-- Portability :  unspecified
--
-- Errors in the semantic analysis [stub]
-----------------------------------------------------------------------------
module Language.C.Analysis.SemError (
InvalidASTError(..), invalidAST,
BadSpecifier(..), badSpecifierError,
TypeMismatch(..), typeMismatch,
RedefError(..), RedefInfo(..), RedefKind(..), redefinition,
)
where
import Data.Typeable
-- this means we cannot use SemError in SemRep, but use rich types here
import Language.C.Analysis.SemRep 
import Language.C.Data.Error
import Language.C.Data.Node

-- here are the errors available
newtype InvalidASTError = InvalidAST ErrorInfo deriving (Typeable,Error)
newtype BadSpecifier = BadSpecifier ErrorInfo deriving (Typeable,Error)
data RedefError = RedefError ErrorLevel RedefInfo deriving Typeable
data RedefInfo = RedefInfo String RedefKind NodeInfo NodeInfo
data RedefKind = DuplicateDef | DiffKindRedecl | ShadowedDef
data TypeMismatch = TypeMismatch String (NodeInfo,Type) (NodeInfo,Type) deriving Typeable

-- Invalid AST
-- ~~~~~~~~~~~

instance Show InvalidASTError  where show = showError "AST invariant violated"

invalidAST :: NodeInfo -> String -> InvalidASTError
invalidAST node_info msg = InvalidAST (mkErrInfo LevelError msg node_info)

-- Bad specifier (e.g. static for a parameter, or extern when there is an initializer)
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

instance Show BadSpecifier     where show = showError "Bad specifier"

badSpecifierError :: NodeInfo -> String -> BadSpecifier
badSpecifierError node_info msg = BadSpecifier (mkErrInfo LevelError msg node_info)

-- Type mismatch
-- ~~~~~~~~~~~~~
typeMismatch :: String -> (NodeInfo, Type) -> (NodeInfo,Type) -> TypeMismatch
typeMismatch = TypeMismatch

instance Show TypeMismatch where
    show tm = showError "Type mismatch" (typeMismatchInfo tm)
instance Error TypeMismatch where
    errorInfo = typeMismatchInfo
typeMismatchInfo :: TypeMismatch -> ErrorInfo
typeMismatchInfo (TypeMismatch reason (node1,_ty2) _t2) =
    ErrorInfo LevelError (posOfNode node1) [reason]

-- Redefinitions
-- ~~~~~~~~~~~~~

instance Show RedefError  where 
    show (RedefError lvl info) = showErrorInfo (redefErrLabel info) (redefErrorInfo lvl info)
instance Error RedefError where
    errorInfo (RedefError lvl info) = redefErrorInfo lvl info
    changeErrorLevel (RedefError _lvl info) lvl' = RedefError lvl' info 

redefErrLabel :: RedefInfo -> String
redefErrLabel  (RedefInfo ident _ _ _) = ident ++ " redefined"

redefErrorInfo :: ErrorLevel -> RedefInfo -> ErrorInfo
redefErrorInfo lvl info@(RedefInfo _ _ node old_node) = 
    ErrorInfo lvl (posOfNode node) ([redefErrReason info] ++ prevDeclMsg old_node)

redefErrReason :: RedefInfo -> String
redefErrReason (RedefInfo ident DuplicateDef _ _) = "duplicate definition of " ++ ident
redefErrReason (RedefInfo ident ShadowedDef _ _)   = "this declaration of " ++ ident ++ " shadows a previous one"
redefErrReason (RedefInfo ident DiffKindRedecl _ _) = ident ++ " previously declared as a different kind of symbol"

prevDeclMsg :: NodeInfo -> [String]
prevDeclMsg old_node = ["The previous declaration was here: ", show (posOfNode old_node)]

redefinition :: ErrorLevel -> String -> RedefKind -> NodeInfo -> NodeInfo -> RedefError
redefinition lvl ctx kind new old = RedefError lvl (RedefInfo ctx kind new old)

-- (helpers)
-- ~~~~~~~~~
mkErrInfo :: ErrorLevel -> String -> NodeInfo -> ErrorInfo
mkErrInfo lvl msg node = ErrorInfo lvl (posOfNode node) (lines msg)