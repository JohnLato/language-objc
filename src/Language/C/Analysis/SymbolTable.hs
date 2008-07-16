{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis.SymbolTable
-- Copyright   :  (c) 2008 Benedikt Huber
--                  based on code from c2hs
--                (c) [1999..2001] Manuel M. T. Chakravarty
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- This module manages symbols in local and global scopes.
--
-- In C, there are 4 categories of identifiers:
--
--  * labels
--
--  * tag names (@(struct|union|enum) tag-name@), where /all/ tag names live in one namespace
--
--  * members of structures and unions
--
--  * identifiers, type-names and enumeration constants
--
-- There are 4 kind of scopes:
--
--  * file scope: outside of parameter lists and blocks
--
--  * function prototype scope
--
--  * function scope: labels are visible within the entire function, and declared implicitely
--
--  * block scope
--
-- While function scope is irrelevant for variable declarations, they might also appear in member declarations.
-- Therefore, there are also 4 kinds of contexts where a variable might be declared:
--
--  * File Scope Context: external declaration \/ definition
--
--  * Block Scope Context: either external or local definition
--
--  * Function prototype scope context
--
--  * Member Declaration context
--
-- See 
--   <http://www.embedded.com/design/206901036>
--   C99 6
-----------------------------------------------------------------------------
module Language.C.Analysis.SymbolTable (
    SymbolTable(..),DeclContext(..),declContext,
    emptySymbolTable,
    globalDefs,
    enterFunctionScope,leaveFunctionScope,enterBlockScope,leaveBlockScope,
    enterMemberDecl,leaveMemberDecl,
    DeclarationStatus(..),
    declareObject,declareTag,declareLabel,lookupObj,
    lookupTag,lookupLabel,lookupObjInner,lookupTagInner,
)
where
import Language.C.Common.Ident
import Language.C.Common.Name
import Language.C.Analysis.NameSpaceMap
import Language.C.Analysis.SemRep

import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map

data SymbolTable = SymbolTable 
    {
        identDecls   :: NameSpaceMap Ident IdentDef,       -- ^ defined objects
        tagDecls   :: NameSpaceMap SueRef TagDef,          -- ^ defined struct/union/enum  tags
        labelDefs  :: NameSpaceMap Ident Ident,            -- ^ defined labels
        memberDecls :: NameSpaceMap Ident MemberDecl,      -- ^ member declarations (only local)
        declContexts :: [DeclContext],                        -- ^ context stack
        refTable   :: NameMap Name                         -- ^ link names with definitions
    }
emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable nameSpaceMap nameSpaceMap nameSpaceMap nameSpaceMap [FileCtx] emptyNameMap
-- | variables can be declared in 4 different situations
--  * in file scope (external declarations)
--  * in function prototype scope
--  * in block scope (local declarations)
--  * in member declarations
data DeclContext = FileCtx | ProtoCtx | BlockCtx | MemberCtx
                   deriving (Eq,Ord)
addContext :: DeclContext -> SymbolTable -> SymbolTable
addContext ctx symt = symt { declContexts = ctx : (declContexts symt) }
declContext :: SymbolTable -> DeclContext
declContext = head . declContexts
    
globalDefs :: SymbolTable -> GlobalDecls
globalDefs symt = Map.foldWithKey insertDecl (GlobalDecls e e gtags e e) (globalNames $ identDecls symt)
    where
    e = Map.empty
    gtags =   globalNames (tagDecls symt)
    insertDecl ident def ds =
        case def of
            TypeDefIdent tyName   -> ds { gTypedefs = Map.insert ident tyName (gTypedefs ds)}
            EnumIdent _name sueref -> ds { gEnums = Map.insert ident sueref (gEnums ds) }
            DeclIdent vardecl | isFunctionType (declType vardecl) ->
                                    ds { gFuns = Map.insert ident (Left vardecl) (gFuns ds) }
                              | otherwise ->
                                    ds { gObjs = Map.insert ident (Left vardecl) (gObjs ds) }
            DefIdent (Left funDef) -> ds { gFuns = Map.insert ident (Right funDef) (gFuns ds) }
            DefIdent (Right varDef) -> ds { gObjs = Map.insert ident (Right varDef) (gObjs ds) }
            
    
leaveScope_ :: (Ord k) => NameSpaceMap k a -> NameSpaceMap k a
leaveScope_ = fst . leaveScope

enterLocalScope :: SymbolTable -> SymbolTable
enterLocalScope symt = symt {
        identDecls = enterNewScope (identDecls symt),
        tagDecls = enterNewScope (tagDecls symt)
    }
leaveLocalScope :: SymbolTable -> SymbolTable
leaveLocalScope symt = symt {
                        identDecls = leaveScope_ (identDecls symt),
                        tagDecls = leaveScope_ (tagDecls symt),
                        declContexts = tail (declContexts symt)
                       }
-- | Enter function scope (and the corresponding block scope)
enterFunctionScope :: SymbolTable -> SymbolTable
enterFunctionScope symt = enterLocalScope . addContext BlockCtx $ 
                          symt { labelDefs = enterNewScope (labelDefs symt) }

-- | Leave function scope, and return the associated SymbolTable.
--   Error if not in function scope.
leaveFunctionScope :: SymbolTable -> SymbolTable
leaveFunctionScope symt = leaveLocalScope $ symt { labelDefs = leaveScope_ (labelDefs symt) }

-- | Enter a block scope
enterBlockScope :: DeclContext -> SymbolTable -> SymbolTable
enterBlockScope ctx = enterLocalScope . addContext ctx

leaveBlockScope :: SymbolTable -> SymbolTable
leaveBlockScope = leaveLocalScope

enterMemberDecl :: SymbolTable -> SymbolTable
enterMemberDecl symt = addContext MemberCtx $
                       symt { memberDecls = enterNewScope (memberDecls symt) }

leaveMemberDecl :: SymbolTable -> ([MemberDecl], SymbolTable)
leaveMemberDecl symt = 
    let (decls',members) = leaveScope (memberDecls symt) 
    in (,) (map snd members)
           (symt { memberDecls = decls', declContexts = tail (declContexts symt) })

-- * declarations
data DeclarationStatus t =
      DifferentKindRedecl t
    | ShadowDecl t
    | SameScopeRedecl t
    | OuterScopeRedecl t
    
-- | declare\/define an object\/function\/typedef
-- * If there already is an object\/function with the same name
--   * in the same scope, which is an enumerator or typedef, overwrite
--     returns (DifferentKindRedecl def)
--   * in the same scope, define \/ overwrite the existing declaration
--     returns (SameScopeRedeclaration def), if the object is already defined in this scope
--   * in an outer scope, and the object has an `extern` specifier, and the outer scope object has different kind
--     returns (DifferentKindRedecl def)
--   * in an outer scope, and the object has an `extern` specifier, define \/ overwrite the existing declaration
--     returns (OuterScopeRedeclaration def), if the object is already defined in some scope
--   * in an outer scope, and the object has no external specifier, shadow the existing declaration
--     return  (ShadowedDeclaration def), if an declaration is shadowed
declareObject :: Ident -> IdentDef -> SymbolTable -> (SymbolTable, DeclarationStatus IdentDef)
declareObject = undefined

-- | declare\/define an tag
-- For scoping rules, see 'declareObject'
declareTag :: SueRef -> SymbolTable -> (SymbolTable, DeclarationStatus SueRef)
declareTag = undefined

-- | define a label
-- Return the old label if it is already defined in this function's scope
declareLabel :: Ident -> SymbolTable -> (SymbolTable, Maybe Ident) 
declareLabel = undefined

-- | lookup object
lookupObj :: Ident -> SymbolTable -> Maybe IdentDef
lookupObj = undefined
-- | lookup tag
lookupTag :: SueRef -> SymbolTable -> Maybe TagDef
lookupTag = undefined
-- | lookup label
lookupLabel :: Ident -> SymbolTable -> Maybe Ident
lookupLabel = undefined

-- internal
-- | lookup an object in the innermost scope
lookupObjInner :: Ident -> SymbolTable -> Maybe IdentDef
lookupObjInner = undefined
lookupTagInner :: SueRef -> SymbolTable -> Maybe TagDef
lookupTagInner = undefined