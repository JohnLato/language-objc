{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis.DefTable
-- Copyright   :  (c) 2008 Benedikt Huber
--                  based on code from c2hs
--                (c) [1999..2001] Manuel M. T. Chakravarty
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- This module manages symbols in local and global scopes.
-----------------------------------------------------------------------------
module Language.C.Analysis.DefTable (
    DefTable(..),
    emptyDefTable,
    globalDefs,
    enterFunctionScope,leaveFunctionScope,enterBlockScope,leaveBlockScope,
    enterMemberDecl,leaveMemberDecl,
    DeclarationStatus(..),
    declareGlobalObject, declareScopedObject,
    declareTag,declareLabel,lookupObj,
    lookupTag,lookupLabel,lookupObjInner,lookupTagInner,
)
where
import Language.C.Syntax.Ident
import Language.C.Syntax.Name
import Language.C.Syntax.Error (todo)
import Language.C.Analysis.NameSpaceMap
import Language.C.Analysis.SemRep

import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map


{- Name spaces, scopes and contexts [Scopes]

 In C, there are 4 categories of identifiers:

  * labels
  * tag names (@(struct|union|enum) tag-name@), where /all/ tag names live in one namespace
  * members of structures and unions
  * identifiers, type-names and enumeration constants

 There are 4 kind of scopes:

  * file scope: outside of parameter lists and blocks
  * function prototype scope
  * function scope: labels are visible within the entire function, and declared implicitely
  * block scope

 While function scope is irrelevant for variable declarations, they might also appear in member declarations.
 Therefore, there are also 4 kinds of contexts where a variable might be declared:

  * File Scope Context: external declaration \/ definition
  * Block Scope Context: either external or local definition
  * Function prototype scope context
  * Member Declaration context

 See 
   <http://www.embedded.com/design/206901036>
   C99 6
-}
data DefTable = DefTable 
    {
        identDecls   :: NameSpaceMap Ident IdentDecl,       -- ^ defined objects
        tagDecls   :: NameSpaceMap SUERef TagDef,          -- ^ defined struct/union/enum  tags
        labelDefs  :: NameSpaceMap Ident Ident,            -- ^ defined labels
        memberDecls :: NameSpaceMap Ident MemberDecl,      -- ^ member declarations (only local)
        refTable   :: NameMap Name                         -- ^ link names with definitions
    }
emptyDefTable :: DefTable
emptyDefTable = DefTable nameSpaceMap nameSpaceMap nameSpaceMap nameSpaceMap emptyNameMap
    
globalDefs :: DefTable -> GlobalDecls
globalDefs symt = Map.foldWithKey insertDecl (GlobalDecls e e e gtags e) (globalNames $ identDecls symt)
    where
    e = Map.empty
    gtags =   globalNames (tagDecls symt)
    insertDecl ident def ds =
        case def of
            TypeDef tydef         -> ds { gTypedefs = Map.insert ident tydef (gTypedefs ds)}
            EnumDef _ _sueref     -> ds -- ignored, because the information is present in the enumeration anyway
            Declaration decl      | isFunctionType (declType decl) ->
                                      ds { gDecls = Map.insert ident (decl,True) (gDecls ds) }
                                  | otherwise ->
                                      ds { gDecls = Map.insert ident (decl,False) (gDecls ds) }
            FunctionDef funDef   -> ds { gFuns = Map.insert ident funDef (gFuns ds) }
            ObjectDef objDef     -> ds { gObjs = Map.insert ident objDef (gObjs ds) }
            
leaveScope_ :: (Ord k) => NameSpaceMap k a -> NameSpaceMap k a
leaveScope_ = fst . leaveScope

enterLocalScope :: DefTable -> DefTable
enterLocalScope symt = symt {
        identDecls = enterNewScope (identDecls symt),
        tagDecls = enterNewScope (tagDecls symt)
    }
leaveLocalScope :: DefTable -> DefTable
leaveLocalScope symt = symt {
                        identDecls = leaveScope_ (identDecls symt),
                        tagDecls = leaveScope_ (tagDecls symt)
                       }
-- | Enter function scope (and the corresponding block scope)
enterFunctionScope :: DefTable -> DefTable
enterFunctionScope symt = enterLocalScope  $ 
                          symt { labelDefs = enterNewScope (labelDefs symt) }

-- | Leave function scope, and return the associated DefTable.
--   Error if not in function scope.
leaveFunctionScope :: DefTable -> DefTable
leaveFunctionScope symt = leaveLocalScope $ symt { labelDefs = leaveScope_ (labelDefs symt) }

-- | Enter a block scope
enterBlockScope :: DefTable -> DefTable
enterBlockScope = enterLocalScope

leaveBlockScope :: DefTable -> DefTable
leaveBlockScope = leaveLocalScope

enterMemberDecl :: DefTable -> DefTable
enterMemberDecl symt = symt { memberDecls = enterNewScope (memberDecls symt) }

leaveMemberDecl :: DefTable -> ([MemberDecl], DefTable)
leaveMemberDecl symt = 
    let (decls',members) = leaveScope (memberDecls symt) 
    in (,) (map snd members)
           (symt { memberDecls = decls' })

-- * declarations
data DeclarationStatus t =
      NewDecl
    | Redeclared t
    | Shadowed t
    | DifferentKindRedecl t
            
defRedeclStatus :: (t -> t -> Bool) -> t -> Maybe t -> DeclarationStatus t
defRedeclStatus sameKind def oldDecl
    = case oldDecl of
          Just def' | def `sameKind` def' -> Redeclared def'
                    | otherwise           -> DifferentKindRedecl def'
          Nothing                         -> NewDecl
defRedeclStatusLocal :: (Ord k) =>
                        (t -> t -> Bool) -> k -> t -> Maybe t -> NameSpaceMap k t -> DeclarationStatus t
defRedeclStatusLocal sameKind ident def oldDecl nsm = 
    case defRedeclStatus sameKind def oldDecl of 
        NewDecl -> case lookupName nsm ident of
                     Just shadowed -> Shadowed shadowed
                     Nothing       -> NewDecl
        redecl  -> redecl

-- | declare\/define a global object\/function\/typedef
--
--  returns @Redeclared def@ if there is already an object\/function\/typedef
--  in global scope, or @DifferentKindRedec def@ if the old declaration is of a different kind.
declareGlobalObject :: Ident -> IdentDecl -> DefTable -> (DeclarationStatus IdentDecl, DefTable)
declareGlobalObject ident def symt
    = (defRedeclStatus compatibleObjKind def oldDecl, symt { identDecls = decls' })
    where
    (decls',oldDecl) = defGlobal (identDecls symt) ident def

-- | declare\/define a object\/function\/typedef with lexical scope
--
--  returns @Redeclared def@ or @DifferentKindRedec def@  if there is already an object\/function\/typedef
--  in the same scope.
declareScopedObject :: Ident -> IdentDecl -> DefTable -> (DeclarationStatus IdentDecl, DefTable)
declareScopedObject ident def symt
    = (redeclStatus, symt { identDecls = decls' })
    where
    (decls',olddecl) = defLocal (identDecls symt) ident def
    redeclStatus = defRedeclStatusLocal compatibleObjKind ident def olddecl (identDecls symt)

-- | declare\/define an tag
--
declareTag :: SUERef -> TagDef -> DefTable -> (DeclarationStatus TagDef, DefTable)
declareTag sueref tagdef symt
    =  (redeclStatus, symt { tagDecls = decls'})    
    where
    (decls',olddecl) = defLocal (tagDecls symt) sueref tagdef
    redeclStatus = defRedeclStatusLocal sameTagKind sueref tagdef olddecl (tagDecls symt)

-- | define a label
-- Return the old label if it is already defined in this function's scope
--
-- /TODO/: implement
declareLabel :: Ident -> DefTable -> (DefTable, Maybe Ident) 
declareLabel = todo "DefTable.declareLabel"

-- | lookup object
lookupObj :: Ident -> DefTable -> Maybe IdentDecl
lookupObj ident symt = lookupName (identDecls symt) ident
    
-- | lookup tag
lookupTag :: SUERef -> DefTable -> Maybe TagDef
lookupTag sue_ref symt = lookupName (tagDecls symt) sue_ref

-- | lookup label
lookupLabel :: Ident -> DefTable -> Maybe Ident
lookupLabel ident symt = lookupName (labelDefs symt) ident

-- | lookup an object in the innermost scope
lookupObjInner :: Ident -> DefTable -> Maybe IdentDecl
lookupObjInner = undefined
lookupTagInner :: SUERef -> DefTable -> Maybe TagDef
lookupTagInner = undefined