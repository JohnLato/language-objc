{-# LANGUAGE PatternGuards  #-}
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
--
-- There are four different kind of identifiers: ordinary identifiers (henceforth
-- simply called `identifier'), tags (names of struct\/union\/enum types),
-- labels and structure members.
-----------------------------------------------------------------------------
module Language.C.Analysis.DefTable (
    DefTable(..),
    emptyDefTable,
    globalDefs,
    enterFunctionScope,leaveFunctionScope,enterBlockScope,leaveBlockScope,
    enterMemberDecl,leaveMemberDecl,
    DeclarationStatus(..),
    defineGlobalIdent, defineScopedIdent, defineScopedIdentWhen,
    defineTag,defineLabel,lookupIdent,
    lookupTag,lookupLabel,lookupIdentInner,lookupTagInner,
)
where
import Language.C.Syntax.Ident
import Language.C.Syntax.Name
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
  * ordinary identifiers, denoting objects, functions, typedefs and enumeration constants

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
        identDecls   :: NameSpaceMap Ident IdentDecl,      -- ^ defined `ordinary identifiers'
        tagDecls   :: NameSpaceMap SUERef TagDef,          -- ^ defined struct/union/enum  tags
        labelDefs  :: NameSpaceMap Ident Ident,            -- ^ defined labels
        memberDecls :: NameSpaceMap Ident MemberDecl,      -- ^ member declarations (only local)
        refTable   :: NameMap Name                         -- ^ link names with definitions
    }
emptyDefTable :: DefTable
emptyDefTable = DefTable nameSpaceMap nameSpaceMap nameSpaceMap nameSpaceMap emptyNameMap
    
globalDefs :: DefTable -> GlobalDecls
globalDefs deftbl = Map.foldWithKey insertDecl (GlobalDecls e e e gtags e) (globalNames $ identDecls deftbl)
    where
    e = Map.empty
    gtags =   globalNames (tagDecls deftbl)
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
enterLocalScope deftbl = deftbl {
        identDecls = enterNewScope (identDecls deftbl),
        tagDecls = enterNewScope (tagDecls deftbl)
    }
leaveLocalScope :: DefTable -> DefTable
leaveLocalScope deftbl = deftbl {
                        identDecls = leaveScope_ (identDecls deftbl),
                        tagDecls = leaveScope_ (tagDecls deftbl)
                       }
-- | Enter function scope (and the corresponding block scope)
enterFunctionScope :: DefTable -> DefTable
enterFunctionScope deftbl = enterLocalScope  $ 
                          deftbl { labelDefs = enterNewScope (labelDefs deftbl) }

-- | Leave function scope, and return the associated DefTable.
--   Error if not in function scope.
leaveFunctionScope :: DefTable -> DefTable
leaveFunctionScope deftbl = leaveLocalScope $ deftbl { labelDefs = leaveScope_ (labelDefs deftbl) }

-- | Enter a block scope
enterBlockScope :: DefTable -> DefTable
enterBlockScope = enterLocalScope

leaveBlockScope :: DefTable -> DefTable
leaveBlockScope = leaveLocalScope

enterMemberDecl :: DefTable -> DefTable
enterMemberDecl deftbl = deftbl { memberDecls = enterNewScope (memberDecls deftbl) }

leaveMemberDecl :: DefTable -> ([MemberDecl], DefTable)
leaveMemberDecl deftbl = 
    let (decls',members) = leaveScope (memberDecls deftbl) 
    in (,) (map snd members)
           (deftbl { memberDecls = decls' })

-- * declarations
data DeclarationStatus t =
      NewDecl
    | Redeclared t
    | KeepDef t
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
defineGlobalIdent :: Ident -> IdentDecl -> DefTable -> (DeclarationStatus IdentDecl, DefTable)
defineGlobalIdent ident def deftbl
    = (defRedeclStatus compatibleObjKind def oldDecl, deftbl { identDecls = decls' })
    where
    (decls',oldDecl) = defGlobal (identDecls deftbl) ident def

-- | declare\/define a object\/function\/typedef with lexical scope
--
--  returns @Redeclared def@ or @DifferentKindRedec def@  if there is already an object\/function\/typedef
--  in the same scope.
defineScopedIdent :: Ident -> IdentDecl -> DefTable -> (DeclarationStatus IdentDecl, DefTable)
defineScopedIdent = defineScopedIdentWhen (const True)

-- | declare\/define a object\/function\/typedef with lexical scope, if the given predicate holds on the old
--   entry.
--
--  returns @Redeclared def@ or @DifferentKindRedec def@  if there is already an object\/function\/typedef
--  in the same scope.
defineScopedIdentWhen :: (IdentDecl -> Bool) -> Ident -> IdentDecl -> DefTable -> 
                           (DeclarationStatus IdentDecl, DefTable)
defineScopedIdentWhen do_override ident def deftbl
    = (redecl_status, deftbl { identDecls = decls' })
    where
    old_decls = identDecls deftbl
    old_decl_opt = lookupInnermostScope old_decls ident
    (decls',redecl_status)  | (Just old_decl) <- old_decl_opt, not (old_decl `compatibleObjKind` def) 
                              = (new_decls, DifferentKindRedecl old_decl)
                            | maybe True do_override old_decl_opt
                              = (new_decls, redeclStatus old_decl_opt)
                            | otherwise
                              = (old_decls, maybe NewDecl KeepDef old_decl_opt)
    new_decls = fst (defLocal old_decls ident def)
    redeclStatus overriden_decl = defRedeclStatusLocal compatibleObjKind ident def overriden_decl old_decls

-- | declare\/define an tag
--
defineTag :: SUERef -> TagDef -> DefTable -> (DeclarationStatus TagDef, DefTable)
defineTag sueref tagdef deftbl
    =  (redeclStatus, deftbl { tagDecls = decls'})    
    where
    (decls',olddecl) = defLocal (tagDecls deftbl) sueref tagdef
    redeclStatus = defRedeclStatusLocal sameTagKind sueref tagdef olddecl (tagDecls deftbl)

-- | define a label
-- Return the old label if it is already defined in this function's scope
--
-- /TODO/: implement
defineLabel :: Ident -> DefTable -> (DeclarationStatus Ident, DefTable) 
defineLabel ident deftbl = 
    let (labels',old_label) = defLocal (labelDefs deftbl) ident ident
    in  (maybe NewDecl Redeclared old_label, deftbl { labelDefs = labels' })
        

-- | lookup identifier (object, function, typedef, enumerator)
lookupIdent :: Ident -> DefTable -> Maybe IdentDecl
lookupIdent ident deftbl = lookupName (identDecls deftbl) ident
    
-- | lookup tag
lookupTag :: SUERef -> DefTable -> Maybe TagDef
lookupTag sue_ref deftbl = lookupName (tagDecls deftbl) sue_ref

-- | lookup label
lookupLabel :: Ident -> DefTable -> Maybe Ident
lookupLabel ident deftbl = lookupName (labelDefs deftbl) ident

-- | lookup an object in the innermost scope
lookupIdentInner :: Ident -> DefTable -> Maybe IdentDecl
lookupIdentInner ident deftbl = lookupInnermostScope (identDecls deftbl) ident

-- | lookup an identifier in the innermost scope
lookupTagInner :: SUERef -> DefTable -> Maybe TagDef
lookupTagInner sue_ref deftbl = lookupInnermostScope (tagDecls deftbl) sue_ref