-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Toolkit.NameSpaceMap
-- Copyright   :  (c) [1995..1999] Manuel M. T. Chakravarty
--                (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- This module manages name spaces.
--
--  * A name space map associates identifiers with their definition.
--
--  * Each name space map is organized in a hierarchical way using the notion of
--    scopes. A name space map, at any moment, always has a global scope and may
--    have several local scopes. Definitions in inner scopes hide definitions
--    of the same identifier in outer scopes.
--
module Language.C.Toolkit.NameSpaceMap (
    NameSpaceMap, nameSpaceMap, 
    defGlobal, 
    enterNewScope, leaveScope,
    defLocal, 
    find, 
    nsMapToList)
where

import qualified Data.Map as Map (empty, insert, lookup, toList)
import Data.Map   (Map)
import Language.C.Toolkit.Idents     (Ident)
import Language.C.Toolkit.Errors     (interr)


-- | @NameSpaceMap a@ is a Map from identifiers to @a@, which manages
-- global and local name spaces.

-- DevDocs:
--
-- * the definitions in the global scope are stored in a finite map, because
--   they tend to be a lot and are normally not updated after the global scope
--   is constructed
--
-- * the definitions of the local scopes are stored in a single list, usually
--   they are not very many and the definitions entered last are the most
--   frequently accessed ones; the list structure naturally hides older
--   definitions, i.e., definitions from outer scopes; adding new definitions
--   is done in time proportinal to the current size of the scope; removing a
--   scope is done in constant time (and the definitions of a scope can be
--   returned as a result of leaving the scope); lookup is proportional to the
--   number of definitions in the local scopes and the logarithm of the number
--   of definitions in the global scope -- i.e., efficiency relies on a
--   relatively low number of local definitions together with frequent lookup
--   of the most recently defined local identifiers
--
data NameSpaceMap a = NsMap (Map Ident a)  -- defs in global scope
                             [[(Ident, a)]]       -- stack of local scopes

-- | create a name space
nameSpaceMap :: NameSpaceMap a
nameSpaceMap  = NsMap Map.empty []

-- | Add global definition
--
-- @(ns',oldDef) = defGlobal ns ident def@ 
--   adds a global definition @ident := def@ to the namespace.
--   It returns the modified namespace @ns'@. If the identifier is
--   already declared in the global namespace, the definition is overwritten
--   and the old definition @oldDef@ is returned.
defGlobal :: NameSpaceMap a -> Ident -> a -> (NameSpaceMap a, Maybe a)
defGlobal (NsMap gs lss) ident def  
    = (NsMap (Map.insert ident def gs) lss, Map.lookup ident gs)

-- | Enter new local scope
--
-- @ns' = enterNewScope ns@ creates and enters a new local scope.
enterNewScope                    :: NameSpaceMap a -> NameSpaceMap a
enterNewScope (NsMap gs lss)  = NsMap gs ([]:lss)

-- | Leave innermost local scope 
--
-- @(ns',defs) = leaveScope ns@ pops leaves the innermost local scope.
--  and returns its definitions
leaveScope :: NameSpaceMap a -> (NameSpaceMap a, [(Ident, a)])
leaveScope (NsMap _ [])         = interr "NsMaps.leaveScope: No local scope!"
leaveScope (NsMap gs (ls:lss))  = (NsMap gs lss, ls)

-- | Add local definition 
--
-- @(ns',oldDef) = defLocal ns ident def@ adds the local definition
--   @ident := def@ to the innermost local scope, if there is a local scope, 
--     and to the global scope otherwise. 
--   It returns the modified name space @ns'@ and the old  binding of
--   the identifier @oldDef@, which is overwritten.
defLocal :: NameSpaceMap a -> Ident -> a -> (NameSpaceMap a, Maybe a)
defLocal ns@(NsMap _ []) ident def = defGlobal ns ident def
defLocal (NsMap    gs (ls:lss)) ident def = 
  (NsMap gs (((ident, def):ls):lss),
   lookupLocal ls)
  where
    lookupLocal []                          = Nothing
    lookupLocal ((ident', localDef):defs) | ident == ident' = Just localDef
                                        | otherwise = lookupLocal defs
       
-- | Search for a definition 
--
-- @def = find ns ident@ returns the definition in the innermost scope,
-- if there is one.
find                       :: NameSpaceMap a -> Ident -> Maybe a
find (NsMap gs localDefs) ident  
    = case (lookupLocal localDefs) of
        Nothing  -> Map.lookup ident gs
        Just def -> Just def
  where
    lookupLocal []       = Nothing
    lookupLocal (ls:lss) = case (lookupLocal' ls) of
                        Nothing  -> lookupLocal lss
                        Just def -> Just def

    lookupLocal' []              = Nothing
    lookupLocal' ((ident', def):ls)
            | ident' == ident     = Just def
            | otherwise     = lookupLocal' ls

-- | flatten a namespace into a assoc list
--
--  @nameSpaceToList ns = globalDefs ns ++ (localDefInnermost ns ++ .. ++ localDefsOutermost ns)@
-- TODO: order does not reflect lookup order (innermost .. outermost, global)
nsMapToList :: NameSpaceMap a -> [(Ident, a)]
nsMapToList (NsMap gs lss)  = Map.toList gs ++ concat lss
