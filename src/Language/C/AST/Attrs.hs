--  C->Haskell Compiler: C attribute definitions and manipulation routines
--
--  Author : Manuel M. T. Chakravarty
--  Created: 12 August 99
--
--  Copyright (c) [1999..2001] Manuel M. T. Chakravarty
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  This module provides the attributed version of the C structure tree.
--
--  * C has several name spaces of which two are represented in this module:
--    - `CObj' in `defObjsAC': The name space of objects, functions, typedef
--        names, and enum constants.
--    - `CTag' in `defTagsAC': The name space of tags of structures, unions,
--        and enumerations. 
--
--  * The final state of the names spaces are preserved in the attributed
--    structure tree.  This allows further fast lookups for globally defined
--    identifiers after the name anaysis is over.
--
--  * In addition to the name spaces, the attribute structure tree contains
--    a ident-definition table, which for attribute handles of identifiers
--    refers to the identifiers definition.  These are only used in usage
--    occurences, except for one exception: The tag identifiers in forward
--    definitions of structures or enums get a reference to the corresponding
--    full definition - see `CTrav' for full details.
--
--  * We maintain a shadow definition table, it can be populated with aliases
--    to other objects and maps identifiers to identifiers.  It is populated by
--    using the `applyPrefix' function.  When looksup performed via the shadow
--    variant of a lookup function, shadow aliases are also considered, but
--    they are used only if no normal entry for the identifiers is present.
--
--  * Only ranges delimited by a block open a new range for tags (see
--    `enterNewObjRangeC' and `leaveObjRangeC').
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--- TODO ----------------------------------------------------------------------
--

module Language.C.AST.Attrs (-- attributed C
	       --
	       AttrC, emptyAttrC, enterNewRangeC, enterNewObjRangeC,
	       leaveRangeC, leaveObjRangeC, addDefObjC, lookupDefObjC,
	       lookupDefObjCShadow, addDefTagC, lookupDefTagC,
	       lookupDefTagCShadow, applyPrefix, getDefOfIdentC,
	       setDefOfIdentC, updDefOfIdentC, freezeDefOfIdentsAttrC,
	       softenDefOfIdentsAttrC,
	       --
	       -- C objects
	       --
	       CObj(..), CTag(..), CDef(..))
where

import Data.Char  (toUpper)
import Data.Maybe (mapMaybe)

import Language.C.Toolkit.Position   (Pos(posOf), nopos, dontCarePos, builtinPos)
import Language.C.Toolkit.Errors     (interr)
import Language.C.Toolkit.Idents	  (Ident, getIdentAttrs, identToLexeme, onlyPosIdent)
import Language.C.Toolkit.Attributes (Attr(..), AttrTable, getAttr, setAttr, updAttr,
		   newAttrTable, freezeAttrTable, softenAttrTable)
import Language.C.Toolkit.NameSpaces (NameSpace, nameSpace, enterNewRange, leaveRange, defLocal,
		   defGlobal, find, nameSpaceToList)

import Language.C.AST.AST


-- attributed C structure tree
-- ---------------------------

-- attributes relevant to the outside world gathjered from a C unit
-- (EXPORTED ABSTRACT)
--
data AttrC = AttrC {
		defObjsAC :: CObjNS,		-- defined objects
		defTagsAC :: CTagNS,		-- defined tags
		shadowsAC :: CShadowNS,		-- shadow definitions (prefix)
		defsAC    :: CDefTable		-- ident-def associations
	      }

-- empty headder attribute set (EXPORTED)
--
emptyAttrC :: AttrC
emptyAttrC  = AttrC {
	     defObjsAC = cObjNS,
	     defTagsAC = cTagNS,
	     shadowsAC = cShadowNS,
	     defsAC    = cDefTable
	   }


-- the name space operations
--

-- enter a new range (EXPORTED)
--
enterNewRangeC    :: AttrC -> AttrC
enterNewRangeC ac  = ac {
		      defObjsAC = enterNewRange . defObjsAC $ ac,
		      defTagsAC = enterNewRange . defTagsAC $ ac
		     }

-- enter a new range, only for objects (EXPORTED)
--
enterNewObjRangeC    :: AttrC -> AttrC
enterNewObjRangeC ac  = ac {
		          defObjsAC = enterNewRange . defObjsAC $ ac
		        }

-- leave the current range (EXPORTED)
--
leaveRangeC    :: AttrC -> AttrC
leaveRangeC ac  = ac {
		    defObjsAC = fst . leaveRange . defObjsAC $ ac,
		    defTagsAC = fst . leaveRange . defTagsAC $ ac
		   }

-- leave the current range, only for objects (EXPORTED)
--
leaveObjRangeC    :: AttrC -> AttrC
leaveObjRangeC ac  = ac {
		       defObjsAC = fst . leaveRange . defObjsAC $ ac
		     }

-- add another definitions to the object name space (EXPORTED)
--
-- * if a definition of the same name was already present, it is returned
--
addDefObjC            :: AttrC -> Ident -> CObj -> (AttrC, Maybe CObj)
addDefObjC ac ide obj  = let om          = defObjsAC ac
			     (ac', obj') = defLocal om ide obj
			 in
			 (ac {defObjsAC = ac'}, obj')

-- lookup an identifier in the object name space (EXPORTED)
--
lookupDefObjC        :: AttrC -> Ident -> Maybe CObj
lookupDefObjC ac ide  = find (defObjsAC ac) ide

-- lookup an identifier in the object name space; if nothing found, try 
-- whether there is a shadow identifier that matches (EXPORTED)
--
-- * the returned identifier is the _real_ identifier of the object
--
lookupDefObjCShadow        :: AttrC -> Ident -> Maybe (CObj, Ident)
lookupDefObjCShadow ac ide  = 
  case lookupDefObjC ac ide of
    Just obj -> Just (obj, ide)
    Nothing  -> case find (shadowsAC ac) ide of
		  Nothing   -> Nothing
		  Just ide' -> case lookupDefObjC ac ide' of
			         Just obj -> Just (obj, ide')
				 Nothing  -> Nothing

-- add another definition to the tag name space (EXPORTED)
--
-- * if a definition of the same name was already present, it is returned 
--
addDefTagC            :: AttrC -> Ident -> CTag -> (AttrC, Maybe CTag)
addDefTagC ac ide obj  = let tm          = defTagsAC ac
			     (ac', obj') = defLocal tm ide obj
			 in
			 (ac {defTagsAC = ac'}, obj')

-- lookup an identifier in the tag name space (EXPORTED)
--
lookupDefTagC        :: AttrC -> Ident -> Maybe CTag
lookupDefTagC ac ide  = find (defTagsAC ac) ide

-- lookup an identifier in the tag name space; if nothing found, try 
-- whether there is a shadow identifier that matches (EXPORTED)
--
-- * the returned identifier is the _real_ identifier of the tag
--
lookupDefTagCShadow        :: AttrC -> Ident -> Maybe (CTag, Ident)
lookupDefTagCShadow ac ide  = 
  case lookupDefTagC ac ide of
    Just tag -> Just (tag, ide)
    Nothing  -> case find (shadowsAC ac) ide of
		  Nothing   -> Nothing
		  Just ide' -> case lookupDefTagC ac ide' of
			         Just tag -> Just (tag, ide')
				 Nothing  -> Nothing

-- enrich the shadow name space with identifiers obtained by dropping
-- the given prefix from the identifiers already in the object or tag name
-- space (EXPORTED)
--
-- * in case of a collisions, a random entry is selected
-- 
-- * case is not relevant in the prefix and underscores between the prefix and
--   the stem of an identifier are also dropped
-- 
applyPrefix           :: AttrC -> String -> AttrC
applyPrefix ac prefix  =
  let 
    shadows    = shadowsAC ac
    names      =    map fst (nameSpaceToList (defObjsAC ac))
	         ++ map fst (nameSpaceToList (defTagsAC ac))
    newShadows = mapMaybe (strip prefix) names
  in
  ac {shadowsAC = foldl define shadows newShadows}
  where
    strip prefix ide = case eat prefix (identToLexeme ide) of
		         Nothing      -> Nothing
			 Just ""      -> Nothing
			 Just newName -> Just 
					   (onlyPosIdent (posOf ide) newName,
					    ide)
    --
    eat []         ('_':cs)                        = eat [] cs
    eat []         cs                              = Just cs
    eat (p:prefix) (c:cs) | toUpper p == toUpper c = eat prefix cs
			  | otherwise		   = Nothing
    eat _          _				   = Nothing
    --
    define ns (ide, def) = fst (defGlobal ns ide def)


-- the attribute table operations on the attributes
--

-- get the definition associated with the given identifier (EXPORTED)
--
getDefOfIdentC    :: AttrC -> Ident -> CDef
getDefOfIdentC ac  = getAttr (defsAC ac) . getIdentAttrs

setDefOfIdentC           :: AttrC -> Ident -> CDef -> AttrC
setDefOfIdentC ac id def  = 
  let tot' = setAttr (defsAC ac) (getIdentAttrs id) def
  in
  ac {defsAC = tot'}

updDefOfIdentC            :: AttrC -> Ident -> CDef -> AttrC
updDefOfIdentC ac id def  = 
  let tot' = updAttr (defsAC ac) (getIdentAttrs id) def
  in
  ac {defsAC = tot'}

freezeDefOfIdentsAttrC    :: AttrC -> AttrC
freezeDefOfIdentsAttrC ac  = ac {defsAC = freezeAttrTable (defsAC ac)}

softenDefOfIdentsAttrC    :: AttrC -> AttrC
softenDefOfIdentsAttrC ac  = ac {defsAC = softenAttrTable (defsAC ac)}


-- C objects including operations
-- ------------------------------

-- C objects data definition (EXPORTED)
--
data CObj = TypeCO    CDecl		-- typedef declaration
	  | ObjCO     CDecl		-- object or function declaration
	  | EnumCO    Ident CEnum	-- enumerator
	  | BuiltinCO			-- builtin object

-- two C objects are equal iff they are defined by the same structure
-- tree node (i.e., the two nodes referenced have the same attribute
-- identifier)
--
instance Eq CObj where
  (TypeCO decl1     ) == (TypeCO decl2     ) = decl1 == decl2
  (ObjCO  decl1     ) == (ObjCO  decl2     ) = decl1 == decl2
  (EnumCO ide1 enum1) == (EnumCO ide2 enum2) = ide1 == ide2 && enum1 == enum2
  _		      == _		     = False

instance Pos CObj where
  posOf (TypeCO    def  ) = posOf def
  posOf (ObjCO     def  ) = posOf def
  posOf (EnumCO    ide _) = posOf ide
  posOf (BuiltinCO      ) = builtinPos


-- C tagged objects including operations
-- -------------------------------------

-- C tagged objects data definition (EXPORTED)
--
data CTag = StructUnionCT CStructUnion	-- toplevel struct-union declaration
	  | EnumCT        CEnum		-- toplevel enum declaration

-- two C tag objects are equal iff they are defined by the same structure
-- tree node (i.e., the two nodes referenced have the same attribute
-- identifier)
--
instance Eq CTag where
  (StructUnionCT struct1) == (StructUnionCT struct2) = struct1 == struct2
  (EnumCT        enum1  ) == (EnumCT        enum2  ) = enum1 == enum2
  _			  == _			     = False

instance Pos CTag where
  posOf (StructUnionCT def) = posOf def
  posOf (EnumCT        def) = posOf def


-- C general definition
-- --------------------

-- C general definition (EXPORTED)
--
data CDef = UndefCD			-- undefined object
	  | DontCareCD			-- don't care object
	  | ObjCD      CObj		-- C object
	  | TagCD      CTag		-- C tag

-- two C definitions are equal iff they are defined by the same structure
-- tree node (i.e., the two nodes referenced have the same attribute
-- identifier), but don't care objects are equal to everything and undefined
-- objects may not be compared
--
instance Eq CDef where
  (ObjCD obj1) == (ObjCD obj2) = obj1 == obj2
  (TagCD tag1) == (TagCD tag2) = tag1 == tag2
  DontCareCD   == _	       = True
  _	       == DontCareCD   = True
  UndefCD      == _	       = 
    interr "CAttrs: Attempt to compare an undefined C definition!"
  _	       == UndefCD      = 
    interr "CAttrs: Attempt to compare an undefined C definition!"
  _	       == _	       = False

instance Attr CDef where
  undef    = UndefCD
  dontCare = DontCareCD

  isUndef UndefCD = True
  isUndef _	  = False

  isDontCare DontCareCD = True
  isDontCare _		= False

instance Pos CDef where
  posOf UndefCD     = nopos
  posOf DontCareCD  = dontCarePos
  posOf (ObjCD obj) = posOf obj
  posOf (TagCD tag) = posOf tag


-- object tables (internal use only)
-- ---------------------------------

-- the object name spavce
--
type CObjNS = NameSpace CObj

-- creating a new object name space
--
cObjNS :: CObjNS
cObjNS  = nameSpace

-- the tag name space
--
type CTagNS = NameSpace CTag

-- creating a new tag name space
--
cTagNS :: CTagNS
cTagNS  = nameSpace

-- the shadow name space
--
type CShadowNS = NameSpace Ident

-- creating a shadow name space
--
cShadowNS :: CShadowNS
cShadowNS  = nameSpace

-- the general definition table
--
type CDefTable = AttrTable CDef

-- creating a new definition table
--
cDefTable :: CDefTable
cDefTable  = newAttrTable "C General Definition Table for Idents"
