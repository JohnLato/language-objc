{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Syntax.Ident
-- Copyright   :  (c) [1995..1999] Manuel M. T. Chakravarty
--                (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- This module provides the notion of identifiers in C, speed up using hashing.
-- Identifiers are associated with a 'NodeInfo', i.e. with a unique 'Name' and 
-- a source code position 'Pos'

-- 
--  * We speed up the equality test between identifiers by keeping a hash
--
--  * The ordering relation on identifiers is also based on the hash and,
--    hence, does *not* follow the alphanumerical ordering of the lexemes of
--    the identifiers. Instead, it provides a fast ordering when identifiers
--    are used as keys in a `Data.Map'.
--
--  * Identifiers shall be associated with a unique name, unless they are `internal'
--
--- TODO ----------------------------------------------------------------------
--
--  * Hashing is not 8bit clean.
--
module Language.C.Syntax.Ident (
    Ident(..), SUERef(..), mkIdent, internalIdent, builtinIdent, isInternalIdent, identToString, dumpIdent)
where

import Data.Char
import Language.C.Syntax.Position
import Language.C.Syntax.Node
import Language.C.Syntax.Name   (Name,nameId)
import Data.Generics

-- simple identifier representation (EXPORTED)
--
data Ident = Ident String       -- lexeme
                   {-# UNBOXED #-}   !Int         -- hash to speed up equality check
                   NodeInfo        -- attributes of this ident. incl. position
             deriving (Data,Typeable)

data SUERef =  AnonymousType Name
             | NamedType Ident
    deriving (Typeable, Data, Ord, Eq)
instance Show SUERef where
    show (AnonymousType name) = "$" ++ show (nameId name)
    show (NamedType ident) = identToString ident
    
-- the definition of the equality allows identifiers to be equal that are
-- defined at different source text positions, and aims at speeding up the
-- equality test, by comparing the lexemes only if the two numbers are equal
--
instance Eq Ident where
  (Ident s h _) == (Ident s' h' _) = (h == h') && (s == s')

-- this does *not* follow the alphanumerical ordering of the lexemes
--
instance Ord Ident where
  compare (Ident s h _) (Ident s' h' _) = compare (h, s) (h', s')

-- for displaying identifiers
--
instance Show Ident where
  showsPrec _ ide = showString ("`" ++ identToString ide ++ "'")

-- identifiers are attributed
--
instance CNode Ident where
  nodeInfo (Ident _ _ at) = at
instance Pos Ident where
  posOf = posOfNode . nodeInfo
-- to speed up the equality test we compute some hash-like value for each
-- identifiers lexeme and store it in the identifiers representation

-- hash function from the dragon book pp437; assumes 7 bit characters and needs
-- the (nearly) full range of values guaranteed for `Int' by the Haskell 
-- language definition; can handle 8 bit characters provided we have 29 bit 
-- for the `Int's without sign
--
quad                 :: String -> Int
quad (c1:c2:c3:c4:s)  = ((ord c4 * bits21
                          + ord c3 * bits14 
                          + ord c2 * bits7
                          + ord c1) 
                         `mod` bits28)
                        + (quad s `mod` bits28)
quad (c1:c2:c3:[]  )  = ord c3 * bits14 + ord c2 * bits7 + ord c1
quad (c1:c2:[]     )  = ord c2 * bits7 + ord c1
quad (c1:[]        )  = ord c1
quad ([]           )  = 0

bits7 :: Int
bits7  = 2^(7::Int)
bits14 :: Int
bits14 = 2^(14::Int)
bits21 :: Int
bits21 = 2^(21::Int)
bits28 :: Int
bits28 = 2^(28::Int)

-- given the lexeme of an identifier, yield the abstract identifier (EXPORTED)
--
-- * the only attribute of the resulting identifier is its source text
--   position; as provided in the first argument of this function
--
-- * only minimal error checking, e.g., the characters of the identifier are
--   not checked for being alphanumerical only; the correct lexis of the
--   identifier should be ensured by the caller, e.g., the scanner.
--
-- * for reasons of simplicity the complete lexeme is hashed (with `quad')
--
mkIdent            :: Position -> String -> Name -> Ident
mkIdent pos s name  = Ident s (quad s) (mkNodeInfo pos name)

-- | generate an internal identifier (has internal position and no unique name)
internalIdent   :: String -> Ident
internalIdent s  = Ident s (quad s) (mkNodeInfoOnlyPos internalPos)

builtinIdent   :: String -> Ident
builtinIdent s  = Ident s (quad s) (mkNodeInfoOnlyPos builtinPos)

-- | return true if the given identifier is internal
isInternalIdent :: Ident -> Bool
isInternalIdent (Ident _ _ nodeinfo) = isInternalPos (posOfNode nodeinfo) || isBuiltinPos (posOfNode nodeinfo)

-- | get the string of an identifier
identToString               :: Ident -> String
identToString (Ident s _ _)  = s

-- | dump the lexeme and its positions into a string for debugging purposes
dumpIdent     :: Ident -> String
dumpIdent ide  = identToString ide ++ " at " ++ show (posOf ide) 
