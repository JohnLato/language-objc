{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Data.Name
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- Unique Names with fast equality (a.k.a. newtype Int)
--
module Language.C.Data.Name (
Name(..),namesStartingFrom,
NameMap,emptyNameMap,
) where
import Data.Ix
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Generics

-- | Name is a unique identifier
newtype Name = Name { nameId :: Int } deriving (Show, Read, Eq, Ord, Ix, Data, Typeable)

instance Enum Name where
    toEnum = Name
    fromEnum (Name n) = n
namesStartingFrom :: Int -> [Name]
namesStartingFrom k = [Name k..]

-- | NameMap is currently just an alias for IntMap, but that might change in the future
type NameMap = IntMap
emptyNameMap :: NameMap v
emptyNameMap = IntMap.empty