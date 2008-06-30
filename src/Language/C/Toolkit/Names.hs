-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Toolkit.Names
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- Unique Names with fast equality
--
module Language.C.Toolkit.Names (
Name(..),namesStartingFrom,NameMap,
) where
import Data.Ix
import Data.IntMap (IntMap)
-- | Name is a unique identifier
newtype Name = Name { nameId :: Int } deriving (Show, Eq, Ord, Ix)

instance Enum Name where
    toEnum = Name
    fromEnum (Name n) = n
namesStartingFrom :: Int -> [Name]
namesStartingFrom k = [Name k..]
-- | NameMap is currently just an alias for IntMap, but that might change in the future
type NameMap = IntMap