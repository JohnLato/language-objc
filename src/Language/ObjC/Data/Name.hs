{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.ObjC.Data.Name
-- Copyright   :  (c) 2008 Benedikt Huber
--                (c) 2012 John W. Lato
-- License     :  BSD-style
-- Maintainer  :  jwlato@gmail.com
-- Stability   :  experimental
-- Portability :  ghc
--
-- Unique Names with fast equality (newtype 'Int')
module Language.ObjC.Data.Name (
Name(..),newNameSupply, namesStartingFrom
) where
import Data.Ix
import Data.Generics

-- | Name is a unique identifier
newtype Name = Name { nameId :: Int } deriving (Show, Read, Eq, Ord, Ix, Data, Typeable)

instance Enum Name where
    toEnum = Name
    fromEnum (Name n) = n

-- | return an infinite stream of 'Name's starting with @nameId@ 0
newNameSupply :: [Name]
newNameSupply = namesStartingFrom 0

-- | get the infinite stream of unique names starting from the given integer
namesStartingFrom :: Int -> [Name]
namesStartingFrom k = [Name k..]
