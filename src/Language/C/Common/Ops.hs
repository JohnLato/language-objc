{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Common.Ops
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- unary, binary and asssignment operators
-----------------------------------------------------------------------------
module Language.C.Common.Ops (
CAssignOp(..),CBinaryOp(..),CUnaryOp(..)
)
where
import Data.Generics
-- | C assignment operators (K&R A7.17)
data CAssignOp = CAssignOp
               | CMulAssOp
               | CDivAssOp
               | CRmdAssOp              -- ^ remainder and assignment
               | CAddAssOp
               | CSubAssOp
               | CShlAssOp
               | CShrAssOp
               | CAndAssOp
               | CXorAssOp
               | COrAssOp
               deriving (Eq, Ord,Data,Typeable)

-- | C binary operators (K&R A7.6-15)
--
data CBinaryOp = CMulOp
               | CDivOp
               | CRmdOp                 -- ^ remainder of division
               | CAddOp
               | CSubOp
               | CShlOp                 -- ^ shift left
               | CShrOp                 -- ^ shift right
               | CLeOp                  -- ^ less
               | CGrOp                  -- ^ greater
               | CLeqOp                 -- ^ less or equal
               | CGeqOp                 -- ^ greater or equal
               | CEqOp                  -- ^ equal
               | CNeqOp                 -- ^ not equal
               | CAndOp                 -- ^ bitwise and
               | CXorOp                 -- ^ exclusive bitwise or
               | COrOp                  -- ^ inclusive bitwise or
               | CLndOp                 -- ^ logical and
               | CLorOp                 -- ^ logical or
               deriving (Eq,Ord,Data,Typeable)

-- | C unary operator (K&R A7.3-4)
--
data CUnaryOp = CPreIncOp               -- ^ prefix increment operator
              | CPreDecOp               -- ^ prefix decrement operator
              | CPostIncOp              -- ^ postfix increment operator
              | CPostDecOp              -- ^ postfix decrement operator
              | CAdrOp                  -- ^ address operator
              | CIndOp                  -- ^ indirection operator
              | CPlusOp                 -- ^ prefix plus
              | CMinOp                  -- ^ prefix minus
              | CCompOp                 -- ^ one's complement
              | CNegOp                  -- ^ logical negation
              deriving (Eq,Ord,Data,Typeable)
