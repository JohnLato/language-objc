{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Parser
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  : benedikt.huber@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Language.C parser
-----------------------------------------------------------------------------
module Language.C.Parser (
    parseC,
    ParseError(..)
)
where
import Language.C.Parser.Parser (parseC, ParseError(..))
