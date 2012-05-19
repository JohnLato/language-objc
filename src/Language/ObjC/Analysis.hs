-----------------------------------------------------------------------------
-- |
-- Module      :  Language.ObjC.Analysis
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  alpha
-- Portability :  ghc
--
-- Analysis of the AST.
--
-- Currently, we provide a monad for analysis and analyze declarations and types.
-- Especially note that there is no direct support for analyzing function bodies and
-- constant expressions.
--
-- /NOTE/ This is an experimental interface, and therefore the API will change in the
-- future.
--
-- DONE:
--
--  * Name analysis framework
--
--  * File-scope analysis
--
--  * Declaration analysis
--
-- TODO:
--
--  * Type checking expressions
--
--  * Constant expression evaluation (CEE)
--
--  * Typed representation of attributes (depends on CEE)
--
--  * Normalized representation of initializers
--
--  * Support for analyzing function bodies (depends on CEE)
--
--  * Normalizing expressions and statements
--
--  * Formal rules how to link back to the AST using NodeInfo fields
--
--  * Typed assembler representation

-----------------------------------------------------------------------------
module Language.ObjC.Analysis (
    -- * Semantic representation
    module Language.ObjC.Analysis.SemRep,
    -- * Error datatypes for the analysis
    module Language.ObjC.Analysis.SemError,
    -- * Traversal monad
    module Language.ObjC.Analysis.TravMonad,
    -- * Top level analysis
    module Language.ObjC.Analysis.AstAnalysis,
    -- * Analyzing declarations
    module Language.ObjC.Analysis.DeclAnalysis,
    -- * Debug print
    module Language.ObjC.Analysis.Debug,
)
where
import Language.ObjC.Analysis.SemError
import Language.ObjC.Analysis.SemRep

import Language.ObjC.Analysis.TravMonad

import Language.ObjC.Analysis.AstAnalysis
import Language.ObjC.Analysis.DeclAnalysis

import Language.ObjC.Analysis.Debug
