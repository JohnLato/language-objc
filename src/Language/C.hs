-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C
-- Copyright   :  (c) 2008 Benedikt Huber 
--                [1995..2007] 
--                   Manuel M. T. Chakravarty
--                   Duncan Coutts
--                   Betram Felgenhauer
-- License     :  BSD-style
-- Portability :  portable
-- Stability   :  provisional
--
-- The C99 parser and pretty printer library.
-----------------------------------------------------------------------------
module Language.C (
    module Language.C.Syntax,
    module Language.C.Pretty,
    module Language.C.Parser    
)
where
import Language.C.Syntax
import Language.C.Pretty
import Language.C.Parser    
