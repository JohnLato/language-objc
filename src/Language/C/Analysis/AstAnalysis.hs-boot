module Language.C.Analysis.AstAnalysis where

import Language.C.Analysis.SemRep
import Language.C.Analysis.TravMonad
import Language.C.Syntax.AST

data ExprSide = LValue | RValue

tExpr :: MonadTrav m => ExprSide -> CExpr -> m Type
