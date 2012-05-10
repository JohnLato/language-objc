module Language.ObjC.Analysis.AstAnalysis where

import Language.ObjC.Analysis.SemRep
import Language.ObjC.Analysis.TravMonad
import Language.ObjC.Syntax.AST

data StmtCtx = FunCtx VarDecl
             | LoopCtx
             | SwitchCtx

data ExprSide = LValue | RValue

tExpr :: MonadTrav m => [StmtCtx] -> ExprSide -> CExpr -> m Type
