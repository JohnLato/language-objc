module Language.C.Syntax.Utils (
  -- * Generic operations
  getSubStmts,
  mapSubStmts,
  -- * Concrete operations
  getLabels
) where

import Data.List
import Language.C.Data.Ident
import Language.C.Syntax.AST

-- XXX: This is should be generalized !!!
--      Data.Generics sounds attractive, but we really need to control the evaluation order
-- XXX: Expression statements (which are somewhat problematic anyway), aren't handled yet
getSubStmts :: CStat -> [CStat]
getSubStmts (CLabel _ s _ _)      = [s]
getSubStmts (CCase _ s _)         = [s]
getSubStmts (CCases _ _ s _)      = [s]
getSubStmts (CDefault s _)        = [s]
getSubStmts (CExpr _ _)           = []
getSubStmts (CCompound _ body _)  = concatMap compoundSubStmts body
getSubStmts (CIf _ sthen selse _) = maybe [sthen] (\s -> [sthen,s]) selse
getSubStmts (CSwitch _ s _)       = [s]
getSubStmts (CWhile _ s _ _)      = [s]
getSubStmts (CFor _ _ _ s _)      = [s]
getSubStmts (CGoto _ _)           = []
getSubStmts (CGotoPtr _ _)        = []
getSubStmts (CCont _)             = []
getSubStmts (CBreak _)            = []
getSubStmts (CReturn _ _)         = []
getSubStmts (CAsm _ _)            = []

mapSubStmts :: (CStat -> CStat) -> CStat -> CStat
mapSubStmts f (CLabel i s attrs ni)  = f (CLabel i (mapSubStmts f s) attrs ni)
mapSubStmts f (CCase e s ni)         = f (CCase e (mapSubStmts f s) ni)
mapSubStmts f (CCases e1 e2 s ni)    = f (CCases e1 e2 (mapSubStmts f s) ni)
mapSubStmts f (CDefault s ni)        = f (CDefault (mapSubStmts f s) ni)
mapSubStmts f (CCompound ls body ni) =
  f (CCompound ls (map (mapBlockItemStmts f) body) ni)
mapSubStmts f (CIf e sthen selse ni) =
  f (CIf e
     (mapSubStmts f sthen)
     (maybe Nothing (Just . mapSubStmts f) selse)
     ni)
mapSubStmts f (CSwitch e s ni)       = f (CSwitch e (mapSubStmts f s) ni)
mapSubStmts f (CWhile e s isdo ni)   = f (CWhile e (mapSubStmts f s) isdo ni)
mapSubStmts f (CFor i t a s ni)      = f (CFor i t a (mapSubStmts f s) ni)
mapSubStmts f s                      = f s

mapBlockItemStmts :: (CStat -> CStat) -> CBlockItem -> CBlockItem
mapBlockItemStmts f (CBlockStmt s) = CBlockStmt (mapSubStmts f s)
mapBlockItemStmts _ bi             = bi

compoundSubStmts :: CBlockItem -> [CStat]
compoundSubStmts (CBlockStmt s)    = [s]
compoundSubStmts (CBlockDecl _)    = []
compoundSubStmts (CNestedFunDef _) = []

getLabels :: CStat -> [Ident]
getLabels (CLabel l s _ _)      = l : getLabels s
getLabels (CCompound ls body _) =
  concatMap (concatMap getLabels . compoundSubStmts) body \\ ls
getLabels stmt                  = concatMap getLabels (getSubStmts stmt)
