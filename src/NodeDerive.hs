{-# LANGUAGE TemplateHaskell #-}

module NodeDerive (makeCNode) where
import Language.Haskell.TH.All
import Language.C.Data.Node
import Language.C.Data.Position
makeCNode :: Derivation
makeCNode = derivation genNodeInst "CNode"

nodeInfoTypeName :: [Char]
nodeInfoTypeName = "Language.C.Data.Node.NodeInfo"

genNodeInst :: DataDef -> [Dec]
genNodeInst dat = [
    instance_context ["CNode"] "CNode" dat
      [ FunD (mkName "nodeInfo") (nodeInfoDefs dat) ],
    instance_context ["CNode"] "Pos" dat
      [ FunD (mkName "posOf") (posOfDef) ]
    ]

posOfDef :: [Clause]
posOfDef = [Clause [VarP (mkName "x")]
                   (NormalB$ AppE (VarE$ mkName "posOfNode")
                                  (AppE (VarE$ mkName "nodeInfo") (VarE$ mkName "x")))
                   []]
matchIndex :: (Eq a) => CtorDef -> [(a, t)] -> a -> Pat -> Pat
matchIndex ctor ctorArgs ix pat = ConP (mkName $ ctorName ctor) $ map matchArg ctorArgs
  where
    matchArg (ix',_) | ix == ix' = pat
                     | otherwise = WildP

-- If we have a data constructor
--   X a_1 .. a_n, and execatly one a_k is a Language.C.Data.NodeInfo, then return that a_k
-- Else If we have a data constructor 
--   X a, then return nodeInfo a
-- Else If we have a data constructor
--   X a_1 .. a_n, and exactly one a_k is a polymorphic variable, then return (nodeInfo a_k)
-- Else Fail
nodeInfoDefs :: DataDef -> [Clause]
nodeInfoDefs dat = map nodeInfoImpl (dataCtors dat) where
    nodeInfoImpl ctor =
        case matchNodeInfo ctor of
            Right (pat,expr) ->
                Clause [pat] (NormalB expr) []
            Left err ->
                error $ "Failed to derive NodeInfo for " ++ ctorName ctor ++ ": " ++ err

matchNodeInfo :: CtorDef -> Either [Char] (Pat, Exp)
matchNodeInfo ctor = tryNodeInfoArg
  where
    strictTypes = ctorStrictTypes ctor
    ctorArgs = zip [(1::Integer)..] strictTypes

    tryNodeInfoArg =
        case filter (isNodeInfo.snd.snd) ctorArgs  of
            []       -> tryDelegate
            [(ix,_)] -> Right (matchIndex ctor ctorArgs ix (VarP varName), VarE varName)
            _        -> Left "more than one NodeInfo type"
        where
            isNodeInfo (ConT name) | show name == nodeInfoTypeName = True
                                   | otherwise = False
            isNodeInfo _ = False
            varName = mkName "nodeinfo"
    tryDelegate =
       case strictTypes of
           []        -> Left "cannot derive NodeInfo for nullary constructor"
           [_c]      -> Right (ConP (mkName $ ctorName ctor) [VarP (mkName "d")],
                               AppE (VarE (mkName "nodeInfo")) (VarE (mkName "d")))
           _xs       -> tryDependentInstance

    tryDependentInstance =
        case filter (isVarName . snd . snd) ctorArgs of
           []        -> Left $ "constructor has arity > 1, but neither type variable argument nor " ++
                               " argument of type NodeInfo"
           [(ix,_)]  -> Right (matchIndex ctor ctorArgs ix (VarP varName), delegateExpr)
           _xs       -> Left $ "constructor has arity > 1, but no argument of type NodeInfo, and more " ++
                               " than one type variable: "
        where
           isVarName (VarT _) = True
           isVarName (_) = False
           varName = mkName "t"
           delegateExpr = AppE (VarE (mkName "nodeInfo")) (VarE varName)
