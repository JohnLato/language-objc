{-# LANGUAGE TemplateHaskell #-}

module NodeDerive (makeCNode) where
import Language.Haskell.TH.All
import Language.C.Syntax.Node
import Language.C.Syntax.Position
makeCNode :: Derivation
makeCNode = derivation genNodeInst "CNode"
nodeInfoTypeName = "Language.C.Syntax.Node.NodeInfo"
genNodeInst :: DataDef -> [Dec]
genNodeInst dat = [
    instance_context ["CNode"] "CNode" dat 
      [ FunD (mkName "nodeInfo") (nodeInfoDefs dat) ],
    instance_context [] "Pos" dat
      [ FunD (mkName "posOf") (posOfDef) ]
    ]
posOfDef :: [Clause]
posOfDef = [Clause [VarP (mkName "x")] 
                   (NormalB$ AppE (VarE$ mkName "posOfNode") (AppE (VarE$ mkName "nodeInfo") (VarE$ mkName "x")))  
                   []]
    
-- If we have a data constructor
-- X a_1 .. a_n, and execatly one a_k is a Language.C.Syntax.NodeInfo, then return that a_k
-- If we have a data constructor
-- X a, then return nodeInfo a
-- otherwise fail Dec
nodeInfoDefs :: DataDef -> [Clause]
nodeInfoDefs dat = map nodeInfoImpl (dataCtors dat) where
    nodeInfoImpl ctor = 
        case matchNodeInfo ctor of
            Right (pat,expr) -> 
                Clause [pat] (NormalB expr) []
            Left err ->
                error $ "Failed to derive NodeInfo for " ++ ctorName ctor ++ ": " ++ err
    matchNodeInfo ctor = 
        case filter (isNodeInfo.snd.snd) ctorArgs  of
            []       -> tryDelegate ctor
            [(ix,_)] -> Right (mkNodeInfoPat ix, VarE (mkName "nodeinfo"))
            _        -> Left "more than one NodeInfo type"
        where 
            ctorArgs = zip [(1::Integer)..] (ctorStrictTypes ctor)
            isNodeInfo (ConT name) | show name == nodeInfoTypeName = True
                                   | otherwise = False
            isNodeInfo _ = False
            mkNodeInfoPat ix = ConP (mkName $ ctorName ctor) $ map (matchArg ix) ctorArgs
            matchArg ix (ix',_) | ix == ix' = VarP (mkName "nodeinfo")
                                | otherwise = WildP
    tryDelegate ctor =
       case ctorStrictTypes ctor of
           []        -> Left "cannot derive NodeInfo for nullary constructor"
           [c]       -> Right (ConP (mkName $ ctorName ctor) [VarP (mkName "d")],
                               AppE (VarE (mkName "nodeInfo")) (VarE (mkName "d")))
           _         -> Left "constructor has arity > 1, but no argument if of type NodeInfo"
