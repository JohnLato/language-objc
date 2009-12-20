{-# LANGUAGE DeriveDataTypeable #-}

module DeriveTest where
import Language.C.Data.Node
import Language.C.Data.Position
import Data.Data
data ExplicitNodeInfo = ExplicitNodeInfo1 NodeInfo Int
                      | ExplicitNodeInfo2 Int NodeInfo
                      | ExplicitNodeInfo3 Int NodeInfo Int
                   deriving (Data,Typeable {-! CNode !-})
data OneArgNodeInfo = ExplicitNodeInfo4 Int NodeInfo
                    | Delegator ExplicitNodeInfo
                   deriving (Data,Typeable {-! CNode !-})
data PolyVarNodeInfo a = PolyCon2 Int Int a
                       | PolyCon1 Int a
                       | PolyCon0 a
                       | PolyDelegator OneArgNodeInfo
                   deriving (Data,Typeable {-! CNode !-})

-- -- Should fail
-- data PolyVarNodeInfo a b = PolyCon2 Int Int a
--                         | PolyCon1 a b
--                   deriving (Data,Typeable {-! CNode !-})
--



--------------------------------------------------------
-- DERIVES GENERATED CODE
-- DO NOT MODIFY BELOW THIS LINE
-- CHECKSUM: 629656574

instance CNode ExplicitNodeInfo
    where nodeInfo (ExplicitNodeInfo1 nodeinfo _) = nodeinfo
          nodeInfo (ExplicitNodeInfo2 _ nodeinfo) = nodeinfo
          nodeInfo (ExplicitNodeInfo3 _ nodeinfo _) = nodeinfo
instance Pos ExplicitNodeInfo
    where posOf x = posOfNode (nodeInfo x)

instance CNode OneArgNodeInfo
    where nodeInfo (ExplicitNodeInfo4 _ nodeinfo) = nodeinfo
          nodeInfo (Delegator d) = nodeInfo d
instance Pos OneArgNodeInfo
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (PolyVarNodeInfo t1)
    where nodeInfo (PolyCon2 _ _ t) = nodeInfo t
          nodeInfo (PolyCon1 _ t) = nodeInfo t
          nodeInfo (PolyCon0 d) = nodeInfo d
          nodeInfo (PolyDelegator d) = nodeInfo d
instance CNode t1 => Pos (PolyVarNodeInfo t1)
    where posOf x = posOfNode (nodeInfo x)
