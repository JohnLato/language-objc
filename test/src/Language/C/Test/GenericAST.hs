-----------------------------------------------------------------------------
-- |
-- Module      :  GenericAST.hs
-- Copyright   :  (c) Benedikt Huber Wed May 28 09:18:07 CEST 2008
--
-- Generic ASTs
-- FIXME: Prototype !!!
-- Note that it seems to be clever to use generics here, as long as the AST
-- isn't set in stone - no need to update when the AST changes.
--
-- With GHC as of 6.8.2, you sometimes get error messages like this one when compiling:
-- 
-- > The interface for `main:Language.C.AST.Generic'
-- > Declaration for toConstr22
-- > Unfolding of Language.C.AST.Generic.toConstr22:
-- >   Can't find interface-file declaration for variable Language.C.AST.AST.$cCHeader
-- >   Probable cause: bug in .hi-boot file, or inconsistent .hi file
-- >   Use -ddump-if-trace to get an idea of which file caused the error
--
-- In this case, remove all object and interface files, and recompile.
-----------------------------------------------------------------------------
module Language.C.Test.GenericAST where
import Data.Generics
import Text.PrettyPrint

import Language.C.Toolkit.Attributes
import Language.C.Toolkit.Idents
import Language.C.AST.AST
import Language.C.AST.Pretty
import Language.C.AST.Generic ()
-- | Generic AST
data GenAST =   GNode Constr [GenAST]
              | GNested [GenAST]
              | GLeaf GenLeaf 
              | GIgnore 
              deriving (Show,Eq)
instance Pretty GenAST where
  pretty (GNode constr sub) = 
    text (show constr) $$ nest 2 (vcat $ map pretty sub)
  pretty (GNested sub) =
    text "-" $$ (nest 1 $ (vcat $ map pretty sub))
  pretty (GLeaf l) = text (show l)
  pretty GIgnore = text ""
  
data GenLeaf = GIdent Ident |
               GCharLit  Char |
               GStringLit String |
               GIntConst Integer |
               GDoubleConst Double
              deriving (Show,Eq,Ord)
-- | Convert C AST into generic AST
mkGenericCAST :: CHeader -> GenAST
mkGenericCAST = toGenericAST

-- To build a generic ast, we proceed as follows:
-- If we have a primitive (Ident,Char,String,Integer or Double), we create a generic leaf.
-- If we have a container (Maybe / List), we would like to create a nested node 
--   FIXME: requires SYB with class
-- If we have an Attr, we ignore the datum.
-- If we have an AST Constructor, we get the constructors arguments, 
--   make a list of generic asts and then build the generic ast's node

-- mkQ : (Typeable a, Typeable b) => r -> (b -> r) -> a -> r 
-- extQ: (Typeable a, Typeable b) => (a -> q) -> (b -> q) -> a -> q 
toGenericAST :: (Data a) => a -> GenAST
toGenericAST = 
         mkAstConNode
  `extQ` mkAstAttr
  `extQ` mkLeaf GIdent
  `extQ` mkLeaf GCharLit
  `extQ` mkLeaf GStringLit
  `extQ` mkLeaf GIntConst
  `extQ` mkLeaf GDoubleConst
  where    
    mkAstConNode :: (Data a) => a -> GenAST
    mkAstConNode v = GNode (toConstr v) . map simplifyNode . filter ( /= GIgnore)  $ gmapQ toGenericAST v
    mkAstAttr :: Attrs -> GenAST
    mkAstAttr _ = GIgnore
    mkLeaf :: (a -> GenLeaf) -> (a -> GenAST)
    mkLeaf = (GLeaf .)
    -- bad hack !!! (to do it RIGHT, needs SYB with class)
    simplifyNode (GNode constr [])      | (show constr) == "[]" = GNested []
    simplifyNode (GNode constr [hd,GNested tl]) | (show constr) == "(:)" = GNested (hd:tl)
    simplifyNode (GNode constr [a,b])   | (show constr) == "(,)" = GNested [a,b]
    simplifyNode (GNode constr [a,b,c]) | (show constr) == "(,,)" = GNested [a,b,c]
    simplifyNode (GNode constr [])      | (show constr) == "Nothing" = GNested []    
    simplifyNode (GNode constr [a])     | (show constr) == "Just" = GNested [a]    
    simplifyNode node = node
    -- I think for this one we need SYB 3 (with class)
    --mkAstMaybe :: (Data a) => (Maybe a) -> GenAST
    --mkAstMaybe = maybe (Nested []) (Nested . return . toGenericAST)
    --mkAstList  :: (Data a) => [a] -> GenAST
    --mkAstList  = Nested . map toGenericAST