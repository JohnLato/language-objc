{-# LANGUAGE NoMonomorphismRestriction
            ,ScopedTypeVariables
            ,TypeFamilies
            ,ScopedTypeVariables
            ,RankNTypes #-}

-- | Generic operations for working with AST structures.
--
-- Due to limitations of SYB, many of these functions only work properly
-- on functions with NodeInfo annotations.  It is not always possible to
-- reflect this in type signatures.
module Language.ObjC.Syntax.Generics (
  getName
 ,getTypeName
 ,getIdent
 ,getSelector
 ,getMethodDecs
 ,getAttrs
 ,isProtoDecl
 ,updateDeclaratorName
 ,descendToFuncDec
 ,updateFuncDefName
 ,mapFirst
 ,mFirst
 ,gmapMaybe
)

where

import Language.ObjC.Syntax
import Language.ObjC.Data.Ident
import Language.ObjC.Data
import Data.Generics
import Data.Foldable (foldMap)
import Data.Maybe
import Data.Monoid
import Control.Newtype

-- get a name from an Ident, ClassName, or TypedefName
getName :: (Typeable a) => a -> Maybe String
getName = mkQ Nothing (\(Ident  str  _ _) -> Just str)
          `extQ` (\((ObjCClassNm idnt _) :: ObjCClassNm)   -> extI idnt)
          `extQ` (\((CTypeDef    idnt _) :: CTypeSpec)   -> extI idnt)
 where
  extI (Ident str _ _) = Just str

-- | if an item is a Type name, return it.
getTypeName :: (Data a) => a -> Maybe (CDeclaration NodeInfo)
getTypeName = mkQ Nothing f
  where
   f tn@(CDecl _ ((Just _,Nothing,Nothing):_) _) = Just tn
   f tn@(CDecl _ []                           _) = Just tn
   f _                                           = Nothing

getIdent :: Typeable a => a -> Maybe Ident
getIdent = mkQ Nothing (\(i :: Ident) -> Just i)

-- | If a value is a method selector, return it
getSelector :: (Typeable a) => a -> Maybe (ObjCMethodSel)
getSelector = mkQ Nothing (\(s :: ObjCMethodSel) -> Just s)

-- | A list of all method declarations found in a value
getMethodDecs :: (Data a) => a -> [ObjCMethodDecl]
getMethodDecs = everything (++) (mkQ [] (\(s :: ObjCMethodDecl) -> [s]))

-- | A list of all attributes in an object
getAttrs :: Data a => a -> [CAttr]
getAttrs = listify (\(_ :: CAttr) -> True)

-- | Update the name used in a CDeclr.
-- Be careful, if used as "everywhere updateDeclaratorName", function
-- pointer parameters could be updated also.
-- 
-- WARNING: this function only works on @AST NodeInfo@ types
updateDeclaratorName :: (Data a) => (String->String) -> a -> a
updateDeclaratorName f = mkT updater
 where
  updater :: CDeclr -> CDeclr
  updater (CDeclr (Just nm) drvs lits attrs a) =
      CDeclr (Just $ updateIdent f nm) drvs lits attrs a
  updater x = x

-- | Update the name of a CFunDef
updateFuncDefName
  :: forall a. Data a
  => (String -> String)
  -> CFunctionDef a
  -> CFunctionDef a
updateFuncDefName f = mkT f1
 where
  updater (CDeclr (Just nm) drvs lits attrs a) =
      CDeclr (Just $ updateIdent f nm) drvs lits attrs a
  updater x = x
  f1 :: CFunctionDef a -> CFunctionDef a
  f1 (CFunDef spec decl dlist stm a) =
      CFunDef spec (updater decl) dlist stm a
  f1 d = d

-- | Descend to the top-most function declarator(s) and update
-- the function name.
-- 
-- WARNING: this function only works on AST @NodeInfo@ types due to a
-- bug in ghc
descendToFuncDec :: (Data a) => (String -> String) -> a -> a
descendToFuncDec f = gmapT f'
 where
  t1 = typeOf (undefined :: CDeclr)
  f' :: Data x => x -> x
  f' x = let tx = typeOf x
         in if tx == t1
           then updateDeclaratorName f x -- stop descent here
           -- else descendToFuncDec f x
           else gmapT f' x               -- not a declarator, descend freely

-- | Rename an identifier
updateIdent :: Data a => (String -> String) -> a -> a
updateIdent f = mkT (\(Ident s x p) -> Ident (f s) x p)

-- | Check if a declaration is a Protocol_declaration
-- is a generic function so that it can be used within "wrapMethodDecs" and
-- other generics...
isProtoDecl :: (Data a) => a -> Bool
isProtoDecl = mkQ False $ (\(_ :: ObjCProtoDec) -> True)

gmapMaybe :: (Data a, Data b) => (a -> Maybe a) -> b -> b
gmapMaybe f = mkT (mapMaybe f)

mapFirst :: Data b => GenericQ (Maybe r) -> b -> Maybe r
mapFirst q b = listToMaybe . catMaybes $ gmapQ q b

mFirst :: Maybe a -> Maybe a -> Maybe a
mFirst l r = ala First foldMap [l,r]
