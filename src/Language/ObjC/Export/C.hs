{-# LANGUAGE ScopedTypeVariables #-}

module Language.ObjC.Export.C (
  wrapMethodDecs
 ,wrapMethod
)

where

import Language.ObjC.Syntax
import Language.ObjC.Syntax.Generics
import Language.ObjC.Syntax.Builders

-- just using TravMonad for name generation and error handling,
-- not actually converting this code to SemRep because that
-- doesn't have full ObjC support yet.
import Language.ObjC.Analysis.TravMonad

import Language.ObjC.Data

import Data.Data
import Data.Generics
import Data.List (intercalate)
import Data.Maybe

import Control.Applicative

(****) :: (a -> b -> c) -> (d -> e -> f) -> (a,d) -> (b,e) -> (c,f)
f **** g = \(a,d) (b,e) -> (f a b, g d e)

-- | Wrap all method declarations for an interface, object, or protocol
wrapMethodDecs :: Data a => a -> Bool -> [Trav () (CDecl, CFunDef)]
wrapMethodDecs x discardDeprecated =
  case everything (mFirst **** (||)) (\a -> (getName a, isProtoDecl a)) x of
    (Just cStr,isP) -> map (withDecl cStr isP) $ getMethodDecs x
    (Nothing,_)     -> []
 where
  withDecl cStr isP d = do
    idNm <- genName
    let cName = mkIdent nopos cStr idNm
    wrapMethod isP discardDeprecated (ObjCClassNm cName nonode) d

-- | Wrap a Method declaration
wrapMethod
  :: Bool            -- ^ is a protocol declaration
  -> Bool            -- ^ Discard items with a deprecated attribute
  -> ObjCClassNm
  -> ObjCMethodDecl
  -> Trav () (CDecl, CFunDef)
wrapMethod isP discDep cname dec@(ObjCMethodDecl typ _ _ _ _) = 
  let checkAttr = discDep &&
                   (not . null $ listify (\((CAttr attr _ _) :: CAttr) ->
                                         show attr `elem`
                                          ["\"deprecated\"", "\"unavailable\""])
                                       dec)
      wrapFn = case typ of
                 ObjCInstanceMethod -> wrapInstMethod
                 ObjCClassMethod    -> wrapClassMethod
  in  if checkAttr
        then throwTravError $ userErr
               (show cname ++ " discarding method (deprecated)")
        else wrapFn isP cname dec

-- | Wrap a Class Method in a C function,
-- returning the function declaration and body
wrapClassMethod
  :: Bool
  -> ObjCClassNm
  -> ObjCMethodDecl
  -> Trav () (CDecl, CFunDef)
wrapClassMethod isP cName cm@(ObjCMethodDecl _ oType _ attrs _) = do
   sel <- maybe (throwTravError $ userErr
                    "Can't determine ObjCMethodSelector from input")
                 return $ mapFirst getSelector cm
   fname <- nameFromSel sel
   let gFunc ddeclr = genDecCon oType attrs (CDeclr (Just fname) [ddeclr]
                                Nothing [] nonode)
   (dec,def_con) <- gFunc <$> decBody sel
   body <- defBody (Left cName) sel
   return $ (dec, def_con body)

wrapInstMethod
  :: Bool
  -> ObjCClassNm
  -> ObjCMethodDecl
  -> Trav () (CDecl, CFunDef)
wrapInstMethod isP cName cm@(ObjCMethodDecl _ oType _ attrs _) = do
   sel <- maybe (throwTravError $ userErr "Can't determine ObjCMethodSelector from input")
                 return $ mapFirst getSelector cm
   fname <- nameFromSel sel
   idNm  <- genName
   let selfname = mkIdent nopos "thisObj" idNm
       gFunc ddeclr = genDecCon oType attrs (CDeclr (Just fname) [ddeclr]
                                Nothing [] nonode)
   (protoSel,msgSel) <- explicitSelf isP selfname cName sel

   (dec,def_con) <- gFunc <$> decBody protoSel
   body <- defBody (Right selfname) msgSel
   return $ (dec, def_con body)

genDecCon
  :: Maybe CDecl   -- ^ method output type, if specified
  -> [CAttr]
  -> CDeclr        -- ^ function declaratation (CDeclr nm [CFunDeclr ...)
  -> (CDecl, CStat -> CFunDef)
genDecCon oType attrs fundeclr = 
  let (thisSpec,declrs) = maybe ([idTypeSpec],[]) specFun oType
      specFun (CDecl specs declrs' _) = (stripProtoQuals specs,declrs')

      -- if the output is a pointer or block, grab the abstract
      -- declarator from the oType declarators
      cleanDeclrs = catMaybes $ map (\(a,_,_) -> a) declrs
      extraDeclrs = concatMap (\(CDeclr _ abs _ _ _) -> abs) cleanDeclrs
      fundeclr' = case fundeclr of
                    CDeclr nm ders sLit attrs at ->
                      CDeclr nm (ders ++ extraDeclrs) sLit attrs at
      attrSpec = thisSpec ++ map (CTypeQual . CAttrQual) attrs

  in  (CDecl   attrSpec [(Just fundeclr',Nothing,Nothing)] nonode
      ,\stmt -> CFunDef attrSpec fundeclr' [] stmt nonode)

decBody
  :: ObjCMethodSel
  -> Trav () CDerivedDeclr         -- CFunDeclr
decBody sel = do
  sels <- wrapMethodSelector sel
  return $ CFunDeclr (Right sels) [] nonode

defBody
  :: Either ObjCClassNm Ident
  -> ObjCMethodSel
  -> Trav () CStat
defBody nm sel = do
  fExpr <- msgMethod nm sel
  return $ CCompound [] [
    CBlockStmt (CReturn (Just $ ObjCMessageExpr fExpr nonode) nonode)] nonode

-- | create the name for the wrapping function of a method_selector
nameFromSel :: ObjCMethodSel -> Trav () Ident
nameFromSel sel = do
   nm' <- genName
   return $ flip (mkIdent nopos) nm' . intercalate "_"
     . catMaybes . map (everything mFirst getName) $ listify p sel
 where
  p :: ObjCSel -> Bool
  p (ObjCSel{}) = True
  p _           = False

wrapMethodSelector
  :: ObjCMethodSel
  -> Trav () ([CDecl],Bool)  -- (decls, isVariadic)
wrapMethodSelector (ObjCUnaryMethod _s _)     = return $ ([],False)
wrapMethodSelector (ObjCMethod kds Nothing _) = return $
  (map keydecl2param kds, False)
wrapMethodSelector _ =
  throwTravError $ userErr "wrapMethodSelector: can't deal with Ellipses or parameter types yet"

-- | add an explicit self parameter to a method_selector.
-- useful for wrapping instance methods
-- in the returned tuple, the left ObjCMethodSel is for creating
-- the function prototype, the right ObjCMethodSel is for the
-- function body.
explicitSelf
  :: Bool
  -> Ident
  -> ObjCClassNm
  -> ObjCMethodSel
  -> Trav () (ObjCMethodSel, ObjCMethodSel)
explicitSelf isP selfname (ObjCClassNm cn _) ms = case ms of
  u@(ObjCUnaryMethod _sel      _) ->
    return $ (ObjCMethod [mkKd] Nothing nonode, u)
  (ObjCMethod keydecls ps _) ->
    let ms' = ObjCMethod (mkKd:keydecls) ps nonode
    in return (ms',ms)
  _ -> throwTravError $ userErr "explicitSelf: can't deal with ellipses or parameter types yet"
 where
  tyspec = CTypeSpec $ ObjCClassProto cn [] nonode
  ddeclr = CDeclr Nothing [CPtrDeclr [] nonode] Nothing [] nonode
  basicT = Just $ CDecl [tyspec]
             [(Just ddeclr, Nothing, Nothing)] nonode
  ptrT   = Just $ protoType cn
  mkKd = ObjCKeyDeclr Nothing (if isP then ptrT else basicT) selfname nonode

msgMethod
  :: Either ObjCClassNm Ident
  -> ObjCMethodSel
  -> Trav () ObjCMsgExpr
msgMethod (Left cn) (ObjCUnaryMethod sel _) =
  return $ ObjCMsgClass cn (ObjCMsgSel sel nonode) nonode
msgMethod (Right i) (ObjCUnaryMethod sel _) =
  return $ ObjCMsgExpr (CVar i nonode) (ObjCMsgSel sel nonode) nonode

msgMethod (Left cn) (ObjCMethod decs Nothing _) =
  return $ ObjCMsgClass cn (ObjCKeyArgs (map keydecl2arg decs) nonode) nonode
msgMethod (Right i) (ObjCMethod decs Nothing _) =
  return $ ObjCMsgExpr (CVar i nonode) (ObjCKeyArgs (map keydecl2arg decs) nonode) nonode

msgMethod _ _ =
   throwTravError $ userErr "mesgMethod: can't deal with ellipses or parameter types"

-- | Generate parameter_declarations
-- from obj-c keyword declarators
keydecl2param :: ObjCKeyDeclr -> CDecl
keydecl2param (ObjCKeyDeclr _sel mT declr _) =
  typeName2Paramdecl declr $ fromMaybe idType mT

-- | convert keyword_declarators into keyword_arguments
keydecl2arg :: ObjCKeyDeclr -> ObjCKeyArg
keydecl2arg (ObjCKeyDeclr sel  _ nm _) =
  ObjCKeyArg (ObjCSelKeyName sel nonode) (CVar nm nonode) nonode

typeName2Paramdecl :: Ident -> CDecl -> CDecl
typeName2Paramdecl i (CDecl tyspec declrs a1) =
  CDecl tyspec [(Just $ CDeclr (Just i) ddeclrs Nothing [] nonode
                ,Nothing
                ,Nothing)] a1
 where
  ddeclrs = listify (\(_ :: CDerivedDeclr) -> True) declrs

stripProtoQuals :: Data a => a -> a
stripProtoQuals = everywhere (gmapMaybe f)
 where
  f :: CDeclSpec -> Maybe CDeclSpec
  f (CTypeQual (ObjCProtoQual _)) = Nothing
  f tq = Just tq
