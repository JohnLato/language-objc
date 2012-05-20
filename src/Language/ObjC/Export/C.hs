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
import Data.Generics.Schemes
import Data.List (intercalate)
import Data.Maybe

import Control.Applicative

(****) :: (a -> b -> c) -> (d -> e -> f) -> (a,d) -> (b,e) -> (c,f)
f **** g = \(a,d) (b,e) -> (f a b, g d e)

-- | Wrap all method declarations for an interface, object, or protocol
wrapMethodDecs :: Data a => a -> [Trav () (CDecl, CFunDef)]
wrapMethodDecs x =
  case everything (mFirst **** (||)) (\a -> (getName a, isProtoDecl a)) x of
    (Just cStr,isP) -> map (withDecl cStr isP) $ getMethodDecs x
    (Nothing,_)     -> []
 where
  withDecl cStr isP d = do
    idNm <- genName
    let cName = mkIdent nopos cStr idNm
    wrapMethod isP (ObjCClassNm cName nonode) d

-- | Wrap a Method declaration
wrapMethod
  :: Bool            -- ^ is a protocol declaration
  -> ObjCClassNm
  -> ObjCMethodDecl
  -> Trav () (CDecl, CFunDef)
wrapMethod isP cname dec@(ObjCMethodDecl ObjCClassMethod _ _ _ _) =
  wrapClassMethod isP cname dec
wrapMethod isP cname dec@(ObjCMethodDecl ObjCInstanceMethod _ _ _ _) =
  wrapInstMethod  isP cname dec

-- | Wrap a Class Method in a C function,
-- returning the function declaration and body
wrapClassMethod
  :: Bool
  -> ObjCClassNm
  -> ObjCMethodDecl
  -> Trav () (CDecl, CFunDef)
wrapClassMethod isP cName cm = do
   let oType        = mapFirst getTypeName cm
   sel <- maybe (throwTravError $ userErr "Can't determine ObjCMethodSelector from input")
                 return $ mapFirst getSelector cm
   fname <- nameFromSel "" sel
   let gFunc ddeclr = genDecCon oType (CDeclr (Just fname) [ddeclr]
                                Nothing [] nonode)
   (dec,def_con) <- gFunc <$> decBody sel
   body <- defBody (Left cName) sel
   return $ (dec, def_con body)

wrapInstMethod
  :: Bool
  -> ObjCClassNm
  -> ObjCMethodDecl
  -> Trav () (CDecl, CFunDef)
wrapInstMethod isP cName cm = do
   let oType    = mapFirst getTypeName cm
   sel <- maybe (throwTravError $ userErr "Can't determine ObjCMethodSelector from input")
                 return $ mapFirst getSelector cm
   fname <- nameFromSel "" sel
   idNm  <- genName
   let selfname = mkIdent nopos "thisObj" idNm
       gFunc ddeclr = genDecCon oType (CDeclr (Just fname) [ddeclr]
                                Nothing [] nonode)
   (protoSel,msgSel) <- explicitSelf isP selfname cName sel

   (dec,def_con) <- gFunc <$> decBody protoSel
   body <- defBody (Right selfname) msgSel
   return $ (dec, def_con body)

genDecCon
  :: Maybe CDecl   -- ^ method output type, if specified
  -> CDeclr        -- ^ function declaratation (CDeclr nm [CFunDef ...)
  -> (CDecl, CStat -> CFunDef)
genDecCon oType fundecl = 
  let thisSpec = maybe [idType] specFun oType
      specFun (CDecl specs _decls _) = stripProtoQuals specs
      specFun _ = error "genDecCon: didn't get a type name"

  -- is the _decls ever used?  maybe for pointers and things
  in  (CDecl   thisSpec [(Just fundecl,Nothing,Nothing)] nonode
      ,\stmt -> CFunDef thisSpec fundecl [] stmt nonode)

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
nameFromSel :: String -> ObjCMethodSel -> Trav () Ident
nameFromSel pre sel = do
   nm' <- genName
   return $ flip (mkIdent nopos) nm' . (pre ++) . intercalate "_"
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
  basicT = Just $ CDecl [tyspec] [] nonode
  ptrT   = Just $ CDecl [tyspec] [(Just (error "ptrT in explicitSelf"),Nothing,Nothing)] nonode
  mkKd = ObjCKeyDeclr Nothing
                          (if isP then basicT else ptrT)
                          selfname
                          nonode

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

-- | Generate parameter_declarations (for new-style functions)
-- from obj-c keyword declarators
keydecl2param :: ObjCKeyDeclr -> CDecl
keydecl2param p = case (mapFirst getIdent p, mapFirst getTypeName p) of
  (Just i, Just t)  -> typeName2Paramdecl i t
  (Just i, Nothing) -> typeName2Paramdecl i $ CDecl [idType] [] nonode
  _          -> error "internal error: keydecl2param"

-- | convert keyword_declarators into keyword_arguments
keydecl2arg :: ObjCKeyDeclr -> ObjCKeyArg
keydecl2arg (ObjCKeyDeclr sel  _ nm _) =
  ObjCKeyArg (ObjCSelKeyName sel nonode) (CVar nm nonode) nonode

typeName2Paramdecl :: Ident -> CDecl -> CDecl
typeName2Paramdecl i t = t
{-
-- | Create a Parameter_declaration from an identifier and a Type_name
typeName2Paramdecl :: Ident -> Type_name -> Parameter_declaration
typeName2Paramdecl i (PlainType s) =
  TypeAndParam (specQuals2Declspec s) $ NoPointer (Name i)
typeName2Paramdecl i (ExtendedType s ab) =
  TypeAndParam (specQuals2Declspec s) . abs2decl ab $ Name i
-}

stripProtoQuals :: Data a => a -> a
stripProtoQuals = everywhere (gmapMaybe f)
 where
  f :: CTypeQual -> Maybe CTypeQual
  f (ObjCProtoQual{}) = Nothing
  f tq = Just tq
