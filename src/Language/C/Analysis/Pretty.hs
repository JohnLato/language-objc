{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis.Pretty
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  internal use
-- Portability :  unspecified
--
-- Pretty printing the semantic analysis representation, using Data.Generics.
-- We now use overlapping instances, so this is rather unportable prototype code
-----------------------------------------------------------------------------
module Language.C.Analysis.Pretty (
globalDeclStats,
prettyAssocsWith,
)
where
import Language.C.Analysis.SemRep
import Language.C.Pretty
import Language.C.Syntax hiding (cstringOfLit)
import Data.Generics
import Text.PrettyPrint.HughesPJ
import Data.Map (Map)
import qualified Data.Map as Map


instance Pretty GlobalDecls where
    pretty gd = text "Global Declarations" $$ (nest 4 $ vcat declMaps)
        where
        declMaps = [ prettyDecls "declarations" (Map.assocs $ gDecls gd),
                     prettyMap "objects" $ gObjs gd,  prettyMap "functions" $ gFuns gd, 
                     prettyMap "tags"    $ gTags gd,  prettyMap "typedefs"  $ gTypedefs gd ]
        prettyMap :: (Pretty t, Pretty k) => String -> Map k t -> Doc
        prettyMap label = prettyAssocsWith label pretty pretty . Map.assocs
        prettyDecls label = prettyAssocsWith label pretty $ \(decl,isFunction) ->
                                text (if isFunction then "function: " else "object: ") <+> pretty decl
prettyAssocsWith :: String -> (k -> Doc) -> (v -> Doc) -> [(k,v)] -> Doc
prettyAssocsWith label prettyKey prettyVal theMap =
    text label $$ (nest 8) (vcat $ map prettyEntry theMap)
    where
    prettyEntry (k,v) = prettyKey k <+> text " ~> " <+> prettyVal v

globalDeclStats :: (FilePath -> Bool) -> GlobalDecls -> [(String,Int)]
globalDeclStats file_filter gmap =
    [ ("Object Declarations",Map.size objDecls),
      ("Object Definitions", Map.size objDefs),
      ("Function Declarations", Map.size funDecls),
      ("Function Definitions", Map.size funDefs),
      ("Tag definitions", Map.size tagDefs),
      ("Typedefs", Map.size typeDefs)
    ] 
    where
    gmap' = filterGlobalDecls filterFile gmap
    (funDecls, objDecls) = Map.partition (snd) $ (gDecls gmap')
    objDefs = gObjs gmap'
    funDefs = gFuns gmap'
    tagDefs = gTags gmap'
    typeDefs = gTypedefs gmap'
    filterFile :: (CNode n) => n -> Bool
    filterFile = file_filter . posFile . nodePos . nodeInfo
instance Pretty (Either VarDecl ObjDef) where
    pretty = either pretty pretty
instance Pretty (Either VarDecl FunDef) where
    pretty = either pretty pretty
instance Pretty Ident where
    pretty = text . identToString
instance Pretty SUERef where
    pretty (AnonymousType name)   = text $ "$" ++ show name
    pretty (NamedType ident)      = pretty ident
instance Pretty TagDef where
    pretty (CompTag compty) = pretty compty
    pretty (EnumTag enumty) = pretty enumty
instance Pretty IdentDecl where
    pretty (Declaration decl) = pretty decl
    pretty (ObjectDef odef) = pretty odef
    pretty (FunctionDef fdef) = pretty fdef
    pretty (EnumDef enumerator sueref) = pretty enumerator <+> brackets (pretty sueref)
    pretty (TypeDef tydef) = pretty tydef
instance Pretty Decl where
    pretty (Decl vardecl _) = 
        text "Declaration: " <+>
        pretty vardecl
instance Pretty TypeDef where
    pretty (TypeDef' ident ty _) = text "typedef" <+> pretty ident <+> text "as"  <+> pretty ty
instance Pretty ObjDef where
    pretty (ObjDef vardecl init_opt _) = 
        text "Object definition" <+> 
        (maybe (text "(tentative)") (const empty) init_opt) <+> text ": " <+>
        pretty vardecl <+> maybe empty (((text "=") <+>) . pretty) init_opt
instance Pretty FunDef where
    pretty (FunDef vardecl _stmt _) = 
        text "Function definition: " <+>
        pretty vardecl
instance Pretty VarDecl where
    pretty (VarDecl name attrs ty) = 
        ((hsep . punctuate (text " |")) [pretty name, pretty attrs, pretty ty])
instance Pretty ParamDecl where
    pretty (ParamDecl (VarDecl name declattrs ty) _) = 
        pretty declattrs <+> prettyType (show $ pretty name) ty
instance Pretty DeclAttrs where
    pretty (DeclAttrs inline storage attrs) =
        (if inline then (text "inline") else empty) <+>
        (hsep $ [ pretty storage, pretty attrs])

instance Pretty Type  where  
    pretty ty = prettyType "<obj>" ty
prettyType declr_name ty = prettyTy (text declr_name) ty
    where
    prettyTy declr_name (DirectType ty_name quals attrs) = 
        pretty quals <+> pretty attrs <+> pretty ty_name <+> declr_name
    prettyTy declr_name (PtrType ty quals attrs) = 
        prettyTy empty ty <+>  text "*" <+> pretty attrs <+> pretty quals <+> declr_name
    prettyTy declr_name (ArrayType ty sz quals attrs) = 
        prettyTy empty ty <+> declr_name <+> (brackets (pretty quals <+> pretty attrs <+> pretty sz))
    prettyTy declr_name (FunctionType (FunType ty params variadic attrs)) = 
        prettyTy empty ty <+> 
        declr_name <+>
        parens (hsep . punctuate comma $ (map pretty params ++ (if variadic then [text ",..."] else []))) <+>
        pretty attrs
instance Pretty TypeQuals where
    pretty tyQuals = hsep $ map showAttr [ ("const",constant),("volatile",volatile),("restrict",restrict) ]
        where showAttr (str,select) | select tyQuals = text str
                                    | otherwise      = empty
instance Pretty ArraySize where
    pretty IncompleteArray = empty
    pretty VarSizeArray    = text "*"
    pretty (FixedSizeArray static expr) = (if static then text "static" else empty) <+> pretty expr
instance Pretty TypeName where
    pretty TyVoid = text "void"
    pretty (TyIntegral int_type) = text (show int_type)
    pretty (TyFloating tyquals float_type) = pretty tyquals <+> text (show float_type)
    pretty (TyComp (CompTypeDecl sue_ref tag  attrs _)) | null attrs = text (show tag) <+> text (show sue_ref)        
                                                   | otherwise  = error "attributes for su decl"
    pretty (TyEnum (EnumTypeDecl sue_ref attrs _)) | null attrs = text "enum" <+> text (show sue_ref)
                                                   | otherwise  = error "attributes for enum decl"
    pretty (TyTypeDef (TypeDefRef ident _)) = text "typeref" <> parens (pretty ident)
    pretty (TyOfExpr expr) = text "typeof" <> parens (pretty expr)
    pretty (TyBuiltin TyVaList) = text "va_list"
instance Pretty FloatTypeQuals where
    pretty (NoFloatTypeQual) = empty
    pretty TyComplex = text "__complex__"

instance Pretty CompType where
    pretty _ = text "comptype"
instance Pretty EnumType where
    pretty _ = text "enumtype"
instance Pretty Enumerator where
    pretty (ident,expr) = text "enumerator" <+> pretty ident
instance Pretty Storage where
    pretty NoStorage = empty
    pretty Auto      = text "automatic storage"
    pretty Register  = text "register"
    pretty (Static linkage thread_local) = 
        (hcat . punctuate (text "/") $ [ text "static",pretty linkage ])
        <+> (if thread_local then text ", __thread" else empty)
    pretty (FunLinkage linkage) = hsep $ [ text "function", pretty linkage ]
instance Pretty Linkage where
    pretty InternalLinkage = text "internal"
    pretty ExternalLinkage = text "external"    
instance Pretty VarName where
    pretty NoName = text "<anonymous>"
    pretty (VarName ident asmname_opt) = pretty ident <+> (maybe empty pAsmName asmname_opt)
        where pAsmName asmname = text "" <+> parens (text "asmname" <+> pretty asmname)
instance Pretty Attributes where
    pretty = hsep . punctuate comma . map pretty
instance Pretty Attr where
    pretty _ = text "<attribute>"
instance Pretty AsmName where
    pretty = text.show.cstringOfLit