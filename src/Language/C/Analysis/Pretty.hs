{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis.Pretty
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  internal use
-- Portability :  unspecified
--
-- Pretty printing the semantic analysis representation.
-- This is currently only intended for debugging purposes.
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
        declMaps = [ prettyMap "declarations" $ gDecls gd,
                     prettyMap "objects" $ gObjs gd,  prettyMap "functions" $ gFuns gd, 
                     prettyMap "tags"    $ gTags gd,  prettyMap "typedefs"  $ gTypedefs gd ]
        prettyMap :: (Pretty t, Pretty k) => String -> Map k t -> Doc
        prettyMap label = prettyAssocsWith label pretty pretty . Map.assocs
prettyAssocsWith :: String -> (k -> Doc) -> (v -> Doc) -> [(k,v)] -> Doc
prettyAssocsWith label prettyKey prettyVal theMap =
    text label $$ (nest 8) (vcat $ map prettyEntry theMap)
    where
    prettyEntry (k,v) = prettyKey k <+> text " ~> " <+> prettyVal v

globalDeclStats :: (FilePath -> Bool) -> GlobalDecls -> [(String,Int)]
globalDeclStats file_filter gmap =
    [ ("Object/Function Declarations",Map.size decls),
      ("Function Definitions", Map.size funDefs),
      ("Tag definitions", Map.size tagDefs),
      ("Typedefs", Map.size typeDefs)
    ] 
    where
    gmap' = filterGlobalDecls filterFile gmap
    decls = gDecls gmap'
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
    pretty (AnonymousType name)   = text $ "$sue_" ++ show (nameId name)
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
        text "DECL" <+>
        pretty vardecl
instance Pretty TypeDef where
    pretty (TypeDef' ident ty attrs _) = 
        text "typedef" <+> pretty ident <+> text "as"  <+> 
        pretty attrs <+> pretty ty
instance Pretty ObjDef where
    pretty (ObjDef vardecl init_opt _) = 
        text "OBJ_DEF" <+> 
        pretty vardecl <+> maybe empty (((text "=") <+>) . pretty) init_opt
instance Pretty FunDef where
    pretty (FunDef vardecl _stmt _) = 
        text "FUN_DEF" <+>
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
    pretty ty = prettyType "<?>" ty
prettyType declr_name ty = prettyTy (text declr_name) ty
    where
    prettyTy declr_name (DirectType ty_name quals attrs) = 
        pretty quals <+> pretty attrs <+> pretty ty_name <+> declr_name
    prettyTy declr_name (TypeDefType (TypeDefRef ident _ _)) = 
        text "typeref" <> parens (pretty ident) <+> declr_name
    prettyTy declr_name (TypeOfExpr expr) = 
        text "typeof" <> parens (pretty expr) <+> declr_name
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
    pretty (TyComp (CompTypeDecl sue_ref tag _)) = text (show tag) <+> pretty sue_ref
    pretty (TyEnum (EnumTypeDecl sue_ref _))     = text "enum" <+> pretty sue_ref
    pretty (TyBuiltin TyVaList) = text "va_list"
instance Pretty FloatTypeQuals where
    pretty (NoFloatTypeQual) = empty
    pretty TyComplex = text "__complex__"

instance Pretty CompType where
    pretty (CompType sue_ref tag members attrs node) =
        (text.show) tag <+> pretty sue_ref <+> 
        braces (terminateSemi members) <+>
        pretty attrs
instance Pretty MemberDecl where
    pretty (MemberDecl (VarDecl name declattrs ty) bitfield _) = 
        pretty declattrs <+> prettyType (show $ pretty name) ty <+>
        (maybe empty (\bf -> text ":" <+> pretty bf) bitfield)
    pretty (AnonBitField ty bitfield_sz _) =
        pretty ty <+> text ":" <+> pretty bitfield_sz

instance Pretty EnumType where
    pretty (EnumType sue_ref enumerators attrs _) = 
        text "enum" <+> pretty sue_ref <+> braces (terminateSemi enumerators) <+> pretty attrs
instance Pretty Enumerator where
    pretty (ident,expr) = text "enumerator" <+> pretty ident
instance Pretty Storage where
    pretty NoStorage = empty
    pretty Auto      = text "automatic storage"
    pretty Register  = text "register"
    pretty (Static linkage thread_local) = 
        (hcat . punctuate (text "/") $ [ text "static",pretty linkage ])
        <+> (if thread_local then text ", __thread" else empty)
    pretty (FunLinkage linkage) = text "function/" <> pretty linkage
instance Pretty Linkage where
    pretty InternalLinkage = text "internal"
    pretty ExternalLinkage = text "external"    
instance Pretty VarName where
    pretty NoName = text "<anonymous>"
    pretty (VarName ident asmname_opt) = pretty ident <+> (maybe empty pAsmName asmname_opt)
        where pAsmName asmname = text "" <+> parens (text "asmname" <+> pretty asmname)
instance Pretty Attributes where
    pretty = joinComma
instance Pretty Attr where
    pretty (Attr ident es _) = pretty ident <+> (if null es then empty else text "(...)")
instance Pretty AsmName where
    pretty = text.show.cstringOfLit
    
joinComma :: (Pretty a) => [a] -> Doc
joinComma = hsep . punctuate comma . map pretty
terminateSemi :: (Pretty a) => [a] -> Doc
terminateSemi = hsep . map (<> semi) . map pretty 