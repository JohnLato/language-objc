{-# LANGUAGE DeriveDataTypeable  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis.Syntax
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  non-portable (DeriveDataTypeable)
--
-- This module contains definitions for representing C translation units.
-- In contrast to 'Language.C.Syntax.AST', the representation tries to express the semantics of
-- of a translation unit, and links back to the AST via NodeInfo fields.
--
-- /Ideas/
--
--  1. Pretty Printing anonymous SUE
--
--    * when pretty printing, use a unique name generator to un-anonymize structs
--
--    * use $ names
--
--    * use GNU typeOf extension (together with the next one)
--
--    * (maybe): Keep ref counts, if an anonymous struct is only ref'd one, pp it anonymously
--
--    * (rather not): Try to merge declarations, s.t. we still can use an anonymous struct
--
-- /TODO/  At the moment, we do not analyse function bodies.
--         This will change in the future of course, when we've implemented more analysis steps
---------------------------------------------------------------------------------------------------
module Language.C.Analysis.SemRep
where
import Language.C.Data
import Language.C.Syntax
import Language.C.Syntax.Constants

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Generics
import Text.PrettyPrint.HughesPJ

-- tags (namespace sum)
data TagDef =  CompDef CompType	  --definition
     	       | EnumDef EnumType      -- enum definition
               deriving (Typeable, Data {-! CNode !-})

sameTagKind :: TagDef -> TagDef -> Bool
sameTagKind (CompDef ct1) (CompDef ct2) = compTag ct1 == compTag ct2
sameTagKind (EnumDef _) (EnumDef _) = True
sameTagKind _ _ = False

instance HasSUERef TagDef where
    sueRef (CompDef ct) = sueRef ct
    sueRef (EnumDef et) = sueRef et

typeOfTagDef :: TagDef -> TypeName
typeOfTagDef (CompDef comptype) =  typeOfCompDef comptype
typeOfTagDef (EnumDef enumtype) =  typeOfEnumDef enumtype
  
-- identifiers, typedefs and enumeration constants (namespace sum)
data IdentDecl = Declaration Decl           -- ^ object or function declaration
	             | ObjectDef ObjDef           -- ^ object definition
	             | FunctionDef FunDef         -- ^ function definition
	             | EnumeratorDef Enumerator EnumType  -- ^ definition of an enumerator
               deriving (Typeable, Data)

instance Declaration IdentDecl where
  declName (EnumeratorDef ed _) = VarName (fst ed) Nothing
  declName (Declaration decl) = declName decl
  declName (ObjectDef decl) = declName decl
  declName (FunctionDef decl) = declName decl
  declType (EnumeratorDef _ t) = DirectType (typeOfEnumDef t) noTypeQuals
  declType (Declaration decl) = declType decl
  declType (ObjectDef def) = declType def
  declType (FunctionDef def) = declType def
  declAttrs (EnumeratorDef _ _) = DeclAttrs False NoStorage []
  declAttrs (Declaration decl) = declAttrs decl
  declAttrs (ObjectDef def) = declAttrs def
  declAttrs (FunctionDef def) = declAttrs def

identOfDecl :: IdentDecl -> Ident
identOfDecl ident_decl = identOfVarName (declName ident_decl)

instance CNode IdentDecl where
    nodeInfo (Declaration decl) = nodeInfo decl
    nodeInfo (ObjectDef od) = nodeInfo od
    nodeInfo (FunctionDef fd) = nodeInfo fd
    nodeInfo (EnumeratorDef (ident,_) _) = nodeInfo ident

objKindDescr :: IdentDecl -> String
objKindDescr  (Declaration _ ) = "declaration"
objKindDescr (ObjectDef _) = "object definition"
objKindDescr (FunctionDef _) = "function definition"
objKindDescr (EnumeratorDef _ _) = "enumerator definition"

compatibleObjKind :: IdentDecl -> IdentDecl -> Bool
compatibleObjKind (EnumeratorDef _ _) (EnumeratorDef _ _) = True
compatibleObjKind (EnumeratorDef _ _) _ = False
compatibleObjKind _ (EnumeratorDef _ _) = False
compatibleObjKind _ _ = True

splitIdentDecls :: Map Ident IdentDecl -> (Map Ident (Enumerator,EnumType) , Map Ident Decl,
                                           Map Ident ObjDef, Map Ident FunDef)
splitIdentDecls = Map.foldWithKey deal (Map.empty,Map.empty,Map.empty,Map.empty)
  where
  deal ident (EnumeratorDef e ety) (es,ds,os,fs) = (Map.insert ident (e,ety) es, ds, os ,fs)
  deal ident (Declaration d) (es,ds,os,fs) = (es, Map.insert ident d ds, os ,fs)
  deal ident (ObjectDef o) (es,ds,os,fs) = (es, ds, Map.insert ident o os ,fs)
  deal ident (FunctionDef f) (es,ds,os,fs) = (es, ds, os ,Map.insert ident f fs)


-- * global declarations and definitions
data GlobalDecls = GlobalDecls {
                     gObjs     :: Map Ident IdentDecl,
                     gTags     :: Map SUERef TagDef,
                     gTypedefs :: Map Ident Typedef
                   }
-- some boilerplate for the user
emptyGlobalDecls :: GlobalDecls
emptyGlobalDecls = GlobalDecls Map.empty Map.empty Map.empty

filterGlobalDecls :: (DeclEvent -> Bool) -> GlobalDecls -> GlobalDecls
filterGlobalDecls decl_filter gmap = GlobalDecls
    {
        gObjs  = Map.filter (decl_filter . DeclEvent) (gObjs gmap),
        gTags  = Map.filter (decl_filter . TagEvent) (gTags gmap),
        gTypedefs = Map.filter (decl_filter . TypedefEvent) (gTypedefs gmap)
    }
mergeGlobalDecls :: GlobalDecls -> GlobalDecls -> GlobalDecls
mergeGlobalDecls gmap1 gmap2 = GlobalDecls
    {
        gObjs  = Map.union (gObjs gmap1) (gObjs gmap2),
        gTags  = Map.union  (gTags gmap1) (gTags gmap2),
        gTypedefs = Map.union (gTypedefs gmap1) (gTypedefs gmap2)
    }

-- * Events

-- | declaration events
--
-- /PRELIMINARY/ This will change soon, but we have to take a look what makes most sense
data DeclEvent =
       TagEvent  TagDef
       -- ^ file-scope struct\/union\/enum event
     | DeclEvent IdentDecl
       -- ^ file-scope declaration or definition
     | TypedefEvent Typedef
       -- ^ a type definition
     | AsmEvent AsmBlock
       -- ^ assembler block
     deriving ({-! CNode !-})
     
-- * declarations and definitions

-- | class to reduce namespace clutter for declarations and definitions
class Declaration n where
    declName :: n -> VarName
    declType :: n -> Type
    declAttrs :: n -> DeclAttrs

instance (Declaration a, Declaration b) => Declaration (Either a b) where
    declName = either declName declName
    declType = either declType declType
    declAttrs = either declAttrs declAttrs

-- | Declarations, which aren't definitions
data Decl = Decl VarDecl NodeInfo
            deriving (Typeable, Data {-! CNode !-})
instance Declaration Decl where
    declName  (Decl vd _) = declName vd
    declType  (Decl vd _) = declType vd
    declAttrs (Decl vd _) = declAttrs vd

-- | Object Definitions
-- A object defintion is of the form @ObjDec vardecl initializer? node@
data ObjDef = ObjDef VarDecl (Maybe Initializer) NodeInfo
             deriving (Typeable, Data {-! CNode !-})
instance Declaration ObjDef where
    declName  (ObjDef vd _ _) = declName vd
    declType  (ObjDef vd _ _) = declType vd
    declAttrs  (ObjDef vd _ _) = declAttrs vd

isTentative :: ObjDef -> Bool
isTentative (ObjDef decl init_opt _) | isExtDecl decl = maybe True (const False) init_opt
                                     | otherwise = False
-- | Function definitions
data FunDef = FunDef VarDecl Stmt NodeInfo
             deriving (Typeable, Data {-! CNode !-})
instance Declaration FunDef where
    declName  (FunDef vd _ _) = declName vd
    declType  (FunDef vd _ _) = declType vd
    declAttrs  (FunDef vd _ _) = declAttrs vd

-- | Parameter declaration @ParamDecl maybeIdent type attrs node@
data ParamDecl = ParamDecl VarDecl NodeInfo
    deriving (Typeable, Data {-! CNode !-} )
instance Declaration ParamDecl where
    declName (ParamDecl ld _) = declName ld
    declType (ParamDecl ld _) = declType ld
    declAttrs (ParamDecl ld _) = declAttrs ld

-- | Struct\/Union member declaration
data MemberDecl = MemberDecl VarDecl (Maybe ConstExpr) NodeInfo
                  -- ^ @MemberDecl vardecl bitfieldsize node@
                | AnonBitField Type ConstExpr NodeInfo
                  -- ^ @AnonBitField typ size@
    deriving (Typeable, Data {-! CNode !-} )
instance Declaration MemberDecl where
    declName (MemberDecl ld _ _) = declName ld
    declName _ = NoName
    declType (MemberDecl ld _ _) = declType ld
    declType (AnonBitField ty _ _) = ty
    declAttrs (MemberDecl ld _ _) = declAttrs ld
    declAttrs _ = DeclAttrs False NoStorage []

-- | Generic variable declarations
data VarDecl = VarDecl VarName DeclAttrs Type
              deriving (Typeable, Data)
instance Declaration VarDecl where
    declName  (VarDecl extname _ _)  = extname
    declType  (VarDecl _ _ ty)  = ty
    declAttrs (VarDecl _ declattrs _)  = declattrs
isExtDecl :: VarDecl -> Bool
isExtDecl = hasLinkage . declStorage

-- | attributes of a declared object have the form @DeclAttrs isInlineFunction storage linkage attrs@.
data DeclAttrs = DeclAttrs Bool Storage Attributes
                 -- ^ @DeclAttrs inline storage attrs@
               deriving (Typeable, Data)
declStorage :: (Declaration d) => d -> Storage
declStorage d = case declAttrs d of (DeclAttrs _ st _) -> st

-- In C we have
--  Identifiers can either have internal, external or no linkage
--  (same object everywhere, same object within the translation unit, unique).
--  * top-level identifiers
--      static : internal linkage (objects and function defs)
--      extern : linkage of prior declaration (if specified), external linkage otherwise
--      no-spec: external linkage
--  * storage duration
--      * static storage duration: objects with external or internal linkage, or local ones with the static keyword
--      * automatic storage duration: otherwise (register)
-- See http://publications.gbdirect.co.uk/c_book/chapter8/declarations_and_definitions.html, Table 8.1, 8.2

-- | Storage duration of a variable - can either be static, allocated, register allocated
data Storage  =  NoStorage                  -- ^ no storage
               | Auto                       -- ^ automatic storage
               | Register                   -- ^ register storage
               | Static Linkage ThreadLocal -- ^ static storage, with linkage and thread local specifier (gnu c)
               | FunLinkage Linkage         -- ^ function, either internal or external linkage
               deriving (Typeable, Data, Show, Eq, Ord)
type ThreadLocal = Bool
data Linkage = InternalLinkage | ExternalLinkage
               deriving (Typeable, Data, Show, Eq, Ord)
hasLinkage :: Storage -> Bool
hasLinkage (Static _ _) = True
hasLinkage _ = False

-- * types

-- | Typedefs
data Typedef = Typedef Ident Type Attributes NodeInfo
               deriving (Typeable, Data {-! CNode !-} )

identOfTypedef :: Typedef -> Ident
identOfTypedef (Typedef ide _ _ _) = ide

-- | types of c objects
data Type =
       DirectType TypeName TypeQuals
     -- ^ a non-derived type
     | PtrType Type TypeQuals Attributes
     -- ^ pointer type
     | ArrayType Type ArraySize TypeQuals Attributes
     -- ^ array type
     | FunctionType FunType
     -- ^ function type
     | TypedefType TypedefRef
     -- ^ a defined type
     | TypeOfExpr Expr
     -- ^ (GNU) typeof (/broken/ and should be remove, because we do not have expression type analysis)
     deriving (Typeable, Data)
-- | Function types @FunType params isVariadic attrs type@
data FunType = FunType Type [ParamDecl] Bool Attributes
               deriving (Typeable, Data)

derefTypedef :: Type -> Type
derefTypedef (TypedefType (TypedefRef _ (Just actual_ty) _)) = derefTypedef actual_ty
derefTypedef ty = ty

-- may not be called on undefined typedefs or typeof(expr)
referencedType :: Type -> Maybe Type
referencedType (PtrType ty _ _) = Just ty
referencedType (FunctionType (FunType ty _ _ _)) = Just ty
referencedType (ArrayType ty _ _ _) = Just ty
referencedType (TypedefType (TypedefRef _ (Just actual_ty) _)) = Just actual_ty
referencedType (DirectType _ _) = Nothing
referencedType _ = error "referencedType: failed to resolve type"


hasTypeOfExpr :: Type -> Bool
hasTypeOfExpr (TypeOfExpr _) = True
hasTypeOfExpr ty = maybe False hasTypeOfExpr (referencedType ty)

-- | note that this cannot be answered in the presence of typedefs and, even worse, typeOfExpr types
isFunctionType :: Type -> Bool
isFunctionType ty =
    case ty of  TypedefType (TypedefRef _ (Just actual_ty) _) -> isFunctionType actual_ty
                TypedefType _ -> error "isFunctionType: unresolved typedef"
                TypeOfExpr _  -> error "isFunctionType: typeof(expr)"
                FunctionType _ -> True
                _ -> False

-- | An array type may either have unknown size or a specified array size, the latter either variable or constant.
-- Furthermore, when used as a function parameters, the size may be qualified as /static/.
-- In a function prototype, the size may be `Unspecified variable size' (@[*]@).
data ArraySize =  UnknownArraySize Bool
                | ArraySize Bool Expr
                -- ^ @FixedSizeArray static@
               deriving (Typeable, Data)

-- | normalized type representation
data TypeName =
      TyVoid
    | TyIntegral IntType
    | TyFloating FloatType
    | TyComplex  FloatType
    | TyComp CompTypeDecl
    | TyEnum EnumTypeDecl
    | TyBuiltin BuiltinType
    deriving (Typeable, Data)

-- | Builtin type (va_list)
data BuiltinType = TyVaList
                   deriving (Typeable, Data)

-- | typdef references
-- If the actual type is known, it is attached for convenience
data TypedefRef = TypedefRef Ident (Maybe Type) NodeInfo
               deriving (Typeable, Data {-! CNode !-})

-- | integral types (C99 6.7.2.2)
data IntType =
      TyBool
    | TyChar
    | TySChar
    | TyUChar
    | TyShort
    | TyUShort
    | TyInt
    | TyUInt
    | TyLong
    | TyULong
    | TyLLong
    | TyULLong
    deriving (Typeable, Data, Eq, Ord)
instance Show IntType where
    show TyBool = "_Bool"
    show TyChar = "char"
    show TySChar = "signed char"
    show TyUChar = "unsigned char"
    show TyShort = "short"
    show TyUShort = "unsigned short"
    show TyInt = "int"
    show TyUInt = "unsigned int"
    show TyLong = "long"
    show TyULong = "unsigned long"
    show TyLLong = "long long"
    show TyULLong = "unsigned long long"

-- | floating point type (C99 6.7.2.2)
data FloatType =
      TyFloat
    | TyDouble
    | TyLDouble
    deriving (Typeable, Data, Eq, Ord)
instance Show FloatType where
    show TyFloat = "float"
    show TyDouble = "double"
    show TyLDouble = "long double"

-- | accessor class : struct\/union\/enum names
class HasSUERef a where
    sueRef  :: a -> SUERef

-- | accessor class : composite type tags (struct or enum)
class HasCompTyKind a where
    compTag :: a -> CompTyKind
    
data CompTypeDecl = CompTypeDecl SUERef CompTyKind NodeInfo
    deriving (Typeable, Data {-! CNode !-})
instance HasSUERef  CompTypeDecl where sueRef  (CompTypeDecl ref _ _) = ref
instance HasCompTyKind CompTypeDecl where compTag (CompTypeDecl _ tag _)  = tag

data EnumTypeDecl = EnumTypeDecl SUERef NodeInfo
    deriving (Typeable, Data {-! CNode !-})
instance HasSUERef  EnumTypeRef where sueRef  (EnumTypeRef ref _) = ref

-- | C structure or union specifiers (K&R A8.3, C99 6.7.2.1)
--
data CompType =  CompType SUERef CompTyKind [MemberDecl] Attributes NodeInfo
                 deriving (Typeable, Data {-! CNode !-} )
instance HasSUERef  CompType where sueRef  (CompType ref _ _ _ _) = ref
instance HasCompTyKind CompType where compTag (CompType _ tag _ _ _) = tag

typeOfCompDef :: CompType -> TypeName
typeOfCompDef (CompType ref tag _ _ _) = TyComp (CompTypeDecl ref tag internalNode)

-- | a tag to determine wheter we refer to a @struct@ or @union@, see 'CCompType'.
data CompTyKind =  StructTag
              | UnionTag
    deriving (Eq,Ord,Typeable,Data)
instance Show CompTyKind where
    show StructTag = "struct"
    show UnionTag  = "union"
-- | C enumeration specifier (K&R A8.4, C99 6.7.2.2)
--
data EnumType = EnumType SUERef [Enumerator] Attributes NodeInfo
                 -- ^ @EnumType name enumeration-constants(-value)? attrs node@
                 deriving (Typeable, Data {-! CNode !-} )
instance HasSUERef EnumType where sueRef  (EnumType ref _ _ _) = ref

typeOfEnumDef :: EnumType -> TypeName
typeOfEnumDef (EnumType ref _ _ _) = TyEnum (EnumTypeDecl ref internalNode)

type Enumerator = (Ident,Maybe Expr)
type Initializer = CInit

-- | C initialization (K&R A8.7, C99 6.7.8)
-- 
data Initializer_stub = InitExpr Expr NodeInfo            -- ^ assignment expression
                      | InitList InitList NodeInfo         -- ^ initialization list (see 'InitList')
                 deriving (Typeable, Data {-! CNode !-} )

-- | Initializer List
--
type InitList = [([Designator], Initializer)]
  
-- | Designators
--
-- A designator specifies a member of an object, either an element or range of an array,
-- or the named member of a struct \/ union.
data Designator = ArrDesig Expr NodeInfo        -- ^ array position designator
                | MemberDesig Ident NodeInfo    -- ^ member designator
                | RangeDesig Expr Expr NodeInfo -- ^ array range designator @CRangeDesig from to _@ (GNU C)
    deriving (Typeable, Data {-! CNode !-} )

data TypeQuals = TypeQuals { constant :: Bool, volatile :: Bool, restrict :: Bool }
    deriving (Typeable, Data)
noTypeQuals :: TypeQuals
noTypeQuals = TypeQuals False False False
mergeTypeQuals :: TypeQuals -> TypeQuals -> TypeQuals
mergeTypeQuals (TypeQuals c1 v1 r1) (TypeQuals c2 v2 r2)
    = TypeQuals (c1 && c2) (v1 && v2) (r1 && r2)

-- * statements and expressions (/TODO/)
type Stmt = CStat
type Expr = CExpr
type ConstExpr = Expr

-- * names and attributes

-- | @VarName name assembler-name@ is a name of an declared object
data VarName =  VarName Ident (Maybe AsmName)
              | NoName
               deriving (Typeable, Data)
identOfVarName :: VarName -> Ident
identOfVarName NoName            = error "identOfVarName: NoName"
identOfVarName (VarName ident _) = ident
-- | Top level assembler block
type AsmBlock = CStrLit

-- | Assembler name
type AsmName = CStrLit

-- | @__attribute__@ annotations
--
-- Those are of the form @Attr attribute-name attribute-parameters@,
-- and serve as generic properties of some syntax tree elements.
--
-- Some examples:
--
-- * labels can be attributed with /unused/ to indicate that their not used
--
-- * struct definitions can be attributed with /packed/ to tell the compiler to use the most compact representation
--
-- * declarations can be attributed with /deprecated/
--
-- * function declarations can be attributes with /noreturn/ to tell the compiler that the function will never return,
--
-- * or with /const/ to indicate that it is a pure function
--
-- /TODO/: ultimatively, we want to parse attributes and represent them in a typed way
data Attr = Attr Ident [Expr] NodeInfo
            deriving (Typeable, Data {-! CNode !-})

type Attributes = [Attr]



--------------------------------------------------------
-- DERIVES GENERATED CODE
-- DO NOT MODIFY BELOW THIS LINE
-- CHECKSUM: 742172418

instance CNode TagDef
    where nodeInfo (CompDef d) = nodeInfo d
          nodeInfo (EnumDef d) = nodeInfo d
instance Pos TagDef
    where posOf x = posOfNode (nodeInfo x)

instance CNode DeclEvent
    where nodeInfo (TagEvent d) = nodeInfo d
          nodeInfo (DeclEvent d) = nodeInfo d
          nodeInfo (TypedefEvent d) = nodeInfo d
          nodeInfo (AsmEvent d) = nodeInfo d
instance Pos DeclEvent
    where posOf x = posOfNode (nodeInfo x)

instance CNode Decl
    where nodeInfo (Decl _ nodeinfo) = nodeinfo
instance Pos Decl
    where posOf x = posOfNode (nodeInfo x)

instance CNode ObjDef
    where nodeInfo (ObjDef _ _ nodeinfo) = nodeinfo
instance Pos ObjDef
    where posOf x = posOfNode (nodeInfo x)

instance CNode FunDef
    where nodeInfo (FunDef _ _ nodeinfo) = nodeinfo
instance Pos FunDef
    where posOf x = posOfNode (nodeInfo x)

instance CNode ParamDecl
    where nodeInfo (ParamDecl _ nodeinfo) = nodeinfo
instance Pos ParamDecl
    where posOf x = posOfNode (nodeInfo x)

instance CNode MemberDecl
    where nodeInfo (MemberDecl _ _ nodeinfo) = nodeinfo
          nodeInfo (AnonBitField _ _ nodeinfo) = nodeinfo
instance Pos MemberDecl
    where posOf x = posOfNode (nodeInfo x)

instance CNode Typedef
    where nodeInfo (Typedef _ _ _ nodeinfo) = nodeinfo
instance Pos Typedef
    where posOf x = posOfNode (nodeInfo x)

instance CNode TypedefRef
    where nodeInfo (TypedefRef _ _ nodeinfo) = nodeinfo
instance Pos TypedefRef
    where posOf x = posOfNode (nodeInfo x)

instance CNode CompTypeDecl
    where nodeInfo (CompTypeDecl _ _ nodeinfo) = nodeinfo
instance Pos CompTypeDecl
    where posOf x = posOfNode (nodeInfo x)

instance CNode EnumTypeDecl
    where nodeInfo (EnumTypeDecl _ nodeinfo) = nodeinfo
instance Pos EnumTypeDecl
    where posOf x = posOfNode (nodeInfo x)

instance CNode CompType
    where nodeInfo (CompType _ _ _ _ nodeinfo) = nodeinfo
instance Pos CompType
    where posOf x = posOfNode (nodeInfo x)

instance CNode EnumType
    where nodeInfo (EnumType _ _ _ nodeinfo) = nodeinfo
instance Pos EnumType
    where posOf x = posOfNode (nodeInfo x)

instance CNode Initializer_stub
    where nodeInfo (InitExpr _ nodeinfo) = nodeinfo
          nodeInfo (InitList _ nodeinfo) = nodeinfo
instance Pos Initializer_stub
    where posOf x = posOfNode (nodeInfo x)

instance CNode Designator
    where nodeInfo (ArrDesig _ nodeinfo) = nodeinfo
          nodeInfo (MemberDesig _ nodeinfo) = nodeinfo
          nodeInfo (RangeDesig _ _ nodeinfo) = nodeinfo
instance Pos Designator
    where posOf x = posOfNode (nodeInfo x)

instance CNode Attr
    where nodeInfo (Attr _ _ nodeinfo) = nodeinfo
instance Pos Attr
    where posOf x = posOfNode (nodeInfo x)
