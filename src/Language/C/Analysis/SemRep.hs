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
    
-- /TODO/: At the moment, we do not analyse function bodies.
--         This will change in the future of course, when we've implemented more analysis steps
---------------------------------------------------------------------------------------------------
module Language.C.Analysis.SemRep
where
import Language.C.Syntax

import Data.Map (Map)
import Data.Generics
import Text.PrettyPrint.HughesPJ

-- tags (namespace sum)
data TagDef =  CompTag CompType	  --definition
     	     | EnumTag EnumType      -- enum definition
                deriving (Typeable, Data {-! CNode !-})
sameTagKind :: TagDef -> TagDef -> Bool
sameTagKind (CompTag ct1) (CompTag ct2) = compTag ct1 == compTag ct2
sameTagKind (EnumTag _) (EnumTag _) = True
sameTagKind _ _ = False
instance HasSUERef TagDef where
    sueRef (CompTag ct) = sueRef ct
    sueRef (EnumTag et) = sueRef et
    
-- identifiers, typedefs and enumeration constants (namespace sum)
data IdentDecl =   
	             Declaration VarDecl NodeInfo -- object or function declaration
	           | ObjectDef ObjDef           -- object definition
	           | FunctionDef FunDef         -- function definition
	           | EnumDef Enumerator SUERef       -- definition of an enumerator
               | TypeDef Ident Type         -- typedef declaration
               deriving (Typeable, Data)
identOfDecl :: IdentDecl -> Ident
identOfDecl ident_decl =
    case ident_decl of
        Declaration vd _ -> declIdent vd
        ObjectDef od -> declIdent od
        FunctionDef fd  -> declIdent fd
        EnumDef enumerator _sue_ref -> fst $ enumerator
        TypeDef ident _ty -> ident
    where
    declIdent :: (Decl d) => d -> Ident
    declIdent = identOfVarName . declName

instance CNode IdentDecl where
    nodeInfo (Declaration _ node) = node
    nodeInfo (ObjectDef od) = nodeInfo od             
    nodeInfo (FunctionDef fd) = nodeInfo fd             
    nodeInfo (EnumDef (ident,_) _) = nodeInfo ident 
    nodeInfo (TypeDef ident _) = nodeInfo ident          
objKindDescr :: IdentDecl -> String
objKindDescr  (Declaration _ _) = "declaration"
objKindDescr (ObjectDef _) = "object definition"
objKindDescr (FunctionDef _) = "function definition"
objKindDescr (EnumDef _ _) = "enumerator definition"
objKindDescr (TypeDef _ _) = "typedef"
compatibleObjKind :: IdentDecl -> IdentDecl -> Bool
compatibleObjKind (TypeDef _ _) (TypeDef _ _) = True
compatibleObjKind _ (TypeDef _ _) = False
compatibleObjKind (TypeDef _ _) _ = False
compatibleObjKind (EnumDef _ _) (EnumDef _ _) = True
compatibleObjKind (EnumDef _ _) _ = False
compatibleObjKind _ (EnumDef _ _) = False
compatibleObjKind _ _ = True
    

-- * global declarations and definitions
data GlobalDecls = GlobalDecls {
                     gObjs    :: Map Ident (Either VarDecl ObjDef),
                     gFuns    :: Map Ident (Either VarDecl FunDef),
                     gTags    :: Map SUERef TagDef,
                     gTypedefs :: Map Ident Type
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
     | AsmEvent AsmBlock
       -- ^ assembler block
      
-- * declarations and definitions

-- | class to reduce namespace clutter for declarations and definitions
class Decl n where
    declName :: n -> VarName
    declType :: n -> Type
    declAttrs :: n -> DeclAttrs
instance (Decl a, Decl b) => Decl (Either a b) where
    declName = either declName declName
    declType = either declType declType
    declAttrs = either declAttrs declAttrs
-- | external declarations
data VarDecl = VarDecl VarName DeclAttrs Type
              deriving (Typeable, Data)
instance Decl VarDecl where
    declName  (VarDecl extname _ _)  = extname
    declType  (VarDecl _ _ ty)  = ty
    declAttrs (VarDecl _ declattrs _)  = declattrs
isExtDecl :: VarDecl -> Bool
isExtDecl = hasLinkage . storage . declAttrs

-- | Object Definitions 
-- A object defintion is of the form @ObjDec vardecl initializer? node@
data ObjDef = ObjDef VarDecl (Maybe Initializer) NodeInfo
             deriving (Typeable, Data {-! CNode !-})
instance Decl ObjDef where
    declName  (ObjDef vd _ _) = declName vd
    declType  (ObjDef vd _ _) = declType vd
    declAttrs  (ObjDef vd _ _) = declAttrs vd             
isTentative :: ObjDef -> Bool
isTentative (ObjDef decl init_opt _) | isExtDecl decl = maybe True (const False) init_opt
                                     | otherwise = False
-- | Function definitions
data FunDef = FunDef VarDecl Stmt NodeInfo
             deriving (Typeable, Data {-! CNode !-})
instance Decl FunDef where
    declName  (FunDef vd _ _) = declName vd
    declType  (FunDef vd _ _) = declType vd
    declAttrs  (FunDef vd _ _) = declAttrs vd
             
-- | Parameter declaration @ParamDecl maybeIdent type attrs node@
data ParamDecl = ParamDecl VarDecl NodeInfo
    deriving (Typeable, Data {-! CNode !-} )
instance Decl ParamDecl where
    declName (ParamDecl ld _) = declName ld
    declType (ParamDecl ld _) = declType ld
    declAttrs (ParamDecl ld _) = declAttrs ld
    
-- | Struct\/Union member declaration 
data MemberDecl = MemberDecl VarDecl (Maybe ConstExpr) NodeInfo
                  -- ^ @MemberDecl vardecl bitfieldsize node@
                | AnonBitField Type ConstExpr NodeInfo
                  -- ^ @AnonBitField typ size@
    deriving (Typeable, Data {-! CNode !-} )
instance Decl MemberDecl where
    declName (MemberDecl ld _ _) = declName ld
    declName _ = NoName
    declType (MemberDecl ld _ _) = declType ld
    declType (AnonBitField ty _ _) = ty
    declAttrs (MemberDecl ld _ _) = declAttrs ld
    declAttrs _ = DeclAttrs False NoStorage []
    
-- | attributes of a declared object have the form @DeclAttrs isInlineFunction storage linkage attrs@.
data DeclAttrs = DeclAttrs Bool Storage Attributes
                 -- ^ @DeclAttrs inline storage attrs@
               deriving (Typeable, Data)
storage :: DeclAttrs -> Storage
storage (DeclAttrs _ st _) = st

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

-- | types of c objects
data Type =
       DirectType TypeName TypeQuals Attributes
     -- a non-derived type
     | PtrType Type TypeQuals Attributes
     -- ^ pointer type
     | ArrayType Type ArraySize TypeQuals Attributes
     -- ^ array type
     | FunctionType FunType
     -- ^ function type
     deriving (Typeable, Data)

isFunctionType :: Type -> Bool
isFunctionType ty = case ty of FunctionType _ -> True; _ -> False

-- | An array type may either have a size, incomplete type or variable size.
-- Furthermore, when used as a function parameters, it may have a /static/ qualifier.
data ArraySize =  IncompleteArray 
                | VarSizeArray
                | FixedSizeArray Bool Expr
                -- ^ @FixedSizeArray static@      
               deriving (Typeable, Data)

-- | Function types @FunType params isVariadic attrs type@
data FunType = FunType Type [ParamDecl] Bool Attributes 
               deriving (Typeable, Data)

-- | normalized type representation
data TypeName =
      TyVoid
    | TyIntegral IntType
    | TyFloating FloatTypeQuals FloatType
    | TyComp CompTypeDecl
    | TyEnum EnumTypeDecl
    | TyTypeDef TypeDefRef
    | TyOfExpr  Expr
    | TyBuiltin BuiltinType
    deriving (Typeable, Data)
data BuiltinType = TyVaList 
                   deriving (Typeable, Data)
data TypeDefRef = TypeDefRef Ident NodeInfo
               deriving (Typeable, Data {-! CNode !-})

-- | qualifiers for floating types (gnu complex extensions, other gnu extensions may follow)
data FloatTypeQuals =  NoFloatTypeQual 
                     | TyComplex
                   deriving (Typeable,Data)

-- | integral types (C99 6.7.2.2)
data IntType =
      TyChar
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
    | TyBool
    deriving (Typeable, Data, Eq, Ord, Show)

-- | floating point type (C99 6.7.2.2)
data FloatType =
      TyFloat
    | TyDouble
    | TyLDouble
    deriving (Typeable, Data, Eq, Ord, Show)

-- | accessor class : struct\/union\/enum names
class HasSUERef a where
    sueRef  :: a -> SUERef
    
-- | accessor class : composite type tags (struct or enum)
class HasCompTag a where
    compTag :: a -> CompTag
    
data CompTypeDecl = CompTypeDecl SUERef CompTag Attributes NodeInfo
    deriving (Typeable, Data {-! CNode !-})
instance HasSUERef  CompTypeDecl where sueRef  (CompTypeDecl ref _ _ _) = ref
instance HasCompTag CompTypeDecl where compTag (CompTypeDecl _ tag _ _)  = tag

data EnumTypeDecl = EnumTypeDecl SUERef Attributes NodeInfo
    deriving (Typeable, Data {-! CNode !-})
instance HasSUERef  EnumTypeDecl where sueRef  (EnumTypeDecl ref _ _) = ref
              
-- | C structure or union specifiers (K&R A8.3, C99 6.7.2.1)
--
data CompType =  CompType SUERef CompTag [MemberDecl] Attributes NodeInfo
                 deriving (Typeable, Data {-! CNode !-} )
instance HasSUERef  CompType where sueRef  (CompType ref _ _ _ _) = ref
instance HasCompTag CompType where compTag (CompType _ tag _ _ _) = tag

-- | a tag to determine wheter we refer to a @struct@ or @union@, see 'CCompType'.
data CompTag =  StructTag
              | UnionTag
    deriving (Eq,Ord,Typeable,Data)
instance Show CompTag where
    show StructTag = "struct"
    show UnionTag  = "union"
-- | C enumeration specifier (K&R A8.4, C99 6.7.2.2)
--
data EnumType = EnumType SUERef [Enumerator] Attributes NodeInfo
                 -- ^ @EnumDef name enumeration-constants(-value)? attrs node@
                 deriving (Typeable, Data {-! CNode !-} )
instance HasSUERef EnumType where sueRef  (EnumType ref _ _ _) = ref
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
type AsmBlock = StringLit

-- | Assembler name
type AsmName = StringLit
               
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

-- | C constant (K&R A2.5 & A7.2)
data Constant = IntConst   CInteger NodeInfo
              | CharConst  CChar NodeInfo
              | FloatConst CFloat NodeInfo
              | StrLit     CString NodeInfo
            deriving (Data,Typeable {-! CNode !-})

-- | Attributed string literals
data StringLit = StringLit CString NodeInfo
            deriving (Data,Typeable {-! CNode !-})

cstringOfLit :: StringLit -> CString
cstringOfLit (StringLit cstr _) = cstr



--------------------------------------------------------
-- DERIVES GENERATED CODE
-- DO NOT MODIFY BELOW THIS LINE
-- CHECKSUM: 169674109

instance CNode TagDef
    where nodeInfo (CompTag d) = nodeInfo d
          nodeInfo (EnumTag d) = nodeInfo d
instance Pos TagDef
    where posOf x = nodePos (nodeInfo x)

instance CNode ObjDef
    where nodeInfo (ObjDef _ _ nodeinfo) = nodeinfo
instance Pos ObjDef
    where posOf x = nodePos (nodeInfo x)

instance CNode FunDef
    where nodeInfo (FunDef _ _ nodeinfo) = nodeinfo
instance Pos FunDef
    where posOf x = nodePos (nodeInfo x)

instance CNode ParamDecl
    where nodeInfo (ParamDecl _ nodeinfo) = nodeinfo
instance Pos ParamDecl
    where posOf x = nodePos (nodeInfo x)

instance CNode MemberDecl
    where nodeInfo (MemberDecl _ _ nodeinfo) = nodeinfo
          nodeInfo (AnonBitField _ _ nodeinfo) = nodeinfo
instance Pos MemberDecl
    where posOf x = nodePos (nodeInfo x)

instance CNode TypeDefRef
    where nodeInfo (TypeDefRef _ nodeinfo) = nodeinfo
instance Pos TypeDefRef
    where posOf x = nodePos (nodeInfo x)

instance CNode CompTypeDecl
    where nodeInfo (CompTypeDecl _ _ _ nodeinfo) = nodeinfo
instance Pos CompTypeDecl
    where posOf x = nodePos (nodeInfo x)

instance CNode EnumTypeDecl
    where nodeInfo (EnumTypeDecl _ _ nodeinfo) = nodeinfo
instance Pos EnumTypeDecl
    where posOf x = nodePos (nodeInfo x)

instance CNode CompType
    where nodeInfo (CompType _ _ _ _ nodeinfo) = nodeinfo
instance Pos CompType
    where posOf x = nodePos (nodeInfo x)

instance CNode EnumType
    where nodeInfo (EnumType _ _ _ nodeinfo) = nodeinfo
instance Pos EnumType
    where posOf x = nodePos (nodeInfo x)

instance CNode Initializer_stub
    where nodeInfo (InitExpr _ nodeinfo) = nodeinfo
          nodeInfo (InitList _ nodeinfo) = nodeinfo
instance Pos Initializer_stub
    where posOf x = nodePos (nodeInfo x)

instance CNode Designator
    where nodeInfo (ArrDesig _ nodeinfo) = nodeinfo
          nodeInfo (MemberDesig _ nodeinfo) = nodeinfo
          nodeInfo (RangeDesig _ _ nodeinfo) = nodeinfo
instance Pos Designator
    where posOf x = nodePos (nodeInfo x)

instance CNode Attr
    where nodeInfo (Attr _ _ nodeinfo) = nodeinfo
instance Pos Attr
    where posOf x = nodePos (nodeInfo x)

instance CNode Constant
    where nodeInfo (IntConst _ nodeinfo) = nodeinfo
          nodeInfo (CharConst _ nodeinfo) = nodeinfo
          nodeInfo (FloatConst _ nodeinfo) = nodeinfo
          nodeInfo (StrLit _ nodeinfo) = nodeinfo
instance Pos Constant
    where posOf x = nodePos (nodeInfo x)

instance CNode StringLit
    where nodeInfo (StringLit _ nodeinfo) = nodeinfo
instance Pos StringLit
    where posOf x = nodePos (nodeInfo x)
