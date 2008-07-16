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
-- In contrast to 'Language.C.Parser.AST', the representation tries to express the semantics of
-- of a translation unit, and links back to the AST via NodeInfo fields.
--
-- /TODO/: At the moment, we do not analyse function bodies.
--         This will change in the future of course, when we've implemented more analysis steps
---------------------------------------------------------------------------------------------------
module Language.C.Analysis.SemRep
where
import Language.C.Common
import Language.C.Common.Constants
import Language.C.Common.Ops
import Language.C.Parser.AST -- placeholders
import Data.Map (Map)
import Data.Generics

-- tags (namespace sum)
data TagDef =  CompTag (Either CompTypeDecl CompType)	  -- struct-union declaration/definition
    	     | EnumTag (Either EnumTypeDecl Enumeration) -- enum declaration/definition
               deriving (Typeable, Data)

-- identifiers, typedefs and enumeration constants (namespace sum)
data IdentDef = TypeDefIdent TypeName               -- typedef declaration
	          | DeclIdent VarDecl                   -- object or function declaration
	          | DefIdent (Either FunDef ObjDef)	    -- object or function definition
	          | EnumIdent Ident SueRef              -- enumerator
               deriving (Typeable, Data)

-- global declarations and definitions
data GlobalDecls = GlobalDecls {
                     gObjs    :: Map Ident (Either VarDecl ObjDef),
                     gFuns    :: Map Ident (Either VarDecl FunDef),
                     gTags    :: Map SueRef TagDef,
                     gTypedefs :: Map Ident TypeName,
                     gEnums   :: Map Ident SueRef
                   }
                   deriving (Typeable, Data)

-- | C translation unit
--
-- A list of external declarations
data TranslUnit = TranslUnit [ExtDef] GlobalDecls

-- * declarations and definitions

-- | file scope definitions
data ExtDef = 
      ExtObjDef ObjDef
      -- ^ file-scope object definition
    | ExtFunDef FunDef
      -- ^ file-scope function definition
    | ExtAsm AsmBlock
      -- ^ assembler block
    | DeclHint IdentDef
      -- ^ hint where to place declarations
    deriving (Typeable, Data)

-- | class to reduce namespace clutter for declarations and definitions
class Decl n where
    declName :: n -> VarName
    declType :: n -> Type
    declAttrs :: n -> DeclAttrs

-- | external declarations
data VarDecl = VarDecl VarName DeclAttrs Type Attributes
              deriving (Typeable, Data)
instance Decl VarDecl where
    declName  (VarDecl extname declattrs ty _)  = extname
    declType  (VarDecl name declattrs ty _)  = ty
    declAttrs (VarDecl name declattrs ty _)  = declattrs
isExtDecl :: VarDecl -> Bool
isExtDecl = hasLinkage . storage . declAttrs

-- | Object Definitions 
data ObjDef = ObjDef VarDecl (Maybe Initializer) NodeInfo
             deriving (Typeable, Data {-! CNode !-})
isTentative :: ObjDef -> Bool
isTentative (ObjDef decl mInit _) | isExtDecl decl = maybe True (const False) mInit
                                  | otherwise = False
-- | Function definitions
data FunDef = FunDef VarDecl Stmt NodeInfo
             deriving (Typeable, Data {-! CNode !-})

             
-- | Parameter declaration @ParamDecl maybeIdent type attrs node@
data ParamDecl = ParamDecl VarDecl NodeInfo
    deriving (Typeable, Data {-! CNode !-} )
instance Decl ParamDecl where
    declName (ParamDecl ld _) = declName ld
    declType (ParamDecl ld _) = declType ld
    declAttrs (ParamDecl ld _) = declAttrs ld
    
-- | Struct\/Union member declaration 
data MemberDecl = MemberDecl VarDecl NodeInfo
                  -- ^ @MemberDecl name type bitfieldsize attrs node@
                | AnonBitfield Type NodeInfo
                  -- ^ @AnonBitfield size@
    deriving (Typeable, Data {-! CNode !-} )
instance Decl MemberDecl where
    declName (MemberDecl ld _) = declName ld
    declName _ = NoName
    declType (MemberDecl ld _) = declType ld
    declType (AnonBitfield ty _) = ty
    declAttrs (MemberDecl ld _) = declAttrs ld
    declAttrs _ = DeclAttrs False NoStorage []
    
-- | type names
data TypeNameDef = TypeNameDef' Ident Type Attributes NodeInfo
                   deriving (Typeable, Data {-! CNode !-} )
 
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
       DirectType TypeQuals TypeName Attributes
     -- a non-derived type
     | PtrType TypeQuals Type Attributes
     -- ^ pointer type
     | ArrayType TypeQuals ArraySize Type Attributes
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
data FunType = FunType [ParamDecl] Bool Attributes Type
               deriving (Typeable, Data)

-- | normalized type representation
data TypeName =
      TyVoid
    | TyIntegral IntType
    | TyFloating FloatTypeQuals FloatType
    | TyComp CompTypeDecl
    | TyEnum EnumTypeDecl
    | TypeNameType TypeDefRef
    | TypeOfExpr  Expr
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

data CompTypeDecl = CompTypeDecl SueRef CompTag NodeInfo
    deriving (Typeable, Data {-! CNode !-})
data EnumTypeDecl = EnumTypeDecl SueRef NodeInfo
    deriving (Typeable, Data {-! CNode !-})
                

data TypeDecl = CompDef CompType
              | EnumDef Enumeration                
              deriving (Typeable, Data)
              
-- | C structure or union specifiers (K&R A8.3, C99 6.7.2.1)
--
data CompType =  CompType SueRef [MemberDecl] Attributes NodeInfo
               deriving (Typeable, Data {-! CNode !-} )

-- | a tag to determine wheter we refer to a @struct@ or @union@, see 'CCompType'.
data CompTag =  StructTag
              | UnionTag
    deriving (Eq,Ord,Typeable,Data)

-- | C enumeration specifier (K&R A8.4, C99 6.7.2.2)
--
data Enumeration = EnumType SueRef [(Ident,Maybe Expr)] Attributes NodeInfo
                 -- ^ @EnumDef name enumeration-constants(-value)? attrs node@
                 deriving (Typeable, Data {-! CNode !-} )
          
-- | C initialization (K&R A8.7, C99 6.7.8)
-- 
data Initializer = InitExpr Expr NodeInfo            -- ^ assignment expression
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

-- * statements and expressions (/TODO/)
type Stmt = CStat
type Expr = CExpr

-- * names and attributes

-- | @VarName name assembler-name@ is a name of an declared object
data VarName =  VarName Ident (Maybe AsmName)
              | NoName
               deriving (Typeable, Data)
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
-- CHECKSUM: 1487437734

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
    where nodeInfo (MemberDecl _ nodeinfo) = nodeinfo
          nodeInfo (AnonBitfield _ nodeinfo) = nodeinfo
instance Pos MemberDecl
    where posOf x = nodePos (nodeInfo x)

instance CNode TypeNameDef
    where nodeInfo (TypeNameDef' _ _ _ nodeinfo) = nodeinfo
instance Pos TypeNameDef
    where posOf x = nodePos (nodeInfo x)

instance CNode TypeDefRef
    where nodeInfo (TypeDefRef _ nodeinfo) = nodeinfo
instance Pos TypeDefRef
    where posOf x = nodePos (nodeInfo x)

instance CNode CompTypeDecl
    where nodeInfo (CompTypeDecl _ _ nodeinfo) = nodeinfo
instance Pos CompTypeDecl
    where posOf x = nodePos (nodeInfo x)

instance CNode EnumTypeDecl
    where nodeInfo (EnumTypeDecl _ nodeinfo) = nodeinfo
instance Pos EnumTypeDecl
    where posOf x = nodePos (nodeInfo x)

instance CNode CompType
    where nodeInfo (CompType _ _ _ nodeinfo) = nodeinfo
instance Pos CompType
    where posOf x = nodePos (nodeInfo x)

instance CNode Enumeration
    where nodeInfo (EnumType _ _ _ nodeinfo) = nodeinfo
instance Pos Enumeration
    where posOf x = nodePos (nodeInfo x)

instance CNode Initializer
    where nodeInfo (InitExpr _ nodeinfo) = nodeinfo
          nodeInfo (InitList _ nodeinfo) = nodeinfo
instance Pos Initializer
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
