{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.ObjC.Syntax.AST
-- Copyright   :  (c) [1999..2007] Manuel M T Chakravarty
--                (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  : benedikt.huber@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Abstract syntax of C source and header files.
--
--  The tree structure is based on the grammar in Appendix A of K&R.  The
--  abstract syntax simplifies the concrete syntax by merging similar concrete
--  constructs into a single type of abstract tree structure: declarations are
--  merged with structure declarations, parameter declarations and type names,
--  and declarators are merged with abstract declarators.
--
--  With K&R we refer to ``The C Programming Language'', second edition, Brain
--  W. Kernighan and Dennis M. Ritchie, Prentice Hall, 1988. The AST supports all
--  of C99 <http://www.open-std.org/JTC1/SC22/WG14/www/docs/n1256.pdf> and several
--  GNU extensions <http://gcc.gnu.org/onlinedocs/gcc/C-Extensions.html>.
-----------------------------------------------------------------------------
module Language.ObjC.Syntax.AST (
  -- * C translation units
  CTranslUnit,  CExtDecl,
  CTranslationUnit(..),  CExternalDeclaration(..),
  -- * Declarations
  CFunDef,  CDecl, CStructUnion, CEnum,
  CFunctionDef(..),  CDeclaration(..),
  CStructTag(..), CStructureUnion(..),  CEnumeration(..),
  -- ** Objective-C extensions
  ObjCIface, ObjCClassDef, ObjCProtoNm, ObjCInstanceVarBlock, ObjCVisSpec,
  ObjCPropDecl, ObjCSel, ObjCMethodSel, ObjCMethodDecl, ObjCProtoDec,
  ObjCProtoDeclBlock, ObjCCatDec,
  ObjCInterface(..), ObjCClassListDef(..), ObjCProtocolName(..),
  ObjCInstanceVariableBlock(..),ObjCVisibilitySpec(..),ObjCVisType(..),
  ObjCPropertyDeclaration(..), ObjCSelector(..), ObjCMethodSelector(..),
  ObjCMethodType(..), ObjCMethodDeclaration(..), ObjCProtocolDec(..),
  ObjCProtocolDeclBlock(..), ObjCCategoryDec(..),
  -- * Declaration attributes
  CDeclSpec, partitionDeclSpecs,
  CStorageSpec, CTypeSpec, isSUEDef, CTypeQual, CAttr,
  CDeclarationSpecifier(..), CStorageSpecifier(..), CTypeSpecifier(..),
  CTypeQualifier(..), CAttribute(..),
  -- * Objective-C extensions
  ObjCPropMod, ObjCProtoQual,
  ObjCPropertyModifier(..), ObjCProtoQualifier(..),
  -- * Declarators
  CDeclr,CDerivedDeclr,CArrSize,
  CDeclarator(..), CDerivedDeclarator(..), CArraySize(..),
  -- ** Objective-C Extensions
  ObjCClassNm, ObjCClassDeclr, ObjCIfaceDecl, ObjCKeywordDecl,
  ObjCClassName(..), ObjCClassDeclarator(..), ObjCInterfaceDeclaration(..),
  ObjCKeywordDeclarator(..),
  -- * Initialization
  CInit, CInitList, CDesignator,
  CInitializer(..), CInitializerList, CPartDesignator(..),
  -- * Statements
  CStat, CBlockItem, CAsmStmt, CAsmOperand,
  CStatement(..), CCompoundBlockItem(..),
  CAssemblyStatement(..), CAssemblyOperand(..),
  -- * Expressions
  CExpr, CExpression(..),
  CAssignOp(..), CBinaryOp(..), CUnaryOp(..),
  CBuiltin, CBuiltinThing(..),
  -- ** Objective-C Extensions
  ObjCMsgExpr, ObjCMsgSel, ObjCKeyArg, ObjCSelName, ObjCSelKeyName,
  ObjCMessageExpression(..), ObjCMessageSelector(..), ObjCKeywordArg(..),
  ObjCSelectorName(..), ObjCSelectorKeyName(..),
  -- * Constants
  CConst, CStrLit, cstringOfLit, liftStrLit,
  CConstant(..), CStringLiteral(..)
) where
import Language.ObjC.Syntax.Constants
import Language.ObjC.Syntax.Ops
import Language.ObjC.Data.Ident
import Language.ObjC.Data.Node
import Language.ObjC.Data.Position
import Data.Generics

-- | Complete C tranlsation unit (C99 6.9, K&R A10)
--
-- A complete C translation unit, for example representing a C header or source file.
-- It consists of a list of external (i.e. toplevel) declarations.
type CTranslUnit = CTranslationUnit NodeInfo
data CTranslationUnit a
  = CTranslUnit [CExternalDeclaration a] a
    deriving (Show, Data, Typeable {-! ,CNode ,Functor, Annotated !-})

-- | External C declaration (C99 6.9, K&R A10)
--
-- Either a toplevel declaration, function definition, class, category,
-- protocol, or external assembler.
type CExtDecl = CExternalDeclaration NodeInfo
data CExternalDeclaration a
  = CDeclExt (CDeclaration a)
  | CFDefExt (CFunctionDef a)
  | ObjCIfaceExt (ObjCInterface a)
  | ObjCClassExt (ObjCClassListDef a)
  | ObjCProtoExt (ObjCProtocolDec a)
  | ObjCCatExt (ObjCCategoryDec a)
  | CAsmExt  (CStringLiteral a) a
    deriving (Show, Data,Typeable {-! ,CNode ,Functor, Annotated !-})

-- | C function definition (C99 6.9.1, K&R A10.1)
--
-- A function definition is of the form @CFunDef specifiers declarator decllist? stmt@.
--
-- * @specifiers@ are the type and storage-class specifiers of the function.
--   The only storage-class specifiers allowed are /extern/ and /static/.
--
-- * The @declarator@ must be such that the declared identifier has /function type/.
--   The return type shall be void or an object type other than array type.
--
-- * The optional declaration list @decllist@ is for old-style function declarations.
--
-- * The statement @stmt@ is a compound statement.
type CFunDef = CFunctionDef NodeInfo
data CFunctionDef a
  = CFunDef
    [CDeclarationSpecifier a] -- type specifier and qualifier
    (CDeclarator a)           -- declarator
    [CDeclaration a]          -- optional declaration list
    (CStatement a)            -- compound statement
    a
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})

type ObjCCatDec = ObjCCategoryDec NodeInfo

-- | Objective-C Category definition (declaration)
data ObjCCategoryDec a =
  ObjCCatDec
    Ident
    Ident
    [ObjCProtocolName a]      -- protocol reference list
    [ObjCInterfaceDeclaration a]  -- interface declaration list
    [CAttribute a]
    a
  deriving (Show, Data, Typeable, Functor {-! ,CNode ,Annotated !-})

type ObjCClassDef = ObjCClassListDef NodeInfo

-- | Objective-C class listing
-- 
-- @class NSObject, NSArray;
data ObjCClassListDef a =
  ObjCClassDef [ObjCClassDeclarator a] a
  deriving (Show, Data, Typeable {-! , CNode, Functor, Annotated !-})

type ObjCProtoDec = ObjCProtocolDec NodeInfo

-- | Objective-C protocol declaration
data ObjCProtocolDec a =
    ObjCForwardProtoDec [Ident] [CAttribute a] a
  | ObjCProtoDec Ident
                 [ObjCProtocolName a]
                 [ObjCProtocolDeclBlock a]
                 [CAttribute a]
                 a
  deriving (Show, Data, Typeable {-! , CNode, Functor, Annotated !-})

type ObjCProtoDeclBlock = ObjCProtocolDeclBlock NodeInfo

data ObjCProtocolDeclBlock a =
    ObjCProtoDeclBlock [ObjCInterfaceDeclaration a] a
  | ObjCReqProtoBlock  [ObjCInterfaceDeclaration a] a
  | ObjCOptProtoBlock  [ObjCInterfaceDeclaration a] a
  deriving (Show, Data, Typeable {-! , CNode, Functor, Annotated !-})

type ObjCIface = ObjCInterface NodeInfo

-- | Interface declaration
data ObjCInterface a =
  ObjCIface
    (ObjCClassDeclarator a)   -- class name
    (Maybe (ObjCClassName a)) -- superclass
    [ObjCProtocolName a]      --  protocol reference list
    [ObjCInstanceVariableBlock a] -- instance variables
    [ObjCInterfaceDeclaration a]  -- interface declaration list
    [CAttribute a]                -- optional attributes
    a
  deriving (Show, Data, Typeable, Functor {-! ,CNode ,Annotated !-})

type ObjCClassDeclr = ObjCClassDeclarator NodeInfo

data ObjCClassDeclarator a =
  ObjCClassDeclr Ident a
 deriving (Show, Data, Typeable, Functor {-! ,CNode, Annotated !-})

type ObjCClassNm = ObjCClassName NodeInfo

data ObjCClassName a = ObjCClassNm Ident a
 deriving (Show, Data, Typeable, Functor {-! ,CNode, Annotated !-})

type ObjCProtoNm = ObjCProtocolName NodeInfo

data ObjCProtocolName a = ObjCProtoNm Ident a
  deriving (Show, Data, Typeable, Functor {-! ,CNode ,Annotated !-})

type ObjCInstanceVarBlock = ObjCInstanceVariableBlock NodeInfo

data ObjCInstanceVariableBlock a =
  ObjCInstanceVarBlock
  (Maybe (ObjCVisibilitySpec a))
  [CDeclaration a]
  a
  deriving (Show, Data, Typeable, Functor {-! ,CNode ,Annotated !-})

type ObjCVisSpec = ObjCVisibilitySpec NodeInfo

data ObjCVisibilitySpec a =
  ObjCVisSpec ObjCVisType a
  deriving (Show, Data, Typeable, Functor {-! ,CNode ,Annotated !-})

-- | Available visibility specifications.
data ObjCVisType =
    ObjCPrivVis
  | ObjCProtVis
  | ObjCPubVis
  | ObjCPackageVis
  deriving (Show, Data, Typeable, Enum)
 
type ObjCIfaceDecl = ObjCInterfaceDeclaration NodeInfo

data ObjCInterfaceDeclaration a =
    ObjCIfaceDecl (CDeclaration a) a
  | ObjCIfaceMethodDecl (ObjCMethodDeclaration a) a
  | ObjCIfacePropDecl   (ObjCPropertyDeclaration a) a
  deriving (Show, Data, Typeable, Functor {-! ,CNode ,Annotated !-})

type ObjCMethodDecl = ObjCMethodDeclaration NodeInfo

data ObjCMethodDeclaration a =
    ObjCMethodDecl
    ObjCMethodType
    (Maybe (CDeclaration a))   -- ^ type_name (as with CCast)
    (ObjCMethodSelector a)
    [CAttribute a]
    a
  deriving (Show, Data, Typeable, Functor {-! ,CNode ,Annotated !-})

data ObjCMethodType = ObjCClassMethod | ObjCInstanceMethod
  deriving (Show, Data, Typeable, Enum)

type ObjCMethodSel = ObjCMethodSelector NodeInfo

data ObjCMethodSelector a =
    ObjCUnaryMethod (ObjCSelector a) a
  | ObjCMethod [ObjCKeywordDeclarator a] (Maybe ([CDeclaration a],Bool)) a
  | ObjCEllipseMethod [ObjCKeywordDeclarator a] a
  deriving (Show, Data, Typeable, Functor {-! ,CNode ,Annotated !-})

type ObjCKeywordDecl = ObjCKeywordDeclarator NodeInfo

data ObjCKeywordDeclarator a =
   ObjCKeywordDecl
   (Maybe (ObjCSelector a)) -- ^ selector
   (Maybe (CDeclaration a)) -- ^ type name
   Ident
   a
  deriving (Show, Data, Typeable, Functor {-! ,CNode ,Annotated !-})

type ObjCPropDecl = ObjCPropertyDeclaration NodeInfo

data ObjCPropertyDeclaration a =
   ObjCPropDecl
   [ObjCPropertyModifier a]
   (CDeclaration a)
   a
  deriving (Show, Data, Typeable, Functor {-! ,CNode ,Annotated !-})

type ObjCPropMod = ObjCPropertyModifier NodeInfo

data ObjCPropertyModifier a =
    ObjCPropMod Ident (Maybe Ident) a
  deriving (Show, Data, Typeable, Functor {-! ,CNode ,Annotated !-})


-- | C declarations (K&R A8, C99 6.7), including structure declarations, parameter
--   declarations and type names.
--
-- A declaration is of the form @CDecl specifiers init-declarator-list@, where the form of the declarator list's
--  elements depends on the kind of declaration:
--
-- 1) Toplevel declarations (K&R A8, C99 6.7 declaration)
--
--   * C99 requires that there is at least one specifier, though this is merely a syntactic restriction
--
--   * at most one storage class specifier is allowed per declaration
--
--   * the elements of the non-empty @init-declarator-list@ are of the form @(Just declr, init?, Nothing)@.
--      The declarator @declr@ has to be present and non-abstract and the initialization expression is
--      optional.
--
-- 2) Structure declarations (K&R A8.3, C99 6.7.2.1 struct-declaration)
--
--   Those are the declarations of a structure's members.
--
--   * do not allow storage specifiers
--
--   * in strict C99, the list of declarators has to be non-empty
--
--   * the elements of @init-declarator-list@ are either of the form @(Just declr, Nothing, size?)@,
--     representing a member with optional bit-field size, or of the form @(Nothing, Nothing, Just size)@,
--     for unnamed bitfields. @declr@ has to be non-abstract.
--
--   * no member of a structure shall have incomplete type
--
-- 3) Parameter declarations (K&R A8.6.3, C99 6.7.5 parameter-declaration)
--
--   * @init-declarator-list@ must contain at most one triple of the form @(Just declr, Nothing, Nothing)@,
--     i.e. consist of a single declarator, which is allowed to be abstract (i.e. unnamed).
--
-- 4) Type names (A8.8, C99 6.7.6)
--
--   * do not allow storage specifiers
--
--   * @init-declarator-list@ must contain at most one triple of the form @(Just declr, Nothing, Nothing)@.
--     where @declr@ is an abstract declarator (i.e. doesn't contain a declared identifier)
--
type CDecl = CDeclaration NodeInfo
data CDeclaration a
  = CDecl
    [CDeclarationSpecifier a] -- type specifier and qualifier, __attribute__
    [(Maybe (CDeclarator a),  -- declarator (may be omitted)
      Maybe (CInitializer a), -- optional initialize
      Maybe (CExpression a))] -- optional size (const expr)
    a
    deriving (Show, Data,Typeable {-! ,CNode ,Annotated !-})

-- Derive instance is a little bit ugly
instance Functor CDeclaration where
  fmap f (CDecl specs declarators annot) =
    CDecl (map (fmap f) specs) (map fmap3m declarators) (f annot)
      where fmap3m (a,b,c) = (fmap (fmap f) a, fmap (fmap f) b, fmap (fmap f) c)

-- | C declarator (K&R A8.5, C99 6.7.5) and abstract declarator (K&R A8.8, C99 6.7.6)
--
-- A declarator declares a single object, function, or type. It is always associated with
-- a declaration ('CDecl'), which specifies the declaration's type and the additional storage qualifiers and
-- attributes, which apply to the declared object.
--
-- A declarator is of the form @CDeclr name? indirections asm-name? attrs _@, where
-- @name@ is the name of the declared object (missing for abstract declarators),
-- @declquals@ is a set of additional declaration specifiers,
-- @asm-name@ is the optional assembler name and attributes is a set of
-- attrs is a set of @__attribute__@ annotations for the declared object.
--
-- @indirections@ is a set of pointer, array and function declarators, which modify the type of the declared object as
-- described below. If the /declaration/ specifies the non-derived type @T@,
-- and we have @indirections = [D1, D2, ..., Dn]@ than the declared object has type
-- @(D1 `indirect` (D2 `indirect` ...  (Dn `indirect` T)))@, where
--
--  * @(CPtrDeclr attrs) `indirect` T@ is /attributed pointer to T/
--
--  * @(CFunDeclr attrs) `indirect` T@ is /attributed function returning T/
--
--  * @(CArrayDeclr attrs) `indirect` T@ is /attributed array of elemements of type T/
--
-- Examples (simplified attributes):
--
--  * /x/ is an int
--
-- > int x;
-- > CDeclr "x" []
--
--  * /x/ is a restrict pointer to a const pointer to int
--
-- > const int * const * restrict x;
-- > CDeclr "x" [CPtrDeclr [restrict], CPtrDeclr [const]]
--
--  * /f/ is an function return a constant pointer to int
--
-- > int* const f();
-- > CDeclr "f" [CFunDeclr [],CPtrDeclr [const]]
--
--  * /f/ is a constant pointer to a function returning int
--
-- > int (* const f)(); ==>
-- > CDeclr "f" [CPtrDeclr [const], CFunDeclr []]
type CDeclr = CDeclarator NodeInfo
data CDeclarator a
  = CDeclr (Maybe Ident) [CDerivedDeclarator a] (Maybe (CStringLiteral a)) [CAttribute a] a
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


-- | Derived declarators, see 'CDeclr'
--
-- Indirections are qualified using type-qualifiers and generic attributes, and additionally
--
--    * The size of an array is either a constant expression, variable length ('*') or missing; in the last case, the
--      type of the array is incomplete. The qualifier static is allowed for function arguments only, indicating that
--      the supplied argument is an array of at least the given size.
--
--    * New style parameter lists have the form @Right (declarations, isVariadic)@, old style parameter lists have the
--      form @Left (parameter-names)@
type CDerivedDeclr = CDerivedDeclarator NodeInfo
data CDerivedDeclarator a
  = CPtrDeclr [CTypeQualifier a] a
  -- ^ Pointer declarator @CPtrDeclr tyquals declr@
  | CBlkDeclr [CTypeQualifier a] a
  -- ^ Block declarator @CBlkDeclr tyquals declr@
  | CArrDeclr [CTypeQualifier a] (CArraySize a) a
  -- ^ Array declarator @CArrDeclr declr tyquals size-expr?@
  | CFunDeclr (Either [Ident] ([CDeclaration a],Bool)) [CAttribute a] a
    -- ^ Function declarator @CFunDeclr declr (old-style-params | new-style-params) c-attrs@
    deriving (Show, Data,Typeable {-! ,CNode , Annotated !-})

-- Derived instance relies on fmap2
instance Functor CDerivedDeclarator where
        fmap _f (CPtrDeclr a1 a2) = CPtrDeclr (fmap (fmap _f) a1) (_f a2)
        fmap _f (CBlkDeclr a1 a2) = CBlkDeclr (fmap (fmap _f) a1) (_f a2)
        fmap _f (CArrDeclr a1 a2 a3)
          = CArrDeclr (fmap (fmap _f) a1) (fmap _f a2) (_f a3)
        fmap _f (CFunDeclr a1 a2 a3)
          = CFunDeclr (fmap (fmapFirst (fmap (fmap _f))) a1) (fmap (fmap _f) a2)
              (_f a3)
          where fmapFirst f (a,b) = (f a, b)

-- | Size of an array
type CArrSize = CArraySize NodeInfo
data CArraySize a
  = CNoArrSize Bool               -- ^ @CUnknownSize isCompleteType@
  | CArrSize Bool (CExpression a) -- ^ @CArrSize isStatic expr@
    deriving (Show, Data,Typeable {-! , Functor !-})


-- | C statement (K&R A9, C99 6.8)
--
type CStat = CStatement NodeInfo
data CStatement a
  -- | An (attributed) label followed by a statement
  = CLabel  Ident (CStatement a) [CAttribute a] a
  -- | A statement of the form @case expr : stmt@
  | CCase (CExpression a) (CStatement a) a
  -- | A case range of the form @case lower ... upper : stmt@
  | CCases (CExpression a) (CExpression a) (CStatement a) a
  -- | The default case @default : stmt@
  | CDefault (CStatement a) a
  -- | A simple statement, that is in C: evaluating an expression with
  --   side-effects and discarding the result.
  | CExpr (Maybe (CExpression a)) a
  -- | compound statement @CCompound localLabels blockItems at@
  | CCompound [Ident] [CCompoundBlockItem a] a
  -- | conditional statement @CIf ifExpr thenStmt maybeElseStmt at@
  | CIf (CExpression a) (CStatement a) (Maybe (CStatement a)) a
  -- | switch statement @CSwitch selectorExpr switchStmt@, where
  -- @switchStmt@ usually includes /case/, /break/ and /default/
  -- statements
  | CSwitch (CExpression a) (CStatement a) a
  -- | while or do-while statement @CWhile guard stmt isDoWhile at@
  | CWhile (CExpression a) (CStatement a) Bool a
  -- | for statement @CFor init expr-2 expr-3 stmt@, where @init@ is
  -- either a declaration or initializing expression
  | CFor (Either (Maybe (CExpression a)) (CDeclaration a))
    (Maybe (CExpression a))
    (Maybe (CExpression a))
    (CStatement a)
    a
  -- | goto statement @CGoto label@
  | CGoto Ident a
  -- | computed goto @CGotoPtr labelExpr@
  | CGotoPtr (CExpression a) a
  -- | continue statement
  | CCont a
  -- | break statement
  | CBreak a
  -- | return statement @CReturn returnExpr@
  | CReturn (Maybe (CExpression a)) a
  -- | assembly statement
  | CAsm (CAssemblyStatement a) a
    deriving (Show, Data,Typeable {-! , CNode , Annotated !-})

-- Derived instance relies on fmap2 :(
instance Functor CStatement where
        fmap _f (CLabel a1 a2 a3 a4)
          = CLabel a1 (fmap _f a2) (fmap (fmap _f) a3) (_f a4)
        fmap _f (CCase a1 a2 a3) = CCase (fmap _f a1) (fmap _f a2) (_f a3)
        fmap _f (CCases a1 a2 a3 a4)
          = CCases (fmap _f a1) (fmap _f a2) (fmap _f a3) (_f a4)
        fmap _f (CDefault a1 a2) = CDefault (fmap _f a1) (_f a2)
        fmap _f (CExpr a1 a2) = CExpr (fmap (fmap _f) a1) (_f a2)
        fmap _f (CCompound a1 a2 a3)
          = CCompound a1 (fmap (fmap _f) a2) (_f a3)
        fmap _f (CIf a1 a2 a3 a4)
          = CIf (fmap _f a1) (fmap _f a2) (fmap (fmap _f) a3) (_f a4)
        fmap _f (CSwitch a1 a2 a3)
          = CSwitch (fmap _f a1) (fmap _f a2) (_f a3)
        fmap _f (CWhile a1 a2 a3 a4)
          = CWhile (fmap _f a1) (fmap _f a2) a3 (_f a4)
        fmap _f (CFor a1 a2 a3 a4 a5)
          = CFor (mapEither (fmap (fmap _f)) (fmap _f) a1)
                 (fmap (fmap _f) a2) (fmap (fmap _f) a3) (fmap _f a4)
                 (_f a5)
          where mapEither f1 f2 = either (Left . f1) (Right . f2)
        fmap _f (CGoto a1 a2) = CGoto a1 (_f a2)
        fmap _f (CGotoPtr a1 a2) = CGotoPtr (fmap _f a1) (_f a2)
        fmap _f (CCont a1) = CCont (_f a1)
        fmap _f (CBreak a1) = CBreak (_f a1)
        fmap _f (CReturn a1 a2) = CReturn (fmap (fmap _f) a1) (_f a2)
        fmap _f (CAsm a1 a2) = CAsm (fmap _f a1) (_f a2)

-- | GNU Assembler statement
--
-- > CAssemblyStatement type-qual? asm-expr out-ops in-ops clobbers _
--
-- is an inline assembler statement.
-- The only type-qualifier (if any) allowed is /volatile/.
-- @asm-expr@ is the actual assembler epxression (a string), @out-ops@ and @in-ops@ are the input
-- and output operands of the statement.
-- @clobbers@ is a list of registers which are clobbered when executing the assembler statement
type CAsmStmt = CAssemblyStatement NodeInfo
data CAssemblyStatement a
  = CAsmStmt
    (Maybe (CTypeQualifier a)) -- maybe volatile
    (CStringLiteral a)         -- assembler expression (String)
    [CAssemblyOperand a]       -- output operands
    [CAssemblyOperand a]       -- input operands
    [CStringLiteral a]         -- Clobbers
    a
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})

-- | Assembler operand
--
-- @CAsmOperand argName? constraintExpr arg@ specifies an operand for an assembler
-- statement.
type CAsmOperand = CAssemblyOperand NodeInfo
data CAssemblyOperand a
  = CAsmOperand
    (Maybe Ident)       -- argument name
    (CStringLiteral a)  -- constraint expr
    (CExpression a)     -- argument
    a
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})

-- | C99 Block items
--
--  Things that may appear in compound statements: either statements, declarations
--   or nested function definitions.
type CBlockItem = CCompoundBlockItem NodeInfo
data CCompoundBlockItem a
  = CBlockStmt    (CStatement a)    -- ^ A statement
  | CBlockDecl    (CDeclaration a)  -- ^ A local declaration
  | CNestedFunDef (CFunctionDef a)  -- ^ A nested function (GNU C)
    deriving (Show, Data,Typeable {-! , CNode , Functor, Annotated !-})

-- | C declaration specifiers and qualifiers
--
-- Declaration specifiers include at most one storage-class specifier (C99 6.7.1),
-- type specifiers (6.7.2) and type qualifiers (6.7.3).
type CDeclSpec = CDeclarationSpecifier NodeInfo
data CDeclarationSpecifier a
  = CStorageSpec (CStorageSpecifier a) -- ^ storage-class specifier or typedef
  | CTypeSpec    (CTypeSpecifier a)    -- ^ type name
  | CTypeQual    (CTypeQualifier a)    -- ^ type qualifier
    deriving (Show, Data,Typeable {-! ,CNode ,Functor, Annotated !-})


-- | Separate the declaration specifiers
--
-- Note that inline isn't actually a type qualifier, but a function specifier.
-- @__attribute__@ of a declaration qualify declarations or declarators (but not types),
-- and are therefore separated as well.
partitionDeclSpecs :: [CDeclarationSpecifier a]
                   -> ( [CStorageSpecifier a], [CAttribute a]
                      , [CTypeQualifier a], [CTypeSpecifier a], Bool)
partitionDeclSpecs = foldr deals ([],[],[],[],False) where
    deals (CTypeQual (CInlineQual _)) (sts,ats,tqs,tss,_) = (sts,ats,tqs,tss,True)
    deals (CStorageSpec sp) (sts,ats,tqs,tss,inline)  = (sp:sts,ats,tqs,tss,inline)
    deals (CTypeQual (CAttrQual attr)) (sts,ats,tqs,tss,inline)  = (sts,attr:ats,tqs,tss,inline)
    deals (CTypeQual tq) (sts,ats,tqs,tss,inline)     = (sts,ats,tq:tqs,tss,inline)
    deals (CTypeSpec ts) (sts,ats,tqs,tss,inline)     = (sts,ats,tqs,ts:tss,inline)

-- | C storage class specifier (and typedefs) (K&R A8.1, C99 6.7.1)
type CStorageSpec = CStorageSpecifier NodeInfo
data CStorageSpecifier a
  = CAuto     a     -- ^ auto
  | CRegister a     -- ^ register
  | CStatic   a     -- ^ static
  | CExtern   a     -- ^ extern
  | CTypedef  a     -- ^ typedef
  | CThread   a     -- ^ GNUC thread local storage
    deriving (Show, Eq,Ord,Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


-- | C type specifier (K&R A8.2, C99 6.7.2)
--
-- Type specifiers are either basic types such as @char@ or @int@,
-- @struct@, @union@ or @enum@ specifiers or typedef names.
--
-- As a GNU extension, a @typeof@ expression also is a type specifier.
-- 
-- Objective-C class names and specifiers of the form 'typdefname <Protocols>'
-- are also allowed.
type CTypeSpec = CTypeSpecifier NodeInfo
data CTypeSpecifier a
  = CVoidType    a
  | CCharType    a
  | CShortType   a
  | CIntType     a
  | CLongType    a
  | CFloatType   a
  | CDoubleType  a
  | CSignedType  a
  | CUnsigType   a
  | CBoolType    a
  | CComplexType a
  | CSUType      (CStructureUnion a) a      -- ^ Struct or Union specifier
  | CEnumType    (CEnumeration a)    a      -- ^ Enumeration specifier
  | CTypeDef     Ident        a      -- ^ Typedef name
  | CTypeOfExpr  (CExpression a)  a  -- ^ @typeof(expr)@
  | CTypeOfType  (CDeclaration a) a  -- ^ @typeof(type)@
  | ObjCClassProto Ident [ObjCProtocolName a] a      -- ^ class name with protocol list
  | ObjCTypeProto  Ident [ObjCProtocolName a] a      -- ^ Typedef name with protocol list
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


-- | returns @True@ if the given typespec is a struct, union or enum /definition/
isSUEDef :: CTypeSpecifier a -> Bool
isSUEDef (CSUType (CStruct _ _ (Just _) _ _) _) = True
isSUEDef (CEnumType (CEnum _ (Just _) _ _) _) = True
isSUEDef _ = False

-- | C type qualifiers (K&R A8.2, C99 6.7.3), function specifiers (C99 6.7.4), and attributes.
--
-- @const@, @volatile@ and @restrict@ type qualifiers and @inline@ function specifier.
-- Additionally, @__attribute__@ annotations for declarations and declarators.
type CTypeQual = CTypeQualifier NodeInfo
data CTypeQualifier a
  = CConstQual a
  | CVolatQual a
  | CRestrQual a
  | CInlineQual a
  | CAttrQual  (CAttribute a)
  | ObjCProtoQual (ObjCProtoQualifier a)
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})

type ObjCProtoQual = ObjCProtoQualifier NodeInfo

data ObjCProtoQualifier a =
    ObjCInQual a
  | ObjCOutQual a
  | ObjCInOutQual a
  | ObjCBycopyQual a
  | ObjCOnewayQual a
    deriving (Show, Data, Typeable, Functor {-! , CNode, Annotated !-})

-- | C structure or union specifiers (K&R A8.3, C99 6.7.2.1)
--
-- @CStruct tag identifier struct-decls c-attrs@ represents a struct or union specifier (depending on @tag@).
--
--   * either @identifier@ or the declaration list @struct-decls@ (or both) have to be present.
--
--     Example: in @struct foo x;@, the identifier is present, in @struct { int y; } x@ the declaration list, and
--     in @struct foo { int y; } x;@ both of them.
--
--   * @c-attrs@ is a list of @__attribute__@ annotations associated with the struct or union specifier
type CStructUnion = CStructureUnion NodeInfo
data CStructureUnion a
  = CStruct
    CStructTag
    (Maybe Ident)
    (Maybe [CDeclaration a])  -- member declarations
    [CAttribute a]            -- __attribute__s
    a
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


-- | A tag to determine wheter we refer to a @struct@ or @union@, see 'CStructUnion'.
data CStructTag = CStructTag
                | CUnionTag
                deriving (Show, Eq,Data,Typeable)


-- | C enumeration specifier (K&R A8.4, C99 6.7.2.2)
--
-- @CEnum identifier enumerator-list attrs@ represent as enum specifier
--
--  * Either the identifier or the enumerator-list (or both) have to be present.
--
--  * If @enumerator-list@ is present, it has to be non-empty.
--
--  * The enumerator list is of the form @(enumeration-constant, enumeration-value?)@, where the latter
--    is an optional constant integral expression.
--
--  * @attrs@ is a list of @__attribute__@ annotations associated with the enumeration specifier
type CEnum = CEnumeration NodeInfo
data CEnumeration a
  = CEnum
    (Maybe Ident)
    (Maybe [(Ident,                   -- variant name
             Maybe (CExpression a))]) -- explicit variant value
    [CAttribute a]                    -- __attribute__s
    a
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})



-- | C initialization (K&R A8.7, C99 6.7.8)
--
-- Initializers are either assignment expressions or initializer lists
-- (surrounded in curly braces), whose elements are themselves
-- initializers, paired with an optional list of designators.
type CInit = CInitializer NodeInfo
data CInitializer a
  -- | assignment expression
  = CInitExpr (CExpression a) a
  -- | initialization list (see 'CInitList')
  | CInitList (CInitializerList a) a
    deriving (Show, Data,Typeable {-! ,CNode , Annotated !-})

-- deriving Functor does not work (type synonym)
instance Functor CInitializer where
        fmap _f (CInitExpr a1 a2) = CInitExpr (fmap _f a1) (_f a2)
        fmap _f (CInitList a1 a2) = CInitList (fmapInitList _f a1) (_f a2)
fmapInitList :: (a->b) -> (CInitializerList a) -> (CInitializerList b)
fmapInitList _f = map (\(desigs, initializer) -> (fmap (fmap _f) desigs, fmap _f initializer))

-- | Initializer List
--
-- The members of an initializer list are of the form @(designator-list,initializer)@.
-- The @designator-list@ specifies one member of the compound type which is initialized.
-- It is allowed to be empty - in this case the initializer refers to the
-- ''next'' member of the compound type (see C99 6.7.8).
--
-- Examples (simplified expressions and identifiers):
--
-- > -- int x[3][4] = { [0][3] = 4, [2] = 5, 8 };
-- > --   corresponds to the assignments
-- > -- x[0][3] = 4; x[2][0] = 5; x[2][1] = 8;
-- > let init1 = ([CArrDesig 0, CArrDesig 3], CInitExpr 4)
-- >     init2 = ([CArrDesig 2]             , CInitExpr 5)
-- >     init3 = ([]                        , CInitExpr 8)
-- > in  CInitList [init1, init2, init3]
--
-- > -- struct { struct { int a[2]; int b[2]; int c[2]; } s; } x = { .s = { {2,3} , .c[0] = 1 } };
-- > --   corresponds to the assignments
-- > -- x.s.a[0] = 2; x.s.a[1] = 3; x.s.c[0] = 1;
-- > let init_s_0 = CInitList [ ([], CInitExpr 2), ([], CInitExpr 3)]
-- >     init_s   = CInitList [
-- >                            ([], init_s_0),
-- >                            ([CMemberDesig "c", CArrDesig 0], CInitExpr 1)
-- >                          ]
-- > in  CInitList [(CMemberDesig "s", init_s)]
type CInitList = CInitializerList NodeInfo
type CInitializerList a = [([CPartDesignator a], CInitializer a)]

-- | Designators
--
-- A designator specifies a member of an object, either an element or range of an array,
-- or the named member of a struct \/ union.
type CDesignator = CPartDesignator NodeInfo
data CPartDesignator a
  -- | array position designator
  = CArrDesig     (CExpression a) a
  -- | member designator
  | CMemberDesig  Ident a
  -- | array range designator @CRangeDesig from to _@ (GNU C)
  | CRangeDesig (CExpression a) (CExpression a) a
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


-- | @__attribute__@ annotations
--
-- Those are of the form @CAttr attribute-name attribute-parameters@,
-- and serve as generic properties of some syntax tree elements.
type CAttr = CAttribute NodeInfo
data CAttribute a = CAttr Ident [CExpression a] a
                    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


-- | C expression (K&R A7)
--
-- * these can be arbitrary expression, as the argument of `sizeof' can be
--   arbitrary, even if appearing in a constant expression
--
-- * GNU C extensions: @alignof@, @__real@, @__imag@, @({ stmt-expr })@, @&& label@ and built-ins
-- * objective-C style code blocks
--
type CExpr = CExpression NodeInfo
data CExpression a
  = CComma       [CExpression a]         -- comma expression list, n >= 2
                 a
  | CAssign      CAssignOp               -- assignment operator
                 (CExpression a)         -- l-value
                 (CExpression a)         -- r-value
                 a
  | CCond        (CExpression a)         -- conditional
                 (Maybe (CExpression a)) -- true-expression (GNU allows omitting)
                 (CExpression a)         -- false-expression
                 a
  | CBinary      CBinaryOp               -- binary operator
                 (CExpression a)         -- lhs
                 (CExpression a)         -- rhs
                 a
  | CCast        (CDeclaration a)        -- type name
                 (CExpression a)
                 a
  | CUnary       CUnaryOp                -- unary operator
                 (CExpression a)
                 a
  | CSizeofExpr  (CExpression a)
                 a
  | CSizeofType  (CDeclaration a)        -- type name
                 a
  | CAlignofExpr (CExpression a)
                 a
  | CAlignofType (CDeclaration a)        -- type name
                 a
  | CComplexReal (CExpression a)         -- real part of complex number
                 a
  | CComplexImag (CExpression a)         -- imaginary part of complex number
                 a
  | CIndex       (CExpression a)         -- array
                 (CExpression a)         -- index
                 a
  | CCall        (CExpression a)         -- function
                 [CExpression a]         -- arguments
                 a
  | CMember      (CExpression a)         -- structure
                 Ident                   -- member name
                 Bool                    -- deref structure? (True for `->')
                 a
  | CVar         Ident                   -- identifier (incl. enumeration const)
                 a
  | CConst       (CConstant a)           -- ^ integer, character, floating point and string constants
  | CCompoundLit (CDeclaration a)
                 (CInitializerList a)    -- type name & initialiser list
                 a                       -- ^ C99 compound literal
  | CStatExpr    (CStatement a) a        -- ^ GNU C compound statement as expr
  | CLabAddrExpr Ident a                 -- ^ GNU C address of label
  | CBuiltinExpr (CBuiltinThing a)       -- ^ builtin expressions, see 'CBuiltin'
  -- objective-c additions
  | CBlockExpr   ([CDeclaration a],Bool) (CStatement a) a -- ^ Code block definition, new-style params, compound statement
  | ObjCMessageExpr  (ObjCMessageExpression a) a -- ^ Obj-c message
  | ObjCSelectorExpr (ObjCSelectorName a) a      -- ^ selector name
  | ObjCProtoExpr Ident a                        -- ^ @protocol expression
  | ObjCEncodeExpr (CDeclaration a) a       -- ^ @encode expression (parse cdecls as CCastExpr)
    deriving (Data,Typeable,Show, Functor {-! ,CNode , Annotated !-})

type ObjCSelName = ObjCSelectorName NodeInfo

data ObjCSelectorName a =
    ObjCSelPlain (ObjCSelector a) a
  | ObjCSelKeys [ObjCSelectorKeyName a] a
    deriving (Data,Typeable,Show, Functor {-! ,CNode , Annotated !-})

type ObjCSelKeyName = ObjCSelectorKeyName NodeInfo

data ObjCSelectorKeyName a =
    ObjCSelKeyName (Maybe (ObjCSelector a)) a
    deriving (Data,Typeable,Show, Functor {-! ,CNode , Annotated !-})

type ObjCSel = ObjCSelector NodeInfo

data ObjCSelector a =
    ObjCSel Ident a
  | ObjCInSel a
  | ObjCOutSel a
    deriving (Data,Typeable,Show, Functor {-! ,CNode , Annotated !-})

type ObjCMsgExpr = ObjCMessageExpression NodeInfo

data ObjCMessageExpression a =
    ObjCMsgExpr (CExpression a)    (ObjCMessageSelector a) a
  | ObjCMsgClass (ObjCClassName a) (ObjCMessageSelector a) a
  | ObjCMsgSup                     (ObjCMessageSelector a) a
    deriving (Data,Typeable,Show, Functor {-! ,CNode , Annotated !-})

type ObjCMsgSel = ObjCMessageSelector NodeInfo

data ObjCMessageSelector a =
   ObjCMsgSel (ObjCSelector a) a
 | ObjCKeyArgs [ObjCKeywordArg a] a
    deriving (Data,Typeable,Show, Functor {-! ,CNode , Annotated !-})

type ObjCKeyArg = ObjCKeywordArg NodeInfo

data ObjCKeywordArg a =
    ObjCKeyArg (ObjCSelectorKeyName a) (CExpression a) a
    deriving (Data,Typeable,Show, Functor {-! ,CNode , Annotated !-})

-- | GNU Builtins, which cannot be typed in C99
type CBuiltin = CBuiltinThing NodeInfo
data CBuiltinThing a
  = CBuiltinVaArg (CExpression a) (CDeclaration a) a            -- ^ @(expr, type)@
  | CBuiltinOffsetOf (CDeclaration a) [CPartDesignator a] a -- ^ @(type, designator-list)@
  | CBuiltinTypesCompatible (CDeclaration a) (CDeclaration a) a  -- ^ @(type,type)@
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


-- | C constant (K&R A2.5 & A7.2)
type CConst = CConstant NodeInfo
data CConstant a
  = CIntConst   CInteger a
  | CCharConst  CChar a
  | CFloatConst CFloat a
  | CStrConst   CString a
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


-- | Attributed string literals
type CStrLit = CStringLiteral NodeInfo
data CStringLiteral a = CStrLit CString a
            deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


cstringOfLit :: CStringLiteral a -> CString
cstringOfLit (CStrLit cstr _) = cstr

-- | Lift a string literal to a C constant
liftStrLit :: CStringLiteral a -> CConstant a
liftStrLit (CStrLit str at) = CStrConst str at

-- | All AST nodes are annotated. Inspired by the Annotated
-- class of Niklas Broberg's haskell-src-exts package.
-- In principle, we could have Copointed superclass instead
-- of @ann@, for the price of another dependency.
class (Functor ast) => Annotated ast where
  -- | get the annotation of an AST node
  annotation :: ast a -> a
  -- | change the annotation (non-recursively)
  --   of an AST node. Use fmap for recursively
  --   modifying the annotation.
  amap  :: (a->a) -> ast a -> ast a

-- fmap2 :: (a->a') -> (a,b) -> (a',b)
-- fmap2 f (a,b) = (f a, b)

-- Instances generated using derive-2.*
-- GENERATED START

 
instance (CNode t1) => CNode (CTranslationUnit t1) where
        nodeInfo (CTranslUnit _ n) = nodeInfo n
 
instance (CNode t1) => Pos (CTranslationUnit t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor CTranslationUnit where
        fmap _f (CTranslUnit a1 a2)
          = CTranslUnit (fmap (fmap _f) a1) (_f a2)

 
instance Annotated CTranslationUnit where
        annotation (CTranslUnit _ n) = n
        amap f (CTranslUnit a_1 a_2) = CTranslUnit a_1 (f a_2)

 
instance (CNode t1) => CNode (CExternalDeclaration t1) where
        nodeInfo (CDeclExt d) = nodeInfo d
        nodeInfo (CFDefExt d) = nodeInfo d
        nodeInfo (ObjCIfaceExt d) = nodeInfo d
        nodeInfo (ObjCClassExt d) = nodeInfo d
        nodeInfo (ObjCProtoExt d) = nodeInfo d
        nodeInfo (ObjCCatExt d) = nodeInfo d
        nodeInfo (CAsmExt _ n) = nodeInfo n
 
instance (CNode t1) => Pos (CExternalDeclaration t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor CExternalDeclaration where
        fmap _f (CDeclExt a1) = CDeclExt (fmap _f a1)
        fmap _f (CFDefExt a1) = CFDefExt (fmap _f a1)
        fmap _f (ObjCIfaceExt a1) = ObjCIfaceExt (fmap _f a1)
        fmap _f (ObjCClassExt a1) = ObjCClassExt (fmap _f a1)
        fmap _f (ObjCProtoExt a1) = ObjCProtoExt (fmap _f a1)
        fmap _f (ObjCCatExt a1) = ObjCCatExt (fmap _f a1)
        fmap _f (CAsmExt a1 a2) = CAsmExt (fmap _f a1) (_f a2)

 
instance Annotated CExternalDeclaration where
        annotation (CDeclExt n) = annotation n
        annotation (CFDefExt n) = annotation n
        annotation (ObjCIfaceExt n) = annotation n
        annotation (ObjCClassExt n) = annotation n
        annotation (ObjCProtoExt n) = annotation n
        annotation (ObjCCatExt n) = annotation n
        annotation (CAsmExt _ n) = n
        amap f (CDeclExt n) = CDeclExt (amap f n)
        amap f (CFDefExt n) = CFDefExt (amap f n)
        amap f (ObjCIfaceExt n) = ObjCIfaceExt (amap f n)
        amap f (ObjCClassExt n) = ObjCClassExt (amap f n)
        amap f (ObjCProtoExt n) = ObjCProtoExt (amap f n)
        amap f (ObjCCatExt n) = ObjCCatExt (amap f n)
        amap f (CAsmExt a_1 a_2) = CAsmExt a_1 (f a_2)

 
instance (CNode t1) => CNode (CFunctionDef t1) where
        nodeInfo (CFunDef _ _ _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (CFunctionDef t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor CFunctionDef where
        fmap _f (CFunDef a1 a2 a3 a4 a5)
          = CFunDef (fmap (fmap _f) a1) (fmap _f a2) (fmap (fmap _f) a3)
              (fmap _f a4)
              (_f a5)

 
instance Annotated CFunctionDef where
        annotation (CFunDef _ _ _ _ n) = n
        amap f (CFunDef a_1 a_2 a_3 a_4 a_5)
          = CFunDef a_1 a_2 a_3 a_4 (f a_5)

 
instance (CNode t1) => CNode (ObjCCategoryDec t1) where
        nodeInfo (ObjCCatDec _ _ _ _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCCategoryDec t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCCategoryDec where
        annotation (ObjCCatDec _ _ _ _ _ n) = n
        amap f (ObjCCatDec a_1 a_2 a_3 a_4 a_5 a_6)
          = ObjCCatDec a_1 a_2 a_3 a_4 a_5 (f a_6)

 
instance (CNode t1) => CNode (ObjCClassListDef t1) where
        nodeInfo (ObjCClassDef _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCClassListDef t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor ObjCClassListDef where
        fmap _f (ObjCClassDef a1 a2)
          = ObjCClassDef (fmap (fmap _f) a1) (_f a2)

 
instance Annotated ObjCClassListDef where
        annotation (ObjCClassDef _ n) = n
        amap f (ObjCClassDef a_1 a_2) = ObjCClassDef a_1 (f a_2)

 
instance (CNode t1) => CNode (ObjCProtocolDec t1) where
        nodeInfo (ObjCForwardProtoDec _ _ n) = nodeInfo n
        nodeInfo (ObjCProtoDec _ _ _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCProtocolDec t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor ObjCProtocolDec where
        fmap _f (ObjCForwardProtoDec a1 a2 a3)
          = ObjCForwardProtoDec a1 (fmap (fmap _f) a2) (_f a3)
        fmap _f (ObjCProtoDec a1 a2 a3 a4 a5)
          = ObjCProtoDec a1 (fmap (fmap _f) a2) (fmap (fmap _f) a3)
              (fmap (fmap _f) a4)
              (_f a5)

 
instance Annotated ObjCProtocolDec where
        annotation (ObjCForwardProtoDec _ _ n) = n
        annotation (ObjCProtoDec _ _ _ _ n) = n
        amap f (ObjCForwardProtoDec a_1 a_2 a_3)
          = ObjCForwardProtoDec a_1 a_2 (f a_3)
        amap f (ObjCProtoDec a_1 a_2 a_3 a_4 a_5)
          = ObjCProtoDec a_1 a_2 a_3 a_4 (f a_5)

 
instance (CNode t1) => CNode (ObjCProtocolDeclBlock t1) where
        nodeInfo (ObjCProtoDeclBlock _ n) = nodeInfo n
        nodeInfo (ObjCReqProtoBlock _ n) = nodeInfo n
        nodeInfo (ObjCOptProtoBlock _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCProtocolDeclBlock t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor ObjCProtocolDeclBlock where
        fmap _f (ObjCProtoDeclBlock a1 a2)
          = ObjCProtoDeclBlock (fmap (fmap _f) a1) (_f a2)
        fmap _f (ObjCReqProtoBlock a1 a2)
          = ObjCReqProtoBlock (fmap (fmap _f) a1) (_f a2)
        fmap _f (ObjCOptProtoBlock a1 a2)
          = ObjCOptProtoBlock (fmap (fmap _f) a1) (_f a2)

 
instance Annotated ObjCProtocolDeclBlock where
        annotation (ObjCProtoDeclBlock _ n) = n
        annotation (ObjCReqProtoBlock _ n) = n
        annotation (ObjCOptProtoBlock _ n) = n
        amap f (ObjCProtoDeclBlock a_1 a_2)
          = ObjCProtoDeclBlock a_1 (f a_2)
        amap f (ObjCReqProtoBlock a_1 a_2) = ObjCReqProtoBlock a_1 (f a_2)
        amap f (ObjCOptProtoBlock a_1 a_2) = ObjCOptProtoBlock a_1 (f a_2)

 
instance (CNode t1) => CNode (ObjCInterface t1) where
        nodeInfo (ObjCIface _ _ _ _ _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCInterface t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCInterface where
        annotation (ObjCIface _ _ _ _ _ _ n) = n
        amap f (ObjCIface a_1 a_2 a_3 a_4 a_5 a_6 a_7)
          = ObjCIface a_1 a_2 a_3 a_4 a_5 a_6 (f a_7)

 
instance (CNode t1) => CNode (ObjCClassDeclarator t1) where
        nodeInfo (ObjCClassDeclr _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCClassDeclarator t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCClassDeclarator where
        annotation (ObjCClassDeclr _ n) = n
        amap f (ObjCClassDeclr a_1 a_2) = ObjCClassDeclr a_1 (f a_2)

 
instance (CNode t1) => CNode (ObjCClassName t1) where
        nodeInfo (ObjCClassNm _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCClassName t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCClassName where
        annotation (ObjCClassNm _ n) = n
        amap f (ObjCClassNm a_1 a_2) = ObjCClassNm a_1 (f a_2)

 
instance (CNode t1) => CNode (ObjCProtocolName t1) where
        nodeInfo (ObjCProtoNm _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCProtocolName t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCProtocolName where
        annotation (ObjCProtoNm _ n) = n
        amap f (ObjCProtoNm a_1 a_2) = ObjCProtoNm a_1 (f a_2)

 
instance (CNode t1) => CNode (ObjCInstanceVariableBlock t1) where
        nodeInfo (ObjCInstanceVarBlock _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCInstanceVariableBlock t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCInstanceVariableBlock where
        annotation (ObjCInstanceVarBlock _ _ n) = n
        amap f (ObjCInstanceVarBlock a_1 a_2 a_3)
          = ObjCInstanceVarBlock a_1 a_2 (f a_3)

 
instance (CNode t1) => CNode (ObjCVisibilitySpec t1) where
        nodeInfo (ObjCVisSpec _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCVisibilitySpec t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCVisibilitySpec where
        annotation (ObjCVisSpec _ n) = n
        amap f (ObjCVisSpec a_1 a_2) = ObjCVisSpec a_1 (f a_2)

 
instance (CNode t1) => CNode (ObjCInterfaceDeclaration t1) where
        nodeInfo (ObjCIfaceDecl _ n) = nodeInfo n
        nodeInfo (ObjCIfaceMethodDecl _ n) = nodeInfo n
        nodeInfo (ObjCIfacePropDecl _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCInterfaceDeclaration t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCInterfaceDeclaration where
        annotation (ObjCIfaceDecl _ n) = n
        annotation (ObjCIfaceMethodDecl _ n) = n
        annotation (ObjCIfacePropDecl _ n) = n
        amap f (ObjCIfaceDecl a_1 a_2) = ObjCIfaceDecl a_1 (f a_2)
        amap f (ObjCIfaceMethodDecl a_1 a_2)
          = ObjCIfaceMethodDecl a_1 (f a_2)
        amap f (ObjCIfacePropDecl a_1 a_2) = ObjCIfacePropDecl a_1 (f a_2)

 
instance (CNode t1) => CNode (ObjCMethodDeclaration t1) where
        nodeInfo (ObjCMethodDecl _ _ _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCMethodDeclaration t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCMethodDeclaration where
        annotation (ObjCMethodDecl _ _ _ _ n) = n
        amap f (ObjCMethodDecl a_1 a_2 a_3 a_4 a_5)
          = ObjCMethodDecl a_1 a_2 a_3 a_4 (f a_5)

 
instance (CNode t1) => CNode (ObjCMethodSelector t1) where
        nodeInfo (ObjCUnaryMethod _ n) = nodeInfo n
        nodeInfo (ObjCMethod _ _ n) = nodeInfo n
        nodeInfo (ObjCEllipseMethod _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCMethodSelector t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCMethodSelector where
        annotation (ObjCUnaryMethod _ n) = n
        annotation (ObjCMethod _ _ n) = n
        annotation (ObjCEllipseMethod _ n) = n
        amap f (ObjCUnaryMethod a_1 a_2) = ObjCUnaryMethod a_1 (f a_2)
        amap f (ObjCMethod a_1 a_2 a_3) = ObjCMethod a_1 a_2 (f a_3)
        amap f (ObjCEllipseMethod a_1 a_2) = ObjCEllipseMethod a_1 (f a_2)

 
instance (CNode t1) => CNode (ObjCKeywordDeclarator t1) where
        nodeInfo (ObjCKeywordDecl _ _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCKeywordDeclarator t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCKeywordDeclarator where
        annotation (ObjCKeywordDecl _ _ _ n) = n
        amap f (ObjCKeywordDecl a_1 a_2 a_3 a_4)
          = ObjCKeywordDecl a_1 a_2 a_3 (f a_4)

 
instance (CNode t1) => CNode (ObjCPropertyDeclaration t1) where
        nodeInfo (ObjCPropDecl _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCPropertyDeclaration t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCPropertyDeclaration where
        annotation (ObjCPropDecl _ _ n) = n
        amap f (ObjCPropDecl a_1 a_2 a_3) = ObjCPropDecl a_1 a_2 (f a_3)

 
instance (CNode t1) => CNode (ObjCPropertyModifier t1) where
        nodeInfo (ObjCPropMod _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCPropertyModifier t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCPropertyModifier where
        annotation (ObjCPropMod _ _ n) = n
        amap f (ObjCPropMod a_1 a_2 a_3) = ObjCPropMod a_1 a_2 (f a_3)

 
instance (CNode t1) => CNode (CDeclaration t1) where
        nodeInfo (CDecl _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (CDeclaration t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated CDeclaration where
        annotation (CDecl _ _ n) = n
        amap f (CDecl a_1 a_2 a_3) = CDecl a_1 a_2 (f a_3)

 
instance (CNode t1) => CNode (CDeclarator t1) where
        nodeInfo (CDeclr _ _ _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (CDeclarator t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor CDeclarator where
        fmap _f (CDeclr a1 a2 a3 a4 a5)
          = CDeclr a1 (fmap (fmap _f) a2) (fmap (fmap _f) a3)
              (fmap (fmap _f) a4)
              (_f a5)

 
instance Annotated CDeclarator where
        annotation (CDeclr _ _ _ _ n) = n
        amap f (CDeclr a_1 a_2 a_3 a_4 a_5)
          = CDeclr a_1 a_2 a_3 a_4 (f a_5)

 
instance (CNode t1) => CNode (CDerivedDeclarator t1) where
        nodeInfo (CPtrDeclr _ n) = nodeInfo n
        nodeInfo (CBlkDeclr _ n) = nodeInfo n
        nodeInfo (CArrDeclr _ _ n) = nodeInfo n
        nodeInfo (CFunDeclr _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (CDerivedDeclarator t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated CDerivedDeclarator where
        annotation (CPtrDeclr _ n) = n
        annotation (CBlkDeclr _ n) = n
        annotation (CArrDeclr _ _ n) = n
        annotation (CFunDeclr _ _ n) = n
        amap f (CPtrDeclr a_1 a_2) = CPtrDeclr a_1 (f a_2)
        amap f (CBlkDeclr a_1 a_2) = CBlkDeclr a_1 (f a_2)
        amap f (CArrDeclr a_1 a_2 a_3) = CArrDeclr a_1 a_2 (f a_3)
        amap f (CFunDeclr a_1 a_2 a_3) = CFunDeclr a_1 a_2 (f a_3)

 
instance Functor CArraySize where
        fmap _ (CNoArrSize a1) = CNoArrSize a1
        fmap _f (CArrSize a1 a2) = CArrSize a1 (fmap _f a2)

 
instance (CNode t1) => CNode (CStatement t1) where
        nodeInfo (CLabel _ _ _ n) = nodeInfo n
        nodeInfo (CCase _ _ n) = nodeInfo n
        nodeInfo (CCases _ _ _ n) = nodeInfo n
        nodeInfo (CDefault _ n) = nodeInfo n
        nodeInfo (CExpr _ n) = nodeInfo n
        nodeInfo (CCompound _ _ n) = nodeInfo n
        nodeInfo (CIf _ _ _ n) = nodeInfo n
        nodeInfo (CSwitch _ _ n) = nodeInfo n
        nodeInfo (CWhile _ _ _ n) = nodeInfo n
        nodeInfo (CFor _ _ _ _ n) = nodeInfo n
        nodeInfo (CGoto _ n) = nodeInfo n
        nodeInfo (CGotoPtr _ n) = nodeInfo n
        nodeInfo (CCont d) = nodeInfo d
        nodeInfo (CBreak d) = nodeInfo d
        nodeInfo (CReturn _ n) = nodeInfo n
        nodeInfo (CAsm _ n) = nodeInfo n
 
instance (CNode t1) => Pos (CStatement t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated CStatement where
        annotation (CLabel _ _ _ n) = n
        annotation (CCase _ _ n) = n
        annotation (CCases _ _ _ n) = n
        annotation (CDefault _ n) = n
        annotation (CExpr _ n) = n
        annotation (CCompound _ _ n) = n
        annotation (CIf _ _ _ n) = n
        annotation (CSwitch _ _ n) = n
        annotation (CWhile _ _ _ n) = n
        annotation (CFor _ _ _ _ n) = n
        annotation (CGoto _ n) = n
        annotation (CGotoPtr _ n) = n
        annotation (CCont n) = n
        annotation (CBreak n) = n
        annotation (CReturn _ n) = n
        annotation (CAsm _ n) = n
        amap f (CLabel a_1 a_2 a_3 a_4) = CLabel a_1 a_2 a_3 (f a_4)
        amap f (CCase a_1 a_2 a_3) = CCase a_1 a_2 (f a_3)
        amap f (CCases a_1 a_2 a_3 a_4) = CCases a_1 a_2 a_3 (f a_4)
        amap f (CDefault a_1 a_2) = CDefault a_1 (f a_2)
        amap f (CExpr a_1 a_2) = CExpr a_1 (f a_2)
        amap f (CCompound a_1 a_2 a_3) = CCompound a_1 a_2 (f a_3)
        amap f (CIf a_1 a_2 a_3 a_4) = CIf a_1 a_2 a_3 (f a_4)
        amap f (CSwitch a_1 a_2 a_3) = CSwitch a_1 a_2 (f a_3)
        amap f (CWhile a_1 a_2 a_3 a_4) = CWhile a_1 a_2 a_3 (f a_4)
        amap f (CFor a_1 a_2 a_3 a_4 a_5) = CFor a_1 a_2 a_3 a_4 (f a_5)
        amap f (CGoto a_1 a_2) = CGoto a_1 (f a_2)
        amap f (CGotoPtr a_1 a_2) = CGotoPtr a_1 (f a_2)
        amap f (CCont a_1) = CCont (f a_1)
        amap f (CBreak a_1) = CBreak (f a_1)
        amap f (CReturn a_1 a_2) = CReturn a_1 (f a_2)
        amap f (CAsm a_1 a_2) = CAsm a_1 (f a_2)

 
instance (CNode t1) => CNode (CAssemblyStatement t1) where
        nodeInfo (CAsmStmt _ _ _ _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (CAssemblyStatement t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor CAssemblyStatement where
        fmap _f (CAsmStmt a1 a2 a3 a4 a5 a6)
          = CAsmStmt (fmap (fmap _f) a1) (fmap _f a2) (fmap (fmap _f) a3)
              (fmap (fmap _f) a4)
              (fmap (fmap _f) a5)
              (_f a6)

 
instance Annotated CAssemblyStatement where
        annotation (CAsmStmt _ _ _ _ _ n) = n
        amap f (CAsmStmt a_1 a_2 a_3 a_4 a_5 a_6)
          = CAsmStmt a_1 a_2 a_3 a_4 a_5 (f a_6)

 
instance (CNode t1) => CNode (CAssemblyOperand t1) where
        nodeInfo (CAsmOperand _ _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (CAssemblyOperand t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor CAssemblyOperand where
        fmap _f (CAsmOperand a1 a2 a3 a4)
          = CAsmOperand a1 (fmap _f a2) (fmap _f a3) (_f a4)

 
instance Annotated CAssemblyOperand where
        annotation (CAsmOperand _ _ _ n) = n
        amap f (CAsmOperand a_1 a_2 a_3 a_4)
          = CAsmOperand a_1 a_2 a_3 (f a_4)

 
instance (CNode t1) => CNode (CCompoundBlockItem t1) where
        nodeInfo (CBlockStmt d) = nodeInfo d
        nodeInfo (CBlockDecl d) = nodeInfo d
        nodeInfo (CNestedFunDef d) = nodeInfo d
 
instance (CNode t1) => Pos (CCompoundBlockItem t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor CCompoundBlockItem where
        fmap _f (CBlockStmt a1) = CBlockStmt (fmap _f a1)
        fmap _f (CBlockDecl a1) = CBlockDecl (fmap _f a1)
        fmap _f (CNestedFunDef a1) = CNestedFunDef (fmap _f a1)

 
instance Annotated CCompoundBlockItem where
        annotation (CBlockStmt n) = annotation n
        annotation (CBlockDecl n) = annotation n
        annotation (CNestedFunDef n) = annotation n
        amap f (CBlockStmt n) = CBlockStmt (amap f n)
        amap f (CBlockDecl n) = CBlockDecl (amap f n)
        amap f (CNestedFunDef n) = CNestedFunDef (amap f n)

 
instance (CNode t1) => CNode (CDeclarationSpecifier t1) where
        nodeInfo (CStorageSpec d) = nodeInfo d
        nodeInfo (CTypeSpec d) = nodeInfo d
        nodeInfo (CTypeQual d) = nodeInfo d
 
instance (CNode t1) => Pos (CDeclarationSpecifier t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor CDeclarationSpecifier where
        fmap _f (CStorageSpec a1) = CStorageSpec (fmap _f a1)
        fmap _f (CTypeSpec a1) = CTypeSpec (fmap _f a1)
        fmap _f (CTypeQual a1) = CTypeQual (fmap _f a1)

 
instance Annotated CDeclarationSpecifier where
        annotation (CStorageSpec n) = annotation n
        annotation (CTypeSpec n) = annotation n
        annotation (CTypeQual n) = annotation n
        amap f (CStorageSpec n) = CStorageSpec (amap f n)
        amap f (CTypeSpec n) = CTypeSpec (amap f n)
        amap f (CTypeQual n) = CTypeQual (amap f n)

 
instance (CNode t1) => CNode (CStorageSpecifier t1) where
        nodeInfo (CAuto d) = nodeInfo d
        nodeInfo (CRegister d) = nodeInfo d
        nodeInfo (CStatic d) = nodeInfo d
        nodeInfo (CExtern d) = nodeInfo d
        nodeInfo (CTypedef d) = nodeInfo d
        nodeInfo (CThread d) = nodeInfo d
 
instance (CNode t1) => Pos (CStorageSpecifier t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor CStorageSpecifier where
        fmap _f (CAuto a1) = CAuto (_f a1)
        fmap _f (CRegister a1) = CRegister (_f a1)
        fmap _f (CStatic a1) = CStatic (_f a1)
        fmap _f (CExtern a1) = CExtern (_f a1)
        fmap _f (CTypedef a1) = CTypedef (_f a1)
        fmap _f (CThread a1) = CThread (_f a1)

 
instance Annotated CStorageSpecifier where
        annotation (CAuto n) = n
        annotation (CRegister n) = n
        annotation (CStatic n) = n
        annotation (CExtern n) = n
        annotation (CTypedef n) = n
        annotation (CThread n) = n
        amap f (CAuto a_1) = CAuto (f a_1)
        amap f (CRegister a_1) = CRegister (f a_1)
        amap f (CStatic a_1) = CStatic (f a_1)
        amap f (CExtern a_1) = CExtern (f a_1)
        amap f (CTypedef a_1) = CTypedef (f a_1)
        amap f (CThread a_1) = CThread (f a_1)

 
instance (CNode t1) => CNode (CTypeSpecifier t1) where
        nodeInfo (CVoidType d) = nodeInfo d
        nodeInfo (CCharType d) = nodeInfo d
        nodeInfo (CShortType d) = nodeInfo d
        nodeInfo (CIntType d) = nodeInfo d
        nodeInfo (CLongType d) = nodeInfo d
        nodeInfo (CFloatType d) = nodeInfo d
        nodeInfo (CDoubleType d) = nodeInfo d
        nodeInfo (CSignedType d) = nodeInfo d
        nodeInfo (CUnsigType d) = nodeInfo d
        nodeInfo (CBoolType d) = nodeInfo d
        nodeInfo (CComplexType d) = nodeInfo d
        nodeInfo (CSUType _ n) = nodeInfo n
        nodeInfo (CEnumType _ n) = nodeInfo n
        nodeInfo (CTypeDef _ n) = nodeInfo n
        nodeInfo (CTypeOfExpr _ n) = nodeInfo n
        nodeInfo (CTypeOfType _ n) = nodeInfo n
        nodeInfo (ObjCClassProto _ _ n) = nodeInfo n
        nodeInfo (ObjCTypeProto _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (CTypeSpecifier t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor CTypeSpecifier where
        fmap _f (CVoidType a1) = CVoidType (_f a1)
        fmap _f (CCharType a1) = CCharType (_f a1)
        fmap _f (CShortType a1) = CShortType (_f a1)
        fmap _f (CIntType a1) = CIntType (_f a1)
        fmap _f (CLongType a1) = CLongType (_f a1)
        fmap _f (CFloatType a1) = CFloatType (_f a1)
        fmap _f (CDoubleType a1) = CDoubleType (_f a1)
        fmap _f (CSignedType a1) = CSignedType (_f a1)
        fmap _f (CUnsigType a1) = CUnsigType (_f a1)
        fmap _f (CBoolType a1) = CBoolType (_f a1)
        fmap _f (CComplexType a1) = CComplexType (_f a1)
        fmap _f (CSUType a1 a2) = CSUType (fmap _f a1) (_f a2)
        fmap _f (CEnumType a1 a2) = CEnumType (fmap _f a1) (_f a2)
        fmap _f (CTypeDef a1 a2) = CTypeDef a1 (_f a2)
        fmap _f (CTypeOfExpr a1 a2) = CTypeOfExpr (fmap _f a1) (_f a2)
        fmap _f (CTypeOfType a1 a2) = CTypeOfType (fmap _f a1) (_f a2)
        fmap _f (ObjCClassProto a1 a2 a3)
          = ObjCClassProto a1 (fmap (fmap _f) a2) (_f a3)
        fmap _f (ObjCTypeProto a1 a2 a3)
          = ObjCTypeProto a1 (fmap (fmap _f) a2) (_f a3)

 
instance Annotated CTypeSpecifier where
        annotation (CVoidType n) = n
        annotation (CCharType n) = n
        annotation (CShortType n) = n
        annotation (CIntType n) = n
        annotation (CLongType n) = n
        annotation (CFloatType n) = n
        annotation (CDoubleType n) = n
        annotation (CSignedType n) = n
        annotation (CUnsigType n) = n
        annotation (CBoolType n) = n
        annotation (CComplexType n) = n
        annotation (CSUType _ n) = n
        annotation (CEnumType _ n) = n
        annotation (CTypeDef _ n) = n
        annotation (CTypeOfExpr _ n) = n
        annotation (CTypeOfType _ n) = n
        annotation (ObjCClassProto _ _ n) = n
        annotation (ObjCTypeProto _ _ n) = n
        amap f (CVoidType a_1) = CVoidType (f a_1)
        amap f (CCharType a_1) = CCharType (f a_1)
        amap f (CShortType a_1) = CShortType (f a_1)
        amap f (CIntType a_1) = CIntType (f a_1)
        amap f (CLongType a_1) = CLongType (f a_1)
        amap f (CFloatType a_1) = CFloatType (f a_1)
        amap f (CDoubleType a_1) = CDoubleType (f a_1)
        amap f (CSignedType a_1) = CSignedType (f a_1)
        amap f (CUnsigType a_1) = CUnsigType (f a_1)
        amap f (CBoolType a_1) = CBoolType (f a_1)
        amap f (CComplexType a_1) = CComplexType (f a_1)
        amap f (CSUType a_1 a_2) = CSUType a_1 (f a_2)
        amap f (CEnumType a_1 a_2) = CEnumType a_1 (f a_2)
        amap f (CTypeDef a_1 a_2) = CTypeDef a_1 (f a_2)
        amap f (CTypeOfExpr a_1 a_2) = CTypeOfExpr a_1 (f a_2)
        amap f (CTypeOfType a_1 a_2) = CTypeOfType a_1 (f a_2)
        amap f (ObjCClassProto a_1 a_2 a_3)
          = ObjCClassProto a_1 a_2 (f a_3)
        amap f (ObjCTypeProto a_1 a_2 a_3) = ObjCTypeProto a_1 a_2 (f a_3)

 
instance (CNode t1) => CNode (CTypeQualifier t1) where
        nodeInfo (CConstQual d) = nodeInfo d
        nodeInfo (CVolatQual d) = nodeInfo d
        nodeInfo (CRestrQual d) = nodeInfo d
        nodeInfo (CInlineQual d) = nodeInfo d
        nodeInfo (CAttrQual d) = nodeInfo d
        nodeInfo (ObjCProtoQual d) = nodeInfo d
 
instance (CNode t1) => Pos (CTypeQualifier t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor CTypeQualifier where
        fmap _f (CConstQual a1) = CConstQual (_f a1)
        fmap _f (CVolatQual a1) = CVolatQual (_f a1)
        fmap _f (CRestrQual a1) = CRestrQual (_f a1)
        fmap _f (CInlineQual a1) = CInlineQual (_f a1)
        fmap _f (CAttrQual a1) = CAttrQual (fmap _f a1)
        fmap _f (ObjCProtoQual a1) = ObjCProtoQual (fmap _f a1)

 
instance Annotated CTypeQualifier where
        annotation (CConstQual n) = n
        annotation (CVolatQual n) = n
        annotation (CRestrQual n) = n
        annotation (CInlineQual n) = n
        annotation (CAttrQual n) = annotation n
        annotation (ObjCProtoQual n) = annotation n
        amap f (CConstQual a_1) = CConstQual (f a_1)
        amap f (CVolatQual a_1) = CVolatQual (f a_1)
        amap f (CRestrQual a_1) = CRestrQual (f a_1)
        amap f (CInlineQual a_1) = CInlineQual (f a_1)
        amap f (CAttrQual n) = CAttrQual (amap f n)
        amap f (ObjCProtoQual n) = ObjCProtoQual (amap f n)

 
instance (CNode t1) => CNode (ObjCProtoQualifier t1) where
        nodeInfo (ObjCInQual d) = nodeInfo d
        nodeInfo (ObjCOutQual d) = nodeInfo d
        nodeInfo (ObjCInOutQual d) = nodeInfo d
        nodeInfo (ObjCBycopyQual d) = nodeInfo d
        nodeInfo (ObjCOnewayQual d) = nodeInfo d
 
instance (CNode t1) => Pos (ObjCProtoQualifier t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCProtoQualifier where
        annotation (ObjCInQual n) = n
        annotation (ObjCOutQual n) = n
        annotation (ObjCInOutQual n) = n
        annotation (ObjCBycopyQual n) = n
        annotation (ObjCOnewayQual n) = n
        amap f (ObjCInQual a_1) = ObjCInQual (f a_1)
        amap f (ObjCOutQual a_1) = ObjCOutQual (f a_1)
        amap f (ObjCInOutQual a_1) = ObjCInOutQual (f a_1)
        amap f (ObjCBycopyQual a_1) = ObjCBycopyQual (f a_1)
        amap f (ObjCOnewayQual a_1) = ObjCOnewayQual (f a_1)

 
instance (CNode t1) => CNode (CStructureUnion t1) where
        nodeInfo (CStruct _ _ _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (CStructureUnion t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor CStructureUnion where
        fmap _f (CStruct a1 a2 a3 a4 a5)
          = CStruct a1 a2 (fmap (fmap (fmap _f)) a3) (fmap (fmap _f) a4)
              (_f a5)

 
instance Annotated CStructureUnion where
        annotation (CStruct _ _ _ _ n) = n
        amap f (CStruct a_1 a_2 a_3 a_4 a_5)
          = CStruct a_1 a_2 a_3 a_4 (f a_5)

 
instance (CNode t1) => CNode (CEnumeration t1) where
        nodeInfo (CEnum _ _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (CEnumeration t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor CEnumeration where
        fmap _f (CEnum a1 a2 a3 a4)
          = CEnum a1 (fmap (fmap (fmap (fmap (fmap _f)))) a2)
              (fmap (fmap _f) a3)
              (_f a4)

 
instance Annotated CEnumeration where
        annotation (CEnum _ _ _ n) = n
        amap f (CEnum a_1 a_2 a_3 a_4) = CEnum a_1 a_2 a_3 (f a_4)

 
instance (CNode t1) => CNode (CInitializer t1) where
        nodeInfo (CInitExpr _ n) = nodeInfo n
        nodeInfo (CInitList _ n) = nodeInfo n
 
instance (CNode t1) => Pos (CInitializer t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated CInitializer where
        annotation (CInitExpr _ n) = n
        annotation (CInitList _ n) = n
        amap f (CInitExpr a_1 a_2) = CInitExpr a_1 (f a_2)
        amap f (CInitList a_1 a_2) = CInitList a_1 (f a_2)

 
instance (CNode t1) => CNode (CPartDesignator t1) where
        nodeInfo (CArrDesig _ n) = nodeInfo n
        nodeInfo (CMemberDesig _ n) = nodeInfo n
        nodeInfo (CRangeDesig _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (CPartDesignator t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor CPartDesignator where
        fmap _f (CArrDesig a1 a2) = CArrDesig (fmap _f a1) (_f a2)
        fmap _f (CMemberDesig a1 a2) = CMemberDesig a1 (_f a2)
        fmap _f (CRangeDesig a1 a2 a3)
          = CRangeDesig (fmap _f a1) (fmap _f a2) (_f a3)

 
instance Annotated CPartDesignator where
        annotation (CArrDesig _ n) = n
        annotation (CMemberDesig _ n) = n
        annotation (CRangeDesig _ _ n) = n
        amap f (CArrDesig a_1 a_2) = CArrDesig a_1 (f a_2)
        amap f (CMemberDesig a_1 a_2) = CMemberDesig a_1 (f a_2)
        amap f (CRangeDesig a_1 a_2 a_3) = CRangeDesig a_1 a_2 (f a_3)

 
instance (CNode t1) => CNode (CAttribute t1) where
        nodeInfo (CAttr _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (CAttribute t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor CAttribute where
        fmap _f (CAttr a1 a2 a3) = CAttr a1 (fmap (fmap _f) a2) (_f a3)

 
instance Annotated CAttribute where
        annotation (CAttr _ _ n) = n
        amap f (CAttr a_1 a_2 a_3) = CAttr a_1 a_2 (f a_3)

 
instance (CNode t1) => CNode (CExpression t1) where
        nodeInfo (CComma _ n) = nodeInfo n
        nodeInfo (CAssign _ _ _ n) = nodeInfo n
        nodeInfo (CCond _ _ _ n) = nodeInfo n
        nodeInfo (CBinary _ _ _ n) = nodeInfo n
        nodeInfo (CCast _ _ n) = nodeInfo n
        nodeInfo (CUnary _ _ n) = nodeInfo n
        nodeInfo (CSizeofExpr _ n) = nodeInfo n
        nodeInfo (CSizeofType _ n) = nodeInfo n
        nodeInfo (CAlignofExpr _ n) = nodeInfo n
        nodeInfo (CAlignofType _ n) = nodeInfo n
        nodeInfo (CComplexReal _ n) = nodeInfo n
        nodeInfo (CComplexImag _ n) = nodeInfo n
        nodeInfo (CIndex _ _ n) = nodeInfo n
        nodeInfo (CCall _ _ n) = nodeInfo n
        nodeInfo (CMember _ _ _ n) = nodeInfo n
        nodeInfo (CVar _ n) = nodeInfo n
        nodeInfo (CConst d) = nodeInfo d
        nodeInfo (CCompoundLit _ _ n) = nodeInfo n
        nodeInfo (CStatExpr _ n) = nodeInfo n
        nodeInfo (CLabAddrExpr _ n) = nodeInfo n
        nodeInfo (CBuiltinExpr d) = nodeInfo d
        nodeInfo (CBlockExpr _ _ n) = nodeInfo n
        nodeInfo (ObjCMessageExpr _ n) = nodeInfo n
        nodeInfo (ObjCSelectorExpr _ n) = nodeInfo n
        nodeInfo (ObjCProtoExpr _ n) = nodeInfo n
        nodeInfo (ObjCEncodeExpr _ n) = nodeInfo n
 
instance (CNode t1) => Pos (CExpression t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated CExpression where
        annotation (CComma _ n) = n
        annotation (CAssign _ _ _ n) = n
        annotation (CCond _ _ _ n) = n
        annotation (CBinary _ _ _ n) = n
        annotation (CCast _ _ n) = n
        annotation (CUnary _ _ n) = n
        annotation (CSizeofExpr _ n) = n
        annotation (CSizeofType _ n) = n
        annotation (CAlignofExpr _ n) = n
        annotation (CAlignofType _ n) = n
        annotation (CComplexReal _ n) = n
        annotation (CComplexImag _ n) = n
        annotation (CIndex _ _ n) = n
        annotation (CCall _ _ n) = n
        annotation (CMember _ _ _ n) = n
        annotation (CVar _ n) = n
        annotation (CConst n) = annotation n
        annotation (CCompoundLit _ _ n) = n
        annotation (CStatExpr _ n) = n
        annotation (CLabAddrExpr _ n) = n
        annotation (CBuiltinExpr n) = annotation n
        annotation (CBlockExpr _ _ n) = n
        annotation (ObjCMessageExpr _ n) = n
        annotation (ObjCSelectorExpr _ n) = n
        annotation (ObjCProtoExpr _ n) = n
        annotation (ObjCEncodeExpr _ n) = n
        amap f (CComma a_1 a_2) = CComma a_1 (f a_2)
        amap f (CAssign a_1 a_2 a_3 a_4) = CAssign a_1 a_2 a_3 (f a_4)
        amap f (CCond a_1 a_2 a_3 a_4) = CCond a_1 a_2 a_3 (f a_4)
        amap f (CBinary a_1 a_2 a_3 a_4) = CBinary a_1 a_2 a_3 (f a_4)
        amap f (CCast a_1 a_2 a_3) = CCast a_1 a_2 (f a_3)
        amap f (CUnary a_1 a_2 a_3) = CUnary a_1 a_2 (f a_3)
        amap f (CSizeofExpr a_1 a_2) = CSizeofExpr a_1 (f a_2)
        amap f (CSizeofType a_1 a_2) = CSizeofType a_1 (f a_2)
        amap f (CAlignofExpr a_1 a_2) = CAlignofExpr a_1 (f a_2)
        amap f (CAlignofType a_1 a_2) = CAlignofType a_1 (f a_2)
        amap f (CComplexReal a_1 a_2) = CComplexReal a_1 (f a_2)
        amap f (CComplexImag a_1 a_2) = CComplexImag a_1 (f a_2)
        amap f (CIndex a_1 a_2 a_3) = CIndex a_1 a_2 (f a_3)
        amap f (CCall a_1 a_2 a_3) = CCall a_1 a_2 (f a_3)
        amap f (CMember a_1 a_2 a_3 a_4) = CMember a_1 a_2 a_3 (f a_4)
        amap f (CVar a_1 a_2) = CVar a_1 (f a_2)
        amap f (CConst n) = CConst (amap f n)
        amap f (CCompoundLit a_1 a_2 a_3) = CCompoundLit a_1 a_2 (f a_3)
        amap f (CStatExpr a_1 a_2) = CStatExpr a_1 (f a_2)
        amap f (CLabAddrExpr a_1 a_2) = CLabAddrExpr a_1 (f a_2)
        amap f (CBuiltinExpr n) = CBuiltinExpr (amap f n)
        amap f (CBlockExpr a_1 a_2 a_3) = CBlockExpr a_1 a_2 (f a_3)
        amap f (ObjCMessageExpr a_1 a_2) = ObjCMessageExpr a_1 (f a_2)
        amap f (ObjCSelectorExpr a_1 a_2) = ObjCSelectorExpr a_1 (f a_2)
        amap f (ObjCProtoExpr a_1 a_2) = ObjCProtoExpr a_1 (f a_2)
        amap f (ObjCEncodeExpr a_1 a_2) = ObjCEncodeExpr a_1 (f a_2)

 
instance (CNode t1) => CNode (ObjCSelectorName t1) where
        nodeInfo (ObjCSelPlain _ n) = nodeInfo n
        nodeInfo (ObjCSelKeys _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCSelectorName t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCSelectorName where
        annotation (ObjCSelPlain _ n) = n
        annotation (ObjCSelKeys _ n) = n
        amap f (ObjCSelPlain a_1 a_2) = ObjCSelPlain a_1 (f a_2)
        amap f (ObjCSelKeys a_1 a_2) = ObjCSelKeys a_1 (f a_2)

 
instance (CNode t1) => CNode (ObjCSelectorKeyName t1) where
        nodeInfo (ObjCSelKeyName _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCSelectorKeyName t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCSelectorKeyName where
        annotation (ObjCSelKeyName _ n) = n
        amap f (ObjCSelKeyName a_1 a_2) = ObjCSelKeyName a_1 (f a_2)

 
instance (CNode t1) => CNode (ObjCSelector t1) where
        nodeInfo (ObjCSel _ n) = nodeInfo n
        nodeInfo (ObjCInSel d) = nodeInfo d
        nodeInfo (ObjCOutSel d) = nodeInfo d
 
instance (CNode t1) => Pos (ObjCSelector t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCSelector where
        annotation (ObjCSel _ n) = n
        annotation (ObjCInSel n) = n
        annotation (ObjCOutSel n) = n
        amap f (ObjCSel a_1 a_2) = ObjCSel a_1 (f a_2)
        amap f (ObjCInSel a_1) = ObjCInSel (f a_1)
        amap f (ObjCOutSel a_1) = ObjCOutSel (f a_1)

 
instance (CNode t1) => CNode (ObjCMessageExpression t1) where
        nodeInfo (ObjCMsgExpr _ _ n) = nodeInfo n
        nodeInfo (ObjCMsgClass _ _ n) = nodeInfo n
        nodeInfo (ObjCMsgSup _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCMessageExpression t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCMessageExpression where
        annotation (ObjCMsgExpr _ _ n) = n
        annotation (ObjCMsgClass _ _ n) = n
        annotation (ObjCMsgSup _ n) = n
        amap f (ObjCMsgExpr a_1 a_2 a_3) = ObjCMsgExpr a_1 a_2 (f a_3)
        amap f (ObjCMsgClass a_1 a_2 a_3) = ObjCMsgClass a_1 a_2 (f a_3)
        amap f (ObjCMsgSup a_1 a_2) = ObjCMsgSup a_1 (f a_2)

 
instance (CNode t1) => CNode (ObjCMessageSelector t1) where
        nodeInfo (ObjCMsgSel _ n) = nodeInfo n
        nodeInfo (ObjCKeyArgs _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCMessageSelector t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCMessageSelector where
        annotation (ObjCMsgSel _ n) = n
        annotation (ObjCKeyArgs _ n) = n
        amap f (ObjCMsgSel a_1 a_2) = ObjCMsgSel a_1 (f a_2)
        amap f (ObjCKeyArgs a_1 a_2) = ObjCKeyArgs a_1 (f a_2)

 
instance (CNode t1) => CNode (ObjCKeywordArg t1) where
        nodeInfo (ObjCKeyArg _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (ObjCKeywordArg t1) where
        posOf x = posOf (nodeInfo x)

 
instance Annotated ObjCKeywordArg where
        annotation (ObjCKeyArg _ _ n) = n
        amap f (ObjCKeyArg a_1 a_2 a_3) = ObjCKeyArg a_1 a_2 (f a_3)

 
instance (CNode t1) => CNode (CBuiltinThing t1) where
        nodeInfo (CBuiltinVaArg _ _ n) = nodeInfo n
        nodeInfo (CBuiltinOffsetOf _ _ n) = nodeInfo n
        nodeInfo (CBuiltinTypesCompatible _ _ n) = nodeInfo n
 
instance (CNode t1) => Pos (CBuiltinThing t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor CBuiltinThing where
        fmap _f (CBuiltinVaArg a1 a2 a3)
          = CBuiltinVaArg (fmap _f a1) (fmap _f a2) (_f a3)
        fmap _f (CBuiltinOffsetOf a1 a2 a3)
          = CBuiltinOffsetOf (fmap _f a1) (fmap (fmap _f) a2) (_f a3)
        fmap _f (CBuiltinTypesCompatible a1 a2 a3)
          = CBuiltinTypesCompatible (fmap _f a1) (fmap _f a2) (_f a3)

 
instance Annotated CBuiltinThing where
        annotation (CBuiltinVaArg _ _ n) = n
        annotation (CBuiltinOffsetOf _ _ n) = n
        annotation (CBuiltinTypesCompatible _ _ n) = n
        amap f (CBuiltinVaArg a_1 a_2 a_3) = CBuiltinVaArg a_1 a_2 (f a_3)
        amap f (CBuiltinOffsetOf a_1 a_2 a_3)
          = CBuiltinOffsetOf a_1 a_2 (f a_3)
        amap f (CBuiltinTypesCompatible a_1 a_2 a_3)
          = CBuiltinTypesCompatible a_1 a_2 (f a_3)

 
instance (CNode t1) => CNode (CConstant t1) where
        nodeInfo (CIntConst _ n) = nodeInfo n
        nodeInfo (CCharConst _ n) = nodeInfo n
        nodeInfo (CFloatConst _ n) = nodeInfo n
        nodeInfo (CStrConst _ n) = nodeInfo n
 
instance (CNode t1) => Pos (CConstant t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor CConstant where
        fmap _f (CIntConst a1 a2) = CIntConst a1 (_f a2)
        fmap _f (CCharConst a1 a2) = CCharConst a1 (_f a2)
        fmap _f (CFloatConst a1 a2) = CFloatConst a1 (_f a2)
        fmap _f (CStrConst a1 a2) = CStrConst a1 (_f a2)

 
instance Annotated CConstant where
        annotation (CIntConst _ n) = n
        annotation (CCharConst _ n) = n
        annotation (CFloatConst _ n) = n
        annotation (CStrConst _ n) = n
        amap f (CIntConst a_1 a_2) = CIntConst a_1 (f a_2)
        amap f (CCharConst a_1 a_2) = CCharConst a_1 (f a_2)
        amap f (CFloatConst a_1 a_2) = CFloatConst a_1 (f a_2)
        amap f (CStrConst a_1 a_2) = CStrConst a_1 (f a_2)

 
instance (CNode t1) => CNode (CStringLiteral t1) where
        nodeInfo (CStrLit _ n) = nodeInfo n
 
instance (CNode t1) => Pos (CStringLiteral t1) where
        posOf x = posOf (nodeInfo x)

 
instance Functor CStringLiteral where
        fmap _f (CStrLit a1 a2) = CStrLit a1 (_f a2)

 
instance Annotated CStringLiteral where
        annotation (CStrLit _ n) = n
        amap f (CStrLit a_1 a_2) = CStrLit a_1 (f a_2)
-- GENERATED STOP
