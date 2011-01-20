{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Syntax.AST
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
module Language.C.Syntax.AST (
  -- * C translation units
  CTranslUnit,  CExtDecl,
  CTranslationUnit(..),  CExternalDeclaration(..),
  -- * Declarations
  CFunDef,  CDecl, CStructUnion, CEnum,
  CFunctionDef(..),  CDeclaration(..),
  CStructTag(..), CStructureUnion(..),  CEnumeration(..),
  -- * Declaration attributes
  CDeclSpec, partitionDeclSpecs,
  CStorageSpec, CTypeSpec, isSUEDef, CTypeQual, CAttr,
  CDeclarationSpecifier(..), CStorageSpecifier(..), CTypeSpecifier(..),
  CTypeQualifier(..), CAttribute(..),
  -- * Declarators
  CDeclr,CDerivedDeclr,CArrSize,
  CDeclarator(..), CDerivedDeclarator(..), CArraySize(..),
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
  -- * Constants
  CConst, CStrLit, cstringOfLit, liftStrLit,
  CConstant(..), CStringLiteral(..)
) where
import Data.List
import Language.C.Syntax.Constants
import Language.C.Syntax.Ops
import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Data.Position
import Data.Generics

-- | Complete C tranlsation unit (C99 6.9, K&R A10)
--
-- A complete C translation unit, for example representing a C header or source file.
-- It consists of a list of external (i.e. toplevel) declarations.
type CTranslUnit = CTranslationUnit NodeInfo
data CTranslationUnit a
  = CTranslUnit [CExternalDeclaration a] a
    deriving (Show, Data,Typeable {-! CNode !-})

-- | External C declaration (C99 6.9, K&R A10)
--
-- Either a toplevel declaration, function definition or external assembler.
type CExtDecl = CExternalDeclaration NodeInfo
data CExternalDeclaration a
  = CDeclExt (CDeclaration a)
  | CFDefExt (CFunctionDef a)
  | CAsmExt  (CStringLiteral a) a
    deriving (Show, Data,Typeable {-! CNode !-})

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
    deriving (Show, Data,Typeable {-! CNode !-})

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
    deriving (Show, Data,Typeable {-! CNode !-})

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
    deriving (Show, Data,Typeable {-! CNode !-})

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
  | CArrDeclr [CTypeQualifier a] (CArraySize a) a
  -- ^ Array declarator @CArrDeclr declr tyquals size-expr?@
  | CFunDeclr (Either [Ident] ([CDeclaration a],Bool)) [CAttribute a] a
    -- ^ Function declarator @CFunDeclr declr (old-style-params | new-style-params) c-attrs@
    deriving (Show, Data,Typeable {-! CNode !-})

-- | Size of an array
type CArrSize = CArraySize NodeInfo
data CArraySize a
  = CNoArrSize Bool               -- ^ @CUnknownSize isCompleteType@
  | CArrSize Bool (CExpression a) -- ^ @CArrSize isStatic expr@
    deriving (Show, Data,Typeable)

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
  | CAsm CAsmStmt a
    deriving (Show, Data,Typeable {-! CNode !-})

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
    [CAsmOperand]              -- output operands
    [CAsmOperand]              -- input operands
    [CStringLiteral a]         -- Clobbers
    a
    deriving (Show, Data,Typeable {-! CNode !-})
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
    deriving (Show, Data,Typeable {-! CNode !-})
-- | C99 Block items
--
--  Things that may appear in compound statements: either statements, declarations
--   or nested function definitions.
type CBlockItem = CCompoundBlockItem NodeInfo
data CCompoundBlockItem a
  = CBlockStmt    (CStatement a)    -- ^ A statement
  | CBlockDecl    (CDeclaration a)  -- ^ A local declaration
  | CNestedFunDef (CFunctionDef a)  -- ^ A nested function (GNU C)
    deriving (Show, Data,Typeable {-! CNode !-})
-- | C declaration specifiers and qualifiers
--
-- Declaration specifiers include at most one storage-class specifier (C99 6.7.1),
-- type specifiers (6.7.2) and type qualifiers (6.7.3).
type CDeclSpec = CDeclarationSpecifier NodeInfo
data CDeclarationSpecifier a
  = CStorageSpec (CStorageSpecifier a) -- ^ storage-class specifier or typedef
  | CTypeSpec    (CTypeSpecifier a)    -- ^ type name
  | CTypeQual    (CTypeQualifier a)    -- ^ type qualifier
    deriving (Show, Data,Typeable {-! CNode !-})

-- | Seperate the declaration specifiers
--
-- Note that inline isn't actually a type qualifier, but a function specifier.
-- @__attribute__@ of a declaration qualify declarations or declarators (but not types),
-- and are therefore seperated as well.
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
    deriving (Show, Eq,Ord,Data,Typeable {-! CNode !-})

-- | C type specifier (K&R A8.2, C99 6.7.2)
--
-- Type specifiers are either basic types such as @char@ or @int@,
-- @struct@, @union@ or @enum@ specifiers or typedef names.
--
-- As a GNU extension, a @typeof@ expression also is a type specifier.
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
  | CSUType      CStructUnion a      -- ^ Struct or Union specifier
  | CEnumType    CEnum        a      -- ^ Enumeration specifier
  | CTypeDef     Ident        a      -- ^ Typedef name
  | CTypeOfExpr  (CExpression a)  a  -- ^ @typeof(expr)@
  | CTypeOfType  (CDeclaration a) a  -- ^ @typeof(type)@
    deriving (Show, Data,Typeable {-! CNode !-})

-- | returns @True@ if the given typespec is a struct, union or enum /definition/
isSUEDef :: CTypeSpec -> Bool
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
    deriving (Show, Data,Typeable {-! CNode !-})

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
    deriving (Show, Data,Typeable {-! CNode !-})

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
    deriving (Show, Data,Typeable {-! CNode !-})


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
    deriving (Show, Data,Typeable {-! CNode !-})

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
    deriving (Show, Data,Typeable {-! CNode !-})

-- | @__attribute__@ annotations
--
-- Those are of the form @CAttr attribute-name attribute-parameters@,
-- and serve as generic properties of some syntax tree elements.
type CAttr = CAttribute NodeInfo
data CAttribute a = CAttr Ident [CExpression a] a
                    deriving (Show, Data,Typeable {-! CNode !-})

-- | C expression (K&R A7)
--
-- * these can be arbitrary expression, as the argument of `sizeof' can be
--   arbitrary, even if appearing in a constant expression
--
-- * GNU C extensions: @alignof@, @__real@, @__imag@, @({ stmt-expr })@, @&& label@ and built-ins
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
  | CConst       CConst                  -- ^ integer, character, floating point and string constants
  | CCompoundLit (CDeclaration a)
                 (CInitializerList a)    -- type name & initialiser list
                 a                       -- ^ C99 compound literal
  | CStatExpr    (CStatement a) a        -- ^ GNU C compound statement as expr
  | CLabAddrExpr Ident a                 -- ^ GNU C address of label
  | CBuiltinExpr (CBuiltinThing a)       -- ^ builtin expressions, see 'CBuiltin'
    deriving (Show, Data,Typeable {-! CNode !-})


-- | GNU Builtins, which cannot be typed in C99
type CBuiltin = CBuiltinThing NodeInfo
data CBuiltinThing a
  = CBuiltinVaArg (CExpression a) (CDeclaration a) a            -- ^ @(expr, type)@
  | CBuiltinOffsetOf (CDeclaration a) [CDesignator] a -- ^ @(type, designator-list)@
  | CBuiltinTypesCompatible (CDeclaration a) (CDeclaration a) a  -- ^ @(type,type)@
    deriving (Show, Data,Typeable {-! CNode !-})

-- | C constant (K&R A2.5 & A7.2)
type CConst = CConstant NodeInfo
data CConstant a
  = CIntConst   CInteger a
  | CCharConst  CChar a
  | CFloatConst CFloat a
  | CStrConst   CString a
    deriving (Show, Data,Typeable {-! CNode !-})

-- | Attributed string literals
type CStrLit = CStringLiteral NodeInfo
data CStringLiteral a = CStrLit CString a
            deriving (Show, Data,Typeable {-! CNode !-})

cstringOfLit :: CStrLit -> CString
cstringOfLit (CStrLit cstr _) = cstr

-- | Lift a string literal to a C constant
liftStrLit :: CStrLit -> CConst
liftStrLit (CStrLit str at) = CStrConst str at



--------------------------------------------------------
-- DERIVES GENERATED CODE
-- DO NOT MODIFY BELOW THIS LINE
-- CHECKSUM: 1085644567

instance CNode t1 => CNode (CTranslationUnit t1)
    where nodeInfo (CTranslUnit _ t) = nodeInfo t
instance CNode t1 => Pos (CTranslationUnit t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CExternalDeclaration t1)
    where nodeInfo (CDeclExt d) = nodeInfo d
          nodeInfo (CFDefExt d) = nodeInfo d
          nodeInfo (CAsmExt _ t) = nodeInfo t
instance CNode t1 => Pos (CExternalDeclaration t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CFunctionDef t1)
    where nodeInfo (CFunDef _ _ _ _ t) = nodeInfo t
instance CNode t1 => Pos (CFunctionDef t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CDeclaration t1)
    where nodeInfo (CDecl _ _ t) = nodeInfo t
instance CNode t1 => Pos (CDeclaration t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CDeclarator t1)
    where nodeInfo (CDeclr _ _ _ _ t) = nodeInfo t
instance CNode t1 => Pos (CDeclarator t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CDerivedDeclarator t1)
    where nodeInfo (CPtrDeclr _ t) = nodeInfo t
          nodeInfo (CArrDeclr _ _ t) = nodeInfo t
          nodeInfo (CFunDeclr _ _ t) = nodeInfo t
instance CNode t1 => Pos (CDerivedDeclarator t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CStatement t1)
    where nodeInfo (CLabel _ _ _ t) = nodeInfo t
          nodeInfo (CCase _ _ t) = nodeInfo t
          nodeInfo (CCases _ _ _ t) = nodeInfo t
          nodeInfo (CDefault _ t) = nodeInfo t
          nodeInfo (CExpr _ t) = nodeInfo t
          nodeInfo (CCompound _ _ t) = nodeInfo t
          nodeInfo (CIf _ _ _ t) = nodeInfo t
          nodeInfo (CSwitch _ _ t) = nodeInfo t
          nodeInfo (CWhile _ _ _ t) = nodeInfo t
          nodeInfo (CFor _ _ _ _ t) = nodeInfo t
          nodeInfo (CGoto _ t) = nodeInfo t
          nodeInfo (CGotoPtr _ t) = nodeInfo t
          nodeInfo (CCont d) = nodeInfo d
          nodeInfo (CBreak d) = nodeInfo d
          nodeInfo (CReturn _ t) = nodeInfo t
          nodeInfo (CAsm _ t) = nodeInfo t
instance CNode t1 => Pos (CStatement t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CAssemblyStatement t1)
    where nodeInfo (CAsmStmt _ _ _ _ _ t) = nodeInfo t
instance CNode t1 => Pos (CAssemblyStatement t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CAssemblyOperand t1)
    where nodeInfo (CAsmOperand _ _ _ t) = nodeInfo t
instance CNode t1 => Pos (CAssemblyOperand t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CCompoundBlockItem t1)
    where nodeInfo (CBlockStmt d) = nodeInfo d
          nodeInfo (CBlockDecl d) = nodeInfo d
          nodeInfo (CNestedFunDef d) = nodeInfo d
instance CNode t1 => Pos (CCompoundBlockItem t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CDeclarationSpecifier t1)
    where nodeInfo (CStorageSpec d) = nodeInfo d
          nodeInfo (CTypeSpec d) = nodeInfo d
          nodeInfo (CTypeQual d) = nodeInfo d
instance CNode t1 => Pos (CDeclarationSpecifier t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CStorageSpecifier t1)
    where nodeInfo (CAuto d) = nodeInfo d
          nodeInfo (CRegister d) = nodeInfo d
          nodeInfo (CStatic d) = nodeInfo d
          nodeInfo (CExtern d) = nodeInfo d
          nodeInfo (CTypedef d) = nodeInfo d
          nodeInfo (CThread d) = nodeInfo d
instance CNode t1 => Pos (CStorageSpecifier t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CTypeSpecifier t1)
    where nodeInfo (CVoidType d) = nodeInfo d
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
          nodeInfo (CSUType _ t) = nodeInfo t
          nodeInfo (CEnumType _ t) = nodeInfo t
          nodeInfo (CTypeDef _ t) = nodeInfo t
          nodeInfo (CTypeOfExpr _ t) = nodeInfo t
          nodeInfo (CTypeOfType _ t) = nodeInfo t
instance CNode t1 => Pos (CTypeSpecifier t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CTypeQualifier t1)
    where nodeInfo (CConstQual d) = nodeInfo d
          nodeInfo (CVolatQual d) = nodeInfo d
          nodeInfo (CRestrQual d) = nodeInfo d
          nodeInfo (CInlineQual d) = nodeInfo d
          nodeInfo (CAttrQual d) = nodeInfo d
instance CNode t1 => Pos (CTypeQualifier t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CStructureUnion t1)
    where nodeInfo (CStruct _ _ _ _ t) = nodeInfo t
instance CNode t1 => Pos (CStructureUnion t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CEnumeration t1)
    where nodeInfo (CEnum _ _ _ t) = nodeInfo t
instance CNode t1 => Pos (CEnumeration t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CInitializer t1)
    where nodeInfo (CInitExpr _ t) = nodeInfo t
          nodeInfo (CInitList _ t) = nodeInfo t
instance CNode t1 => Pos (CInitializer t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CPartDesignator t1)
    where nodeInfo (CArrDesig _ t) = nodeInfo t
          nodeInfo (CMemberDesig _ t) = nodeInfo t
          nodeInfo (CRangeDesig _ _ t) = nodeInfo t
instance CNode t1 => Pos (CPartDesignator t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CAttribute t1)
    where nodeInfo (CAttr _ _ t) = nodeInfo t
instance CNode t1 => Pos (CAttribute t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CExpression t1)
    where nodeInfo (CComma _ t) = nodeInfo t
          nodeInfo (CAssign _ _ _ t) = nodeInfo t
          nodeInfo (CCond _ _ _ t) = nodeInfo t
          nodeInfo (CBinary _ _ _ t) = nodeInfo t
          nodeInfo (CCast _ _ t) = nodeInfo t
          nodeInfo (CUnary _ _ t) = nodeInfo t
          nodeInfo (CSizeofExpr _ t) = nodeInfo t
          nodeInfo (CSizeofType _ t) = nodeInfo t
          nodeInfo (CAlignofExpr _ t) = nodeInfo t
          nodeInfo (CAlignofType _ t) = nodeInfo t
          nodeInfo (CComplexReal _ t) = nodeInfo t
          nodeInfo (CComplexImag _ t) = nodeInfo t
          nodeInfo (CIndex _ _ t) = nodeInfo t
          nodeInfo (CCall _ _ t) = nodeInfo t
          nodeInfo (CMember _ _ _ t) = nodeInfo t
          nodeInfo (CVar _ t) = nodeInfo t
          nodeInfo (CConst d) = nodeInfo d
          nodeInfo (CCompoundLit _ _ t) = nodeInfo t
          nodeInfo (CStatExpr _ t) = nodeInfo t
          nodeInfo (CLabAddrExpr _ t) = nodeInfo t
          nodeInfo (CBuiltinExpr d) = nodeInfo d
instance CNode t1 => Pos (CExpression t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CBuiltinThing t1)
    where nodeInfo (CBuiltinVaArg _ _ t) = nodeInfo t
          nodeInfo (CBuiltinOffsetOf _ _ t) = nodeInfo t
          nodeInfo (CBuiltinTypesCompatible _ _ t) = nodeInfo t
instance CNode t1 => Pos (CBuiltinThing t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CConstant t1)
    where nodeInfo (CIntConst _ t) = nodeInfo t
          nodeInfo (CCharConst _ t) = nodeInfo t
          nodeInfo (CFloatConst _ t) = nodeInfo t
          nodeInfo (CStrConst _ t) = nodeInfo t
instance CNode t1 => Pos (CConstant t1)
    where posOf x = posOfNode (nodeInfo x)

instance CNode t1 => CNode (CStringLiteral t1)
    where nodeInfo (CStrLit _ t) = nodeInfo t
instance CNode t1 => Pos (CStringLiteral t1)
    where posOf x = posOfNode (nodeInfo x)
