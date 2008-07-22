{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Syntax.AST
-- Copyright   :  (c) [1999..2007] Manuel M T Chakravarty
--                (c) 2008 Benedikt Huber        
-- License     :  BSD-style
-- Maintainer  :  -
-- Portability :  portable
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
  CTranslUnit(..),  CExtDecl(..),
  -- * Declarations
  CFunDef(..),  CDecl(..),
  CStructUnion(..),  CStructTag(..), CEnum(..),
  -- * Declaration attributes
  CDeclSpec(..), CStorageSpec(..), CTypeSpec(..), CTypeQual(..), CAttr(..),
  -- * Declarators
  CDeclr(..),CDerivedDeclr(..),
  -- * Initialization
  CInit(..), CInitList, CDesignator(..), 
  -- * Statements
  CStat(..), CBlockItem(..),
  CAsmStmt(..), CAsmOperand(..), 
  -- * Expressions
  CExpr(..),
  CAssignOp(..), CBinaryOp(..), CUnaryOp(..), 
  CBuiltin(..),
  -- * constants
  CConst(..),CStrLit(..),cstringOfLit,liftStrLit,
) where
import Data.List
import Language.C.Syntax.Constants
import Language.C.Syntax.Ops
import Language.C.Syntax.Ident
import Language.C.Syntax.Node
import Language.C.Syntax.Position
import Data.Generics

-- | Complete C tranlsation unit (C99 6.9, K&R A10)
--  
-- A complete C translation unit, for example representing a C header or source file.
-- It consists of a list of external (i.e. toplevel) declarations.
data CTranslUnit = CTranslUnit [CExtDecl] NodeInfo
                   deriving (Data,Typeable {-! CNode !-})
-- | External C declaration (C99 6.9, K&R A10)
--
-- Either a toplevel declaration, function definition or external assembler.
data CExtDecl = CDeclExt CDecl
              | CFDefExt CFunDef
              | CAsmExt  CStrLit
              deriving (Data,Typeable {-! CNode !-})

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
data CFunDef = CFunDef [CDeclSpec]      -- type specifier and qualifier
                       CDeclr           -- declarator
                       [CDecl]          -- optional declaration list
                       CStat            -- compound statement
                       NodeInfo
               deriving (Data,Typeable {-! CNode !-})

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
data CDecl = CDecl [CDeclSpec]          -- type specifier and qualifier, __attribute__
                   [(Maybe CDeclr,      -- declarator (may be omitted)
                     Maybe CInit,       -- optional initialize
                     Maybe CExpr)]      -- optional size (const expr)
                   NodeInfo
             deriving (Data,Typeable {-! CNode !-})

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
data CDeclr = CDeclr (Maybe Ident) [CDerivedDeclr] (Maybe CStrLit) [CAttr] NodeInfo
              deriving (Data,Typeable {-! CNode !-})

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
data CDerivedDeclr =
              CPtrDeclr [CTypeQual] NodeInfo
              -- ^ Pointer declarator @CPtrDeclr tyquals declr@
            | CArrDeclr [CTypeQual] (Maybe CExpr) NodeInfo 
              -- ^ Array declarator @CArrDeclr declr tyquals size-expr?@ 
            | CFunDeclr (Either [Ident] ([CDecl],Bool)) [CAttr] NodeInfo
              -- ^ Function declarator @CFunDeclr declr (old-style-params | new-style-params) c-attrs@ 
            deriving (Data,Typeable {-! CNode !-})
-- | C statement (K&R A9, C99 6.8)
--
data CStat = CLabel  Ident CStat [CAttr] NodeInfo  -- ^ An (attributed) label followed by a statement
           | CCase CExpr CStat NodeInfo            -- ^ A statement of the form @case expr : stmt@
           | CCases CExpr CExpr CStat NodeInfo     -- ^ A case range of the form @case lower ... upper : stmt@
           | CDefault CStat NodeInfo               -- ^ The default case @default : stmt@
           | CExpr (Maybe CExpr) NodeInfo           
           -- ^ A simple statement, that is in C: evaluating an expression with side-effects
           --   and discarding the result.
           | CCompound [Ident] [CBlockItem] NodeInfo    -- ^ compound statement @CCompound localLabels blockItems at@
           | CIf CExpr CStat (Maybe CStat) NodeInfo     -- ^ conditional statement @CIf ifExpr thenStmt maybeElseStmt at@
           | CSwitch CExpr CStat NodeInfo               
           -- ^ switch statement @CSwitch selectorExpr switchStmt@, where @switchStmt@ usually includes
           -- /case/, /break/ and /default/ statements
           | CWhile CExpr CStat Bool NodeInfo      -- ^ while or do-while statement @CWhile guard stmt isDoWhile at@
           | CFor (Either (Maybe CExpr) CDecl)
                  (Maybe CExpr)
                  (Maybe CExpr)
                  CStat
                  NodeInfo            
           -- ^ for statement @CFor init expr-2 expr-3 stmt@, where @init@ is either a declaration or
           -- initializing expression
           | CGoto Ident NodeInfo            -- ^ goto statement @CGoto label@
           | CGotoPtr CExpr NodeInfo         -- ^ computed goto @CGotoPtr labelExpr@
           | CCont NodeInfo                  -- ^ continue statement
           | CBreak    NodeInfo              -- ^ break statement
           | CReturn (Maybe CExpr)NodeInfo   -- ^ return statement @CReturn returnExpr@
           | CAsm CAsmStmt NodeInfo          -- ^ assembly statement 
           deriving (Data,Typeable {-! CNode !-})

-- | GNU Assembler statement
--
-- > CAsmStatement type-qual? asm-expr out-ops in-ops clobbers _
--
-- is an inline assembler statement. 
-- The only type-qualifier (if any) allowed is /volatile/.
-- @asm-expr@ is the actual assembler epxression (a string), @out-ops@ and @in-ops@ are the input
-- and output operands of the statement. 
-- @clobbers@ is a list of registers which are clobbered when executing the assembler statement
data CAsmStmt 
  = CAsmStmt (Maybe CTypeQual)     -- maybe volatile
                    CStrLit        -- assembler expression (String)
                   [CAsmOperand]   -- output operands
                   [CAsmOperand]   -- input operands
                   [CStrLit]       -- Clobbers
                    NodeInfo
    deriving (Data,Typeable {-! CNode !-})    
-- | Assembler operand
--
-- @CAsmOperand argName? constraintExpr arg@ specifies an operand for an assembler
-- statement.
data CAsmOperand = CAsmOperand (Maybe Ident)   -- argument name
                                CStrLit        -- constraint expr
                                CExpr          -- argument
                                NodeInfo 
                   deriving (Data,Typeable {-! CNode !-})                                
-- | C99 Block items
--
--  Things that may appear in compound statements: either statements, declarations 
--   or nested function definitions.
data CBlockItem = CBlockStmt    CStat           -- ^ A statement
                | CBlockDecl    CDecl           -- ^ A local declaration
                | CNestedFunDef CFunDef         -- ^ A nested function (GNU C)
                  deriving (Data,Typeable {-! CNode !-})
-- | C declaration specifiers and qualifiers
--
-- Declaration specifiers include at most one storage-class specifier (C99 6.7.1),
-- type specifiers (6.7.2) and type qualifiers (6.7.3).
data CDeclSpec = CStorageSpec CStorageSpec  -- ^ storage-class specifier or typedef
               | CTypeSpec    CTypeSpec     -- ^ type name
               | CTypeQual    CTypeQual     -- ^ type qualifier
                 deriving (Data,Typeable {-! CNode !-})

-- | C storage class specifier (and typedefs) (K&R A8.1, C99 6.7.1)
--
data CStorageSpec = CAuto     NodeInfo     -- ^ automatic storage
                  | CRegister NodeInfo     -- ^ register storage
                  | CStatic   NodeInfo     -- ^ static linkage
                  | CExtern   NodeInfo     -- ^ external linkage
                  | CTypedef  NodeInfo     -- ^ a typedef
                  | CThread   NodeInfo     -- ^ GNUC thread local storage
                 deriving (Eq,Ord,Data,Typeable {-! CNode !-})
instance Show CStorageSpec where
    show (CAuto _) = "auto"
    show (CRegister _) = "register"
    show (CStatic _) = "static"
    show (CExtern _) = "extern"
    show (CTypedef _) = "typedef"
    show (CThread _) = "thread"
                        
-- | C type specifier (K&R A8.2, C99 6.7.2)
--
-- Type specifiers are either basic types such as @char@ or @int@, 
-- @struct@, @union@ or @enum@ specifiers or typedef names.
--
-- As a GNU extension, a @typeof@ expression also is a type specifier.
data CTypeSpec = CVoidType    NodeInfo
               | CCharType    NodeInfo
               | CShortType   NodeInfo
               | CIntType     NodeInfo
               | CLongType    NodeInfo
               | CFloatType   NodeInfo
               | CDoubleType  NodeInfo
               | CSignedType  NodeInfo
               | CUnsigType   NodeInfo
               | CBoolType    NodeInfo
               | CComplexType NodeInfo
               | CSUType      CStructUnion      
                              NodeInfo             -- ^ Struct or Union specifier
               | CEnumType    CEnum             
                              NodeInfo             -- ^ Enumeration specifier
               | CTypeDef     Ident
                              NodeInfo             -- ^ Typedef name
               | CTypeOfExpr  CExpr
                              NodeInfo             -- ^ @typeof(expr)@
               | CTypeOfType  CDecl
                              NodeInfo             -- ^ @typeof(type)@
               deriving (Data,Typeable {-! CNode !-})

-- | C type qualifiers (K&R A8.2, C99 6.7.3) and function specifiers (C99 6.7.4)
--
-- @const@, @volatile@ and @restrict@ type qualifiers and @inline@ function specifier.
-- Additionally, @__attribute__@ annotations for declarators and types.
data CTypeQual = CConstQual NodeInfo
               | CVolatQual NodeInfo
               | CRestrQual NodeInfo
               | CInlineQual NodeInfo
               | CAttrQual  CAttr
               deriving (Data,Typeable {-! CNode !-})

-- | C structure or union specifiers (K&R A8.3, C99 6.7.2.1)
--
-- @CStruct tag identifier struct-decls c-attrs@ represents a struct or union specifier (depending on @tag@).
--
--   * either t@identifier@ or the declaration list @struct-decls@ (or both) have to be present.
--     Example: in @struct foo x;@, the identifier is present, in @struct { int y; } x@ the declaration list, and
--     in @struct foo { int y; } x;@ both of them.
--
--   * @c-attrs@ is a list of @__attribute__@s associated with the struct or union specifier
data CStructUnion = CStruct CStructTag
                            (Maybe Ident)
                            (Maybe [CDecl])    -- member declarations
                            [CAttr]            -- __attribute__s
                            NodeInfo
                    deriving (Data,Typeable {-! CNode !-})

-- | a tag to determine wheter we refer to a @struct@ or @union@, see 'CStructUnion'.
data CStructTag = CStructTag
                | CUnionTag
                deriving (Eq,Data,Typeable)

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
data CEnum = CEnum (Maybe Ident)
                   (Maybe [(Ident,             -- variant name
                            Maybe CExpr)])     -- explicit variant value
                   [CAttr]                     -- __attribute__s
                   NodeInfo
             deriving (Data,Typeable {-! CNode !-})
          
-- | C initialization (K&R A8.7, C99 6.7.8)
-- 
-- Initializers are either assignment expressions or initializer lists 
-- (surrounded in curly braces), whose elements are themselves
-- initializers, paired with an optional list of designators.
data CInit = CInitExpr CExpr
                       NodeInfo            -- ^ assignment expression
           | CInitList CInitList
                       NodeInfo            -- ^ initialization list (see 'CInitList')
             deriving (Data,Typeable {-! CNode !-})

-- | Initializer List
--
-- The members of an initializer list are of the form @(designator-list,initializer)@.
-- @designator-list@ is allowed to be empty - in this case the initializer refers to the
-- ''next'' member of the compound type (see C99 6.7.8).
--
-- Examples (simplified expressions and identifiers):
--
-- > -- { [0], [3] = 4, [2] = 5, 8 } 
-- > let init1 = ([CArrDesig 0, CArrDesig 3], CInitExpr 4)
-- >     init2 = ([CArrDesig 2], CInitExpr 5)
-- >     init3 = ([], CInitExpr 8)
-- > in  CInitList [init1, init2, init3]
--
-- > -- { .s = { {2,3} , .a = { 1 } } }
-- > let init_1  = [ ([], CInitExpr 1) ]
-- >     init_23 = zip (repeat []) [CInitExpr 2, CInitExpr 3]
-- >     init_s_1 = ([], CInitList init_23)
-- >     init_s_a = ([CMemberDesig "a"], CInitList init_1)
-- >     init_s  = ((CMemberDesig "s"), CInitList [init_s_1,init_s_a])
-- > in  CInitList [init_s]
type CInitList = [([CDesignator], CInit)]
  
-- | Designators
--
-- A designator specifies a member of an object, either an element or range of an array,
-- or the named member of a struct \/ union.
data CDesignator = CArrDesig     CExpr
                                 NodeInfo  -- ^ array position designator
                 | CMemberDesig  Ident
                                 NodeInfo  -- ^ member designator
                 | CRangeDesig   CExpr  
                                 CExpr
                                 NodeInfo  -- ^ array range designator @CRangeDesig from to _@ (GNU C)
                   deriving (Data,Typeable {-! CNode !-})

-- | @__attribute__@ annotations
--
-- Those are of the form @CAttr attribute-name attribute-parameters@, 
-- and serve as generic properties of some syntax tree elements.
data CAttr = CAttr Ident [CExpr] NodeInfo
             deriving (Data,Typeable {-! CNode !-})

-- | C expression (K&R A7)
--
-- * these can be arbitrary expression, as the argument of `sizeof' can be
--   arbitrary, even if appearing in a constant expression
--
-- * GNU C extensions: @alignof@, @__real@, @__imag@, @({ stmt-expr })@, @&& label@ and built-ins 
--
data CExpr = CComma       [CExpr]       -- comma expression list, n >= 2
                          NodeInfo
           | CAssign      CAssignOp     -- assignment operator
                          CExpr         -- l-value
                          CExpr         -- r-value
                          NodeInfo
           | CCond        CExpr         -- conditional
                   (Maybe CExpr)        -- true-expression (GNU allows omitting)
                          CExpr         -- false-expression
                          NodeInfo
           | CBinary      CBinaryOp     -- binary operator
                          CExpr         -- lhs
                          CExpr         -- rhs
                          NodeInfo
           | CCast        CDecl         -- type name
                          CExpr
                          NodeInfo
           | CUnary       CUnaryOp      -- unary operator
                          CExpr
                          NodeInfo
           | CSizeofExpr  CExpr
                          NodeInfo
           | CSizeofType  CDecl         -- type name
                          NodeInfo
           | CAlignofExpr CExpr
                          NodeInfo
           | CAlignofType CDecl         -- type name
                          NodeInfo
           | CComplexReal CExpr         -- real part of complex number
                          NodeInfo 
           | CComplexImag CExpr         -- imaginary part of complex number
                          NodeInfo
           | CIndex       CExpr         -- array
                          CExpr         -- index
                          NodeInfo
           | CCall        CExpr         -- function
                          [CExpr]       -- arguments
                          NodeInfo
           | CMember      CExpr         -- structure
                          Ident         -- member name
                          Bool          -- deref structure? (True for `->')
                          NodeInfo
           | CVar         Ident         -- identifier (incl. enumeration const)
                          NodeInfo
           | CConst       CConst                -- includes strings
                          NodeInfo
           | CCompoundLit CDecl         -- C99 compound literal
                          CInitList     -- type name & initialiser list
                          NodeInfo
           | CStatExpr    CStat         -- GNUC compound statement as expr
                          NodeInfo
           | CLabAddrExpr Ident         -- GNUC address of label
                          NodeInfo
           | CBuiltinExpr CBuiltin      -- place holder for GNUC builtin exprs
           deriving (Data,Typeable {-! CNode !-})


-- | GNU Builtins, which cannot be typed in C99
data CBuiltin = 
          CBuiltinVaArg CExpr CDecl NodeInfo            -- ^ @(expr, type)@
        | CBuiltinOffsetOf CDecl [CDesignator] NodeInfo -- ^ @(type, designator-list)@
        | CBuiltinTypesCompatible CDecl CDecl NodeInfo  -- ^ @(type,type)@
        deriving (Data,Typeable {-! CNode !-})

-- | C constant (K&R A2.5 & A7.2)
data CConst = CIntConst   CInteger NodeInfo
            | CCharConst  CChar NodeInfo
            | CFloatConst CFloat NodeInfo
            | CStrConst   CString NodeInfo
            deriving (Data,Typeable {-! CNode !-})

-- | Attributed string literals
data CStrLit = CStrLit CString NodeInfo
            deriving (Data,Typeable {-! CNode !-})

cstringOfLit :: CStrLit -> CString
cstringOfLit (CStrLit cstr _) = cstr

-- | Lift a string literal to a C constant
liftStrLit :: CStrLit -> CConst
liftStrLit (CStrLit str at) = CStrConst str at



--------------------------------------------------------
-- DERIVES GENERATED CODE
-- DO NOT MODIFY BELOW THIS LINE
-- CHECKSUM: 37319874

instance CNode CTranslUnit
    where nodeInfo (CTranslUnit _ nodeinfo) = nodeinfo
instance Pos CTranslUnit
    where posOf x = posOfNode (nodeInfo x)

instance CNode CExtDecl
    where nodeInfo (CDeclExt d) = nodeInfo d
          nodeInfo (CFDefExt d) = nodeInfo d
          nodeInfo (CAsmExt d) = nodeInfo d
instance Pos CExtDecl
    where posOf x = posOfNode (nodeInfo x)

instance CNode CFunDef
    where nodeInfo (CFunDef _ _ _ _ nodeinfo) = nodeinfo
instance Pos CFunDef
    where posOf x = posOfNode (nodeInfo x)

instance CNode CDecl
    where nodeInfo (CDecl _ _ nodeinfo) = nodeinfo
instance Pos CDecl
    where posOf x = posOfNode (nodeInfo x)

instance CNode CDeclr
    where nodeInfo (CDeclr _ _ _ _ nodeinfo) = nodeinfo
instance Pos CDeclr
    where posOf x = posOfNode (nodeInfo x)

instance CNode CDerivedDeclr
    where nodeInfo (CPtrDeclr _ nodeinfo) = nodeinfo
          nodeInfo (CArrDeclr _ _ nodeinfo) = nodeinfo
          nodeInfo (CFunDeclr _ _ nodeinfo) = nodeinfo
instance Pos CDerivedDeclr
    where posOf x = posOfNode (nodeInfo x)

instance CNode CStat
    where nodeInfo (CLabel _ _ _ nodeinfo) = nodeinfo
          nodeInfo (CCase _ _ nodeinfo) = nodeinfo
          nodeInfo (CCases _ _ _ nodeinfo) = nodeinfo
          nodeInfo (CDefault _ nodeinfo) = nodeinfo
          nodeInfo (CExpr _ nodeinfo) = nodeinfo
          nodeInfo (CCompound _ _ nodeinfo) = nodeinfo
          nodeInfo (CIf _ _ _ nodeinfo) = nodeinfo
          nodeInfo (CSwitch _ _ nodeinfo) = nodeinfo
          nodeInfo (CWhile _ _ _ nodeinfo) = nodeinfo
          nodeInfo (CFor _ _ _ _ nodeinfo) = nodeinfo
          nodeInfo (CGoto _ nodeinfo) = nodeinfo
          nodeInfo (CGotoPtr _ nodeinfo) = nodeinfo
          nodeInfo (CCont nodeinfo) = nodeinfo
          nodeInfo (CBreak nodeinfo) = nodeinfo
          nodeInfo (CReturn _ nodeinfo) = nodeinfo
          nodeInfo (CAsm _ nodeinfo) = nodeinfo
instance Pos CStat
    where posOf x = posOfNode (nodeInfo x)

instance CNode CAsmStmt
    where nodeInfo (CAsmStmt _ _ _ _ _ nodeinfo) = nodeinfo
instance Pos CAsmStmt
    where posOf x = posOfNode (nodeInfo x)

instance CNode CAsmOperand
    where nodeInfo (CAsmOperand _ _ _ nodeinfo) = nodeinfo
instance Pos CAsmOperand
    where posOf x = posOfNode (nodeInfo x)

instance CNode CBlockItem
    where nodeInfo (CBlockStmt d) = nodeInfo d
          nodeInfo (CBlockDecl d) = nodeInfo d
          nodeInfo (CNestedFunDef d) = nodeInfo d
instance Pos CBlockItem
    where posOf x = posOfNode (nodeInfo x)

instance CNode CDeclSpec
    where nodeInfo (CStorageSpec d) = nodeInfo d
          nodeInfo (CTypeSpec d) = nodeInfo d
          nodeInfo (CTypeQual d) = nodeInfo d
instance Pos CDeclSpec
    where posOf x = posOfNode (nodeInfo x)

instance CNode CStorageSpec
    where nodeInfo (CAuto nodeinfo) = nodeinfo
          nodeInfo (CRegister nodeinfo) = nodeinfo
          nodeInfo (CStatic nodeinfo) = nodeinfo
          nodeInfo (CExtern nodeinfo) = nodeinfo
          nodeInfo (CTypedef nodeinfo) = nodeinfo
          nodeInfo (CThread nodeinfo) = nodeinfo
instance Pos CStorageSpec
    where posOf x = posOfNode (nodeInfo x)

instance CNode CTypeSpec
    where nodeInfo (CVoidType nodeinfo) = nodeinfo
          nodeInfo (CCharType nodeinfo) = nodeinfo
          nodeInfo (CShortType nodeinfo) = nodeinfo
          nodeInfo (CIntType nodeinfo) = nodeinfo
          nodeInfo (CLongType nodeinfo) = nodeinfo
          nodeInfo (CFloatType nodeinfo) = nodeinfo
          nodeInfo (CDoubleType nodeinfo) = nodeinfo
          nodeInfo (CSignedType nodeinfo) = nodeinfo
          nodeInfo (CUnsigType nodeinfo) = nodeinfo
          nodeInfo (CBoolType nodeinfo) = nodeinfo
          nodeInfo (CComplexType nodeinfo) = nodeinfo
          nodeInfo (CSUType _ nodeinfo) = nodeinfo
          nodeInfo (CEnumType _ nodeinfo) = nodeinfo
          nodeInfo (CTypeDef _ nodeinfo) = nodeinfo
          nodeInfo (CTypeOfExpr _ nodeinfo) = nodeinfo
          nodeInfo (CTypeOfType _ nodeinfo) = nodeinfo
instance Pos CTypeSpec
    where posOf x = posOfNode (nodeInfo x)

instance CNode CTypeQual
    where nodeInfo (CConstQual nodeinfo) = nodeinfo
          nodeInfo (CVolatQual nodeinfo) = nodeinfo
          nodeInfo (CRestrQual nodeinfo) = nodeinfo
          nodeInfo (CInlineQual nodeinfo) = nodeinfo
          nodeInfo (CAttrQual d) = nodeInfo d
instance Pos CTypeQual
    where posOf x = posOfNode (nodeInfo x)

instance CNode CStructUnion
    where nodeInfo (CStruct _ _ _ _ nodeinfo) = nodeinfo
instance Pos CStructUnion
    where posOf x = posOfNode (nodeInfo x)

instance CNode CEnum
    where nodeInfo (CEnum _ _ _ nodeinfo) = nodeinfo
instance Pos CEnum
    where posOf x = posOfNode (nodeInfo x)

instance CNode CInit
    where nodeInfo (CInitExpr _ nodeinfo) = nodeinfo
          nodeInfo (CInitList _ nodeinfo) = nodeinfo
instance Pos CInit
    where posOf x = posOfNode (nodeInfo x)

instance CNode CDesignator
    where nodeInfo (CArrDesig _ nodeinfo) = nodeinfo
          nodeInfo (CMemberDesig _ nodeinfo) = nodeinfo
          nodeInfo (CRangeDesig _ _ nodeinfo) = nodeinfo
instance Pos CDesignator
    where posOf x = posOfNode (nodeInfo x)

instance CNode CAttr
    where nodeInfo (CAttr _ _ nodeinfo) = nodeinfo
instance Pos CAttr
    where posOf x = posOfNode (nodeInfo x)

instance CNode CExpr
    where nodeInfo (CComma _ nodeinfo) = nodeinfo
          nodeInfo (CAssign _ _ _ nodeinfo) = nodeinfo
          nodeInfo (CCond _ _ _ nodeinfo) = nodeinfo
          nodeInfo (CBinary _ _ _ nodeinfo) = nodeinfo
          nodeInfo (CCast _ _ nodeinfo) = nodeinfo
          nodeInfo (CUnary _ _ nodeinfo) = nodeinfo
          nodeInfo (CSizeofExpr _ nodeinfo) = nodeinfo
          nodeInfo (CSizeofType _ nodeinfo) = nodeinfo
          nodeInfo (CAlignofExpr _ nodeinfo) = nodeinfo
          nodeInfo (CAlignofType _ nodeinfo) = nodeinfo
          nodeInfo (CComplexReal _ nodeinfo) = nodeinfo
          nodeInfo (CComplexImag _ nodeinfo) = nodeinfo
          nodeInfo (CIndex _ _ nodeinfo) = nodeinfo
          nodeInfo (CCall _ _ nodeinfo) = nodeinfo
          nodeInfo (CMember _ _ _ nodeinfo) = nodeinfo
          nodeInfo (CVar _ nodeinfo) = nodeinfo
          nodeInfo (CConst _ nodeinfo) = nodeinfo
          nodeInfo (CCompoundLit _ _ nodeinfo) = nodeinfo
          nodeInfo (CStatExpr _ nodeinfo) = nodeinfo
          nodeInfo (CLabAddrExpr _ nodeinfo) = nodeinfo
          nodeInfo (CBuiltinExpr d) = nodeInfo d
instance Pos CExpr
    where posOf x = posOfNode (nodeInfo x)

instance CNode CBuiltin
    where nodeInfo (CBuiltinVaArg _ _ nodeinfo) = nodeinfo
          nodeInfo (CBuiltinOffsetOf _ _ nodeinfo) = nodeinfo
          nodeInfo (CBuiltinTypesCompatible _ _ nodeinfo) = nodeinfo
instance Pos CBuiltin
    where posOf x = posOfNode (nodeInfo x)

instance CNode CConst
    where nodeInfo (CIntConst _ nodeinfo) = nodeinfo
          nodeInfo (CCharConst _ nodeinfo) = nodeinfo
          nodeInfo (CFloatConst _ nodeinfo) = nodeinfo
          nodeInfo (CStrConst _ nodeinfo) = nodeinfo
instance Pos CConst
    where posOf x = posOfNode (nodeInfo x)

instance CNode CStrLit
    where nodeInfo (CStrLit _ nodeinfo) = nodeinfo
instance Pos CStrLit
    where posOf x = posOfNode (nodeInfo x)
