-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.AST.AST
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
module Language.C.AST.AST (
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
  CConst (..), CStrLit(..), liftStrLit, 
  CBuiltin(..),
  -- * Summary nodes
  CObj(..),CTag(..),CDef(..),
) where
import Data.List
import Language.C.Toolkit.Position   (Pos(posOf))
import Language.C.Toolkit.Idents     (Ident)
import Language.C.Toolkit.Node (NodeInfo,CNode(..))
import Language.C.AST.Constants

-- | Complete C tranlsation unit (C99 6.9, K&R A10)
--  
-- A complete C translation unit, for example representing a C header or source file.
-- It consists of a list of external (i.e. toplevel) declarations.
data CTranslUnit = CTranslUnit [CExtDecl] NodeInfo
instance CNode CTranslUnit where
  nodeInfo (CTranslUnit _ at) = at
instance Pos CTranslUnit where
  posOf (CTranslUnit _ at) = posOf at
instance Eq CTranslUnit where
  (CTranslUnit _ at1) == (CTranslUnit _ at2) = at1 == at2
    
-- | External C declaration (C99 6.9, K&R A10)
--
-- Either a toplevel declaration, function definition or external assembler.
data CExtDecl = CDeclExt CDecl
              | CFDefExt CFunDef
              | CAsmExt  CStrLit

instance Pos CExtDecl where
  posOf (CDeclExt decl) = posOf decl
  posOf (CFDefExt fdef) = posOf fdef
  posOf (CAsmExt asm)  = posOf asm

instance Eq CExtDecl where
  CDeclExt decl1 == CDeclExt decl2 = decl1 == decl2
  CFDefExt fdef1 == CFDefExt fdef2 = fdef1 == fdef2
  CAsmExt asm1   == CAsmExt asm2   = asm1 == asm2
  _              == _              = False
instance CNode CExtDecl where
  nodeInfo (CDeclExt decl) = nodeInfo decl
  nodeInfo (CFDefExt funDef) = nodeInfo funDef
  nodeInfo (CAsmExt asm) = nodeInfo asm

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
--
-- /TODO/: Merge oldstyle and newstyle declarations
data CFunDef = CFunDef [CDeclSpec]      -- type specifier and qualifier
                       CDeclr           -- declarator
                       [CDecl]          -- optional declaration list
                       CStat            -- compound statement
                       NodeInfo
instance Pos CFunDef where
  posOf (CFunDef _declspecs _declr _oldstyleparams _stat at) = posOf at
instance Eq CFunDef where
  CFunDef _ _ _ _ at1 == CFunDef _ _ _ _ at2 = at1 == at2
instance CNode CFunDef where
  nodeInfo (CFunDef _ _ _ _ at) = at

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
instance CNode CDecl where
  nodeInfo (CDecl _ _ at) = at
instance Pos CDecl where
  posOf (CDecl _ _ at) = posOf at
instance Eq CDecl where
  (CDecl _ _ at1) == (CDecl _ _ at2) = at1 == at2

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
            | CFunDeclr CFunParams [CAttr] NodeInfo
              -- ^ Function declarator @CFunDeclr declr (old-style-params | new-style-params) c-attrs@ 
type CFunParams = (Either [Ident] ([CDecl],Bool))
instance Pos CDeclr where
  posOf (CDeclr _ _ _ _ at) = posOf at
instance Pos CDerivedDeclr where
  posOf (CPtrDeclr _typeQuals at) = posOf at
  posOf (CArrDeclr _typeQuals _arraySize at) = posOf at
  posOf (CFunDeclr _parameters _cAttrs at) = posOf at

instance Eq CDeclr where
  (CDeclr _ _ _ _ at1) == (CDeclr _ _ _ _ at2) = at1 == at2
instance Eq CDerivedDeclr where
  (CPtrDeclr _   at1) == (CPtrDeclr _   at2) = at1 == at2
  (CArrDeclr _ _ at1) == (CArrDeclr _ _ at2) = at1 == at2
  (CFunDeclr _ _ at1) == (CFunDeclr _ _ at2) = at1 == at2
  _                     == _                     = False

-- | C statement (K&R A9, C99 6.8)
--
data CStat = CLabel    Ident            -- label                        
                       CStat            
                       [CAttr]          
                       NodeInfo            -- ^ An (attributed) label followed by a statement
           | CCase     CExpr            -- constant expression
                       CStat            
                       NodeInfo            -- ^ A statement of the form @case expr : stmt@
           | CCases    CExpr            
                       CExpr            
                       CStat
                       NodeInfo            -- ^ A case range of the form @case lower ... upper : stmt@
           | CDefault  CStat            
                       NodeInfo            -- ^ The default case @default : stmt@
           | CExpr     (Maybe CExpr)    
                        NodeInfo           -- ^ A simple statement, that is in C: evaluating an expression with side-effects
                                        --   and discarding the result.
           | CCompound [Ident]          
                       [CBlockItem]     
                        NodeInfo           -- ^ compound statement @CCompound localLabels blockItems at@
           | CIf       CExpr            
                       CStat            
                (Maybe CStat)    
                       NodeInfo            -- ^ conditional statement @CIf ifExpr thenStmt maybeElseStmt at@
           | CSwitch   CExpr            
                       CStat
                       NodeInfo            -- ^ switch statement @CSwitch selectorExpr switchStmt@, where @switchStmt@ usually includes
                                        -- /case/, /break/ and /default/ statements
           | CWhile    CExpr
                       CStat
                       Bool         
                       NodeInfo            -- ^ while or do-while statement @CWhile guard stmt isDoWhile at@
           | CFor      (Either (Maybe CExpr) CDecl)
                           (Maybe CExpr)
                           (Maybe CExpr)
                       CStat
                       NodeInfo            -- ^ for statement @CFor init expr-2 expr-3 stmt@, where @init@ is either a declaration or
                                        -- initializing expression
           | CGoto     Ident            -- 
                       NodeInfo            -- ^ goto statement @CGoto label@
           | CGotoPtr  CExpr            
                       NodeInfo            -- ^ computed goto @CGotoPtr labelExpr@
           | CCont     NodeInfo            -- ^ continue statement
           | CBreak    NodeInfo            -- ^ break statement
           | CReturn   (Maybe CExpr)
                       NodeInfo            -- ^ return statement @CReturn returnExpr@
           | CAsm      CAsmStmt         
                       NodeInfo            -- ^ assembly statement 


instance Pos CStat where
  posOf (CLabel    _ _ _   at) = posOf at
  posOf (CCase     _ _     at) = posOf at
  posOf (CCases    _ _ _   at) = posOf at
  posOf (CDefault  _       at) = posOf at
  posOf (CExpr     _       at) = posOf at
  posOf (CCompound _ _     at) = posOf at
  posOf (CIf       _ _ _   at) = posOf at
  posOf (CSwitch   _ _     at) = posOf at
  posOf (CWhile    _ _ _   at) = posOf at
  posOf (CFor      _ _ _ _ at) = posOf at
  posOf (CGoto     _       at) = posOf at
  posOf (CGotoPtr     _    at) = posOf at
  posOf (CCont             at) = posOf at
  posOf (CBreak            at) = posOf at
  posOf (CReturn   _       at) = posOf at
  posOf (CAsm _            at) = posOf at

instance Eq CStat where
  (CLabel    _ _ _   at1) == (CLabel    _ _ _   at2) = at1 == at2
  (CCase     _ _     at1) == (CCase     _ _     at2) = at1 == at2
  (CCases    _ _ _   at1) == (CCases    _ _ _   at2) = at1 == at2
  (CDefault  _       at1) == (CDefault  _       at2) = at1 == at2
  (CExpr     _       at1) == (CExpr     _       at2) = at1 == at2
  (CCompound _ _     at1) == (CCompound _ _     at2) = at1 == at2
  (CIf       _ _ _   at1) == (CIf       _ _ _   at2) = at1 == at2
  (CSwitch   _ _     at1) == (CSwitch   _ _     at2) = at1 == at2
  (CWhile    _ _ _   at1) == (CWhile    _ _ _   at2) = at1 == at2
  (CFor      _ _ _ _ at1) == (CFor      _ _ _ _ at2) = at1 == at2
  (CGoto     _       at1) == (CGoto     _       at2) = at1 == at2
  (CGotoPtr  _       at1) == (CGotoPtr  _       at2) = at1 == at2
  (CCont             at1) == (CCont             at2) = at1 == at2
  (CBreak            at1) == (CBreak            at2) = at1 == at2
  (CReturn   _       at1) == (CReturn   _       at2) = at1 == at2
  (CAsm _            at1) == (CAsm _            at2) = at1 == at2
  _                       == _                       = False  
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
instance Pos CAsmStmt where
    posOf (CAsmStmt _ _ _ _ _ at) = posOf at
    
-- | Assembler operand
--
-- @CAsmOperand argName? constraintExpr arg@ specifies an operand for an assembler
-- statement.
data CAsmOperand = CAsmOperand (Maybe Ident)   -- argument name
                                CStrLit        -- constraint expr
                                CExpr          -- argument
                                NodeInfo
instance Pos CAsmOperand where
    posOf (CAsmOperand _ _ _ at) = posOf at                            

-- | C99 Block items
--
--  Things that may appear in compound statements: either statements, declarations 
--   or nested function definitions.
data CBlockItem = CBlockStmt    CStat           -- ^ A statement
                | CBlockDecl    CDecl           -- ^ A local declaration
                | CNestedFunDef CFunDef         -- ^ A nested function (GNU C)

instance Pos CBlockItem where
  posOf (CBlockStmt stmt)  = posOf stmt
  posOf (CBlockDecl decl)  = posOf decl
  posOf (CNestedFunDef fdef) = posOf fdef

instance Eq CBlockItem where
  CBlockStmt    stmt1 == CBlockStmt    stmt2 = stmt1 == stmt2
  CBlockDecl    decl1 == CBlockDecl    decl2 = decl1 == decl2
  CNestedFunDef fdef1 == CNestedFunDef fdef2 = fdef1 == fdef2
  _                   == _                   = False


-- | C declaration specifiers and qualifiers
--
-- Declaration specifiers include at most one storage-class specifier (C99 6.7.1),
-- type specifiers (6.7.2) and type qualifiers (6.7.3).
data CDeclSpec = CStorageSpec CStorageSpec  -- ^ storage-class specifier or typedef
               | CTypeSpec    CTypeSpec     -- ^ type name
               | CTypeQual    CTypeQual     -- ^ type qualifier
--               deriving (Eq)

instance Pos CDeclSpec where
  posOf (CStorageSpec sspec) = posOf sspec
  posOf (CTypeSpec    tspec) = posOf tspec
  posOf (CTypeQual    tqual) = posOf tqual

-- | C storage class specifier (and typedefs) (K&R A8.1, C99 6.7.1)
--
data CStorageSpec = CAuto     NodeInfo     -- ^ automatic storage
                  | CRegister NodeInfo     -- ^ register storage
                  | CStatic   NodeInfo     -- ^ static linkage
                  | CExtern   NodeInfo     -- ^ external linkage
                  | CTypedef  NodeInfo     -- ^ a typedef
                  | CThread   NodeInfo     -- ^ GNUC thread local storage
                  
instance Pos CStorageSpec where
  posOf (CAuto     at) = posOf at
  posOf (CRegister at) = posOf at
  posOf (CStatic   at) = posOf at
  posOf (CExtern   at) = posOf at
  posOf (CTypedef  at) = posOf at
  posOf (CThread   at) = posOf at

instance Eq CStorageSpec where
  (CAuto     at1) == (CAuto     at2) = at1 == at2
  (CRegister at1) == (CRegister at2) = at1 == at2
  (CStatic   at1) == (CStatic   at2) = at1 == at2
  (CExtern   at1) == (CExtern   at2) = at1 == at2
  (CTypedef  at1) == (CTypedef  at2) = at1 == at2
  (CThread   at1) == (CThread   at2) = at1 == at2
  _               == _               = False
  
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

instance Pos CTypeSpec where
  posOf (CVoidType      at) = posOf at
  posOf (CCharType      at) = posOf at
  posOf (CShortType     at) = posOf at
  posOf (CIntType       at) = posOf at
  posOf (CLongType      at) = posOf at
  posOf (CFloatType     at) = posOf at
  posOf (CDoubleType    at) = posOf at
  posOf (CSignedType    at) = posOf at
  posOf (CUnsigType     at) = posOf at
  posOf (CBoolType      at) = posOf at
  posOf (CComplexType   at) = posOf at
  posOf (CSUType     _  at) = posOf at
  posOf (CEnumType   _  at) = posOf at
  posOf (CTypeDef    _  at) = posOf at
  posOf (CTypeOfExpr _  at) = posOf at
  posOf (CTypeOfType _  at) = posOf at

instance Eq CTypeSpec where
  (CVoidType     at1) == (CVoidType     at2) = at1 == at2
  (CCharType     at1) == (CCharType     at2) = at1 == at2
  (CShortType    at1) == (CShortType    at2) = at1 == at2
  (CIntType      at1) == (CIntType      at2) = at1 == at2
  (CLongType     at1) == (CLongType     at2) = at1 == at2
  (CFloatType    at1) == (CFloatType    at2) = at1 == at2
  (CDoubleType   at1) == (CDoubleType   at2) = at1 == at2
  (CSignedType   at1) == (CSignedType   at2) = at1 == at2
  (CUnsigType    at1) == (CUnsigType    at2) = at1 == at2
  (CBoolType     at1) == (CBoolType     at2) = at1 == at2
  (CComplexType  at1) == (CComplexType  at2) = at1 == at2
  (CSUType     _ at1) == (CSUType     _ at2) = at1 == at2
  (CEnumType   _ at1) == (CEnumType   _ at2) = at1 == at2
  (CTypeDef    _ at1) == (CTypeDef    _ at2) = at1 == at2
  (CTypeOfExpr _ at1) == (CTypeOfExpr _ at2) = at1 == at2
  (CTypeOfType _ at1) == (CTypeOfType _ at2) = at1 == at2
  _                   == _                   = False
  
-- | C type qualifiers (K&R A8.2, C99 6.7.3) and function specifiers (C99 6.7.4)
--
-- @const@, @volatile@ and @restrict@ type qualifiers and @inline@ function specifier.
-- Additionally, @__attribute__@ annotations for declarators and types.
data CTypeQual = CConstQual NodeInfo
               | CVolatQual NodeInfo
               | CRestrQual NodeInfo
               | CInlinQual NodeInfo
               | CAttrQual  CAttr

instance Pos CTypeQual where
 posOf (CConstQual at) = posOf at
 posOf (CVolatQual at) = posOf at
 posOf (CRestrQual at) = posOf at
 posOf (CInlinQual at) = posOf at
 posOf (CAttrQual cattrs) = posOf cattrs
instance Eq CTypeQual where
  (CConstQual at1) == (CConstQual at2) = at1 == at2
  (CVolatQual at1) == (CVolatQual at2) = at1 == at2
  (CRestrQual at1) == (CRestrQual at2) = at1 == at2
  (CInlinQual at1) == (CInlinQual at2) = at1 == at2
  (CAttrQual at1)  == (CAttrQual at2)  = at1 == at2
  _                == _                = False
  
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

instance Pos CStructUnion where
  posOf (CStruct _ _ _ _ at) = posOf at

instance Eq CStructUnion where
  (CStruct _ _ _ _ at1) == (CStruct _ _ _ _ at2) = at1 == at2

-- | a tag to determine wheter we refer to a @struct@ or @union@, see 'CStructUnion'.
data CStructTag = CStructTag
                | CUnionTag
                deriving (Eq)

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

instance Pos CEnum where
  posOf (CEnum _ _ _ at) = posOf at

instance Eq CEnum where
  (CEnum _ _ _ at1) == (CEnum _ _ _ at2) = at1 == at2


          
-- | C initialization (K&R A8.7, C99 6.7.8)
-- 
-- Initializers are either assignment expressions or initializer lists 
-- (surrounded in curly braces), whose elements are themselves
-- initializers, paired with an optional list of designators.
data CInit = CInitExpr CExpr
                       NodeInfo            -- ^ assignment expression
           | CInitList CInitList
                       NodeInfo            -- ^ initialization list (see 'CInitList')

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

instance Pos CInit where
  posOf (CInitExpr _ at) = posOf at
  posOf (CInitList _ at) = posOf at

instance Eq CInit where
  (CInitExpr _ at1) == (CInitExpr _ at2) = at1 == at2
  (CInitList _ at1) == (CInitList _ at2) = at1 == at2
  _                 == _                 = False
  
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

instance Pos CDesignator where
  posOf (CArrDesig     _ at) = posOf at
  posOf (CMemberDesig  _ at) = posOf at
  posOf (CRangeDesig _ _ at) = posOf at

instance Eq CDesignator where
  (CArrDesig     _ at1) == (CArrDesig     _ at2) = at1 == at2
  (CMemberDesig  _ at1) == (CMemberDesig  _ at2) = at1 == at2
  (CRangeDesig _ _ at1) == (CRangeDesig _ _ at2) = at1 == at2
  _                     == _                     = False


-- | @__attribute__@ annotations
--
-- Those are of the form @CAttr attribute-name attribute-parameters@, 
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
data CAttr = CAttr Ident [CExpr] NodeInfo
instance Pos CAttr where
    posOf (CAttr _ _ at)    = posOf at
instance Eq CAttr where
    (CAttr _ _ at1) == (CAttr _ _ at2) = at1 == at2

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

instance Pos CExpr where
  posOf (CComma       _     at) = posOf at
  posOf (CAssign      _ _ _ at) = posOf at
  posOf (CCond        _ _ _ at) = posOf at
  posOf (CBinary      _ _ _ at) = posOf at
  posOf (CCast        _ _   at) = posOf at
  posOf (CUnary       _ _   at) = posOf at
  posOf (CSizeofExpr  _     at) = posOf at
  posOf (CSizeofType  _     at) = posOf at
  posOf (CAlignofExpr _     at) = posOf at
  posOf (CAlignofType _     at) = posOf at
  posOf (CComplexReal _     at) = posOf at
  posOf (CComplexImag _     at) = posOf at
  posOf (CIndex       _ _   at) = posOf at
  posOf (CCall        _ _   at) = posOf at
  posOf (CMember      _ _ _ at) = posOf at
  posOf (CVar         _     at) = posOf at
  posOf (CConst       _     at) = posOf at
  posOf (CCompoundLit _ _   at) = posOf at
  posOf (CStatExpr    _     at) = posOf at
  posOf (CLabAddrExpr _     at) = posOf at
  posOf (CBuiltinExpr       bt) = posOf bt

instance Eq CExpr where
  (CComma       _     at1) == (CComma       _     at2) = at1 == at2
  (CAssign      _ _ _ at1) == (CAssign      _ _ _ at2) = at1 == at2
  (CCond        _ _ _ at1) == (CCond        _ _ _ at2) = at1 == at2
  (CBinary      _ _ _ at1) == (CBinary      _ _ _ at2) = at1 == at2
  (CCast        _ _   at1) == (CCast        _ _   at2) = at1 == at2
  (CUnary       _ _   at1) == (CUnary       _ _   at2) = at1 == at2
  (CSizeofExpr  _     at1) == (CSizeofExpr  _     at2) = at1 == at2
  (CSizeofType  _     at1) == (CSizeofType  _     at2) = at1 == at2
  (CAlignofExpr _     at1) == (CAlignofExpr _     at2) = at1 == at2
  (CAlignofType _     at1) == (CAlignofType _     at2) = at1 == at2
  (CComplexReal _     at1) == (CComplexReal _     at2) = at1 == at2
  (CComplexImag _     at1) == (CComplexImag _     at2) = at1 == at2
  (CIndex       _ _   at1) == (CIndex       _ _   at2) = at1 == at2
  (CCall        _ _   at1) == (CCall        _ _   at2) = at1 == at2
  (CMember      _ _ _ at1) == (CMember      _ _ _ at2) = at1 == at2
  (CVar         _     at1) == (CVar         _     at2) = at1 == at2
  (CConst       _     at1) == (CConst       _     at2) = at1 == at2
  (CCompoundLit _ _   at1) == (CCompoundLit _ _   at2) = at1 == at2
  (CStatExpr    _     at1) == (CStatExpr    _     at2) = at1 == at2
  (CLabAddrExpr _     at1) == (CLabAddrExpr _     at2) = at1 == at2
  (CBuiltinExpr builtin1)  == (CBuiltinExpr builtin2)  = builtin1 == builtin2
  _                        == _                        = False 
-- | GNU Builtins, which cannot be typed in C99
data CBuiltin = 
          CBuiltinVaArg CExpr CDecl NodeInfo            -- ^ @(expr, type)@
        | CBuiltinOffsetOf CDecl [CDesignator] NodeInfo -- ^ @(type, designator-list)@
        | CBuiltinTypesCompatible CDecl CDecl NodeInfo  -- ^ @(type,type)@
instance Pos CBuiltin where
    posOf (CBuiltinVaArg _ _ at) = posOf at
    posOf (CBuiltinOffsetOf _ _ at) = posOf at
    posOf (CBuiltinTypesCompatible _ _ at) = posOf at
instance Eq CBuiltin where
    (==) (CBuiltinVaArg _ _           at1) (CBuiltinVaArg _ _           at2) = at1 == at2
    (==) (CBuiltinOffsetOf _ _        at1) (CBuiltinOffsetOf _ _        at2) = at1 == at2
    (==) (CBuiltinTypesCompatible _ _ at1) (CBuiltinTypesCompatible _ _ at2) = at1 == at2
    (==) _ _ = False
    
-- | C assignment operators (K&R A7.17)
data CAssignOp = CAssignOp
               | CMulAssOp
               | CDivAssOp
               | CRmdAssOp              -- ^ remainder and assignment
               | CAddAssOp
               | CSubAssOp
               | CShlAssOp
               | CShrAssOp
               | CAndAssOp
               | CXorAssOp
               | COrAssOp
               deriving (Eq)

-- | C binary operators (K&R A7.6-15)
--
data CBinaryOp = CMulOp
               | CDivOp
               | CRmdOp                 -- ^ remainder of division
               | CAddOp
               | CSubOp
               | CShlOp                 -- ^ shift left
               | CShrOp                 -- ^ shift right
               | CLeOp                  -- ^ less
               | CGrOp                  -- ^ greater
               | CLeqOp                 -- ^ less or equal
               | CGeqOp                 -- ^ greater or equal
               | CEqOp                  -- ^ equal
               | CNeqOp                 -- ^ not equal
               | CAndOp                 -- ^ bitwise and
               | CXorOp                 -- ^ exclusive bitwise or
               | COrOp                  -- ^ inclusive bitwise or
               | CLndOp                 -- ^ logical and
               | CLorOp                 -- ^ logical or
               deriving (Eq)

-- | C unary operator (K&R A7.3-4)
--
data CUnaryOp = CPreIncOp               -- ^ prefix increment operator
              | CPreDecOp               -- ^ prefix decrement operator
              | CPostIncOp              -- ^ postfix increment operator
              | CPostDecOp              -- ^ postfix decrement operator
              | CAdrOp                  -- ^ address operator
              | CIndOp                  -- ^ indirection operator
              | CPlusOp                 -- ^ prefix plus
              | CMinOp                  -- ^ prefix minus
              | CCompOp                 -- ^ one's complement
              | CNegOp                  -- ^ logical negation
              deriving (Eq)

-- | C constant (K&R A2.5 & A7.2)
--
-- see 'Language.C.AST.Constants'
data CConst = CIntConst   CInteger NodeInfo
            | CCharConst  CChar    NodeInfo
            | CFloatConst CFloat   NodeInfo
            | CStrConst   CString  NodeInfo

instance Pos CConst where
  posOf (CIntConst   _ at) = posOf at
  posOf (CCharConst  _ at) = posOf at
  posOf (CFloatConst _ at) = posOf at
  posOf (CStrConst   _ at) = posOf at

instance Eq CConst where
  (CIntConst   _ at1) == (CIntConst   _ at2) = at1 == at2
  (CCharConst  _ at1) == (CCharConst  _ at2) = at1 == at2
  (CFloatConst _ at1) == (CFloatConst _ at2) = at1 == at2
  (CStrConst   _ at1) == (CStrConst   _ at2) = at1 == at2
  _                   == _                   = False
  
-- | Attributed string literals
data CStrLit = CStrLit CString NodeInfo
instance Pos CStrLit where
    posOf (CStrLit _ at) = posOf at
instance Eq CStrLit where
  (==) (CStrLit _ at1) (CStrLit _ at2) = at1 == at2
instance CNode CStrLit where
    nodeInfo (CStrLit _ at) = at

-- | Lift a string literal to a C constant
liftStrLit :: CStrLit -> CConst
liftStrLit (CStrLit str at) = CStrConst str at

-- | A @CObj@ is either a declaration, a type definition or an enumerator definition.
--
-- /TODO/: Stub from the analysis code, maybe remove
data CObj = TypeCO    CDecl             -- ^ typedef declaration
          | ObjCO     CDecl             -- ^ object or function declaration
          | EnumCO    Ident CEnum       -- ^ enumerator
          | BuiltinCO                   -- ^ builtin object
-- | A @CTag@ is a struct, union or enum declaration 
--
-- /TODO/: Stub from the analysis code, maybe remove
data CTag = StructUnionCT CStructUnion  -- ^ toplevel struct-union declaration
          | EnumCT        CEnum         -- ^ toplevel enum declaration

-- | A @CDef@ is something an identifier can be associated with -
--   it can either be undefined, an object or and struct/union/enum tag.
--
-- /TODO/: Stub from the analysis code, maybe remove
data CDef = UndefCD                     -- ^ undefined object
          | DontCareCD                  -- ^ don't care object
          | ObjCD      CObj             -- ^ C object
          | TagCD      CTag             -- ^ C tag
