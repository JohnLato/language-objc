{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.AST.Pretty
-- Copyright   :  Copyright (c) 2007 Bertram Felgenhauer
--                          (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- This module provides a pretty printer for the AST ('Language.C.AST.AST').
-----------------------------------------------------------------------------
module Language.C.AST.Pretty (
    Pretty (..),
    prettyUsingInclude
) where
import Data.List (partition,nub,isSuffixOf)
import qualified Data.Set as Set
import Language.C.AST.AST
import Language.C.AST.Constants
import Language.C.Toolkit.Idents
import Language.C.Toolkit.Position
import Text.PrettyPrint.HughesPJ


-- Pretty class
-- TODO: move
class Pretty p where
    pretty     :: p -> Doc
    prettyPrec :: Int -> p -> Doc

    pretty       = prettyPrec 0
    prettyPrec _ = pretty

-- pretty print optional chunk
maybeP :: (p -> Doc) -> Maybe p -> Doc
maybeP = maybe empty

-- pretty print identifier
identP :: Ident -> Doc
identP = text . identToLexeme

-- analogous to showParen
parenPrec :: Int -> Int -> Doc -> Doc
parenPrec prec prec2 t = if prec <= prec2 then t else parens t

-- indent a chunk of code
ii :: Doc -> Doc
ii = nest 4

-- Pretty instances
instance Pretty CHeader where
    pretty (CHeader edecls _) = vcat (map pretty edecls)

prettyUsingInclude :: CHeader -> Doc
prettyUsingInclude cast@(CHeader edecls _) =
  includeWarning headerFiles
    $$
  (vcat $ map (either includeHeader pretty) mappedDecls)
  where
    (headerFiles,mappedDecls) = foldr addDecl (Set.empty,[]) $ map tagIncludedDecls edecls
    tagIncludedDecls edecl | isHeaderFile ((posFile . posOf) edecl) = Left ((posFile . posOf) edecl)
                           | otherwise = Right edecl          
    addDecl decl@(Left headerRef) (headerSet, ds) 
      | Set.member headerRef headerSet = (headerSet, ds)
      | otherwise = (Set.insert headerRef headerSet, decl : ds)
    addDecl decl (headerSet,ds) = (headerSet, decl : ds)                                        
    includeHeader hFile = text "#include" <+> doubleQuotes (text hFile)
    isHeaderFile = (".h" `isSuffixOf`)
    includeWarning hs | Set.null hs = empty
                      | otherwise = text "/* Warning: The #include directives in this file aren't neccessarily correct. */"

instance Pretty CExtDecl where
    pretty (CDeclExt decl) = pretty decl <> semi
    pretty (CFDefExt fund) = pretty fund
    pretty (CAsmExt  _   ) = text "int __asm__ext__todo;"

instance Pretty CFunDef where
    pretty (CFunDef declspecs declr decls stat _) =          -- Example:
        hsep (map pretty declspecs)                          -- extern long
        <+> pretty declr                                     -- foo(int a, b)
        $+$ (ii . vcat . map (<> semi) . map pretty) decls   --     register long b;
        $$ prettyPrec (-1) stat                              -- {  ... 
                                                             -- }

instance Pretty CStat where
    pretty (CLabel ident stat _) = identP ident <> text ":" $$ pretty stat
    pretty (CCase expr stat _) =
        text "case" <+> pretty expr <> text ":" $$ pretty stat
    pretty (CCases expr1 expr2 stat _) =
        text "case" <+> pretty expr1 <> text ".."
                    <>  pretty expr2 <> text ":" $$ pretty stat
    pretty (CDefault stat _) = text "default:" $$ pretty stat
    pretty (CExpr expr _) = ii $ maybeP pretty expr <> semi
    pretty c@(CCompound _ _) = prettyPrec 0 c
    pretty ifStmt@(CIf expr stat estat _) = 
        ii $  text "if" <+> text "(" <> pretty expr <> text ")"
                $+$ prettyPrec (-1) stat
              $$ maybeP prettyElse estat
      where
        prettyElse (CIf expr stat estat _) =
          text "else if" <+> text "(" <> pretty expr <> text ")"
            $+$ prettyPrec (-1) stat
          $$ maybeP prettyElse estat
        prettyElse estmt =
          text "else" <+> prettyPrec (-1) estmt

    pretty (CSwitch expr stat _) =
        ii $ text "switch" <+> text "(" <> pretty expr <> text ")"
               $+$ prettyPrec (-1) stat
    pretty (CWhile expr stat False _) =
        ii $ text "while" <+> text "(" <> pretty expr <> text ")"
               $+$ prettyPrec (-1) stat
    pretty (CWhile expr stat True _) =
        ii $ text "do" $+$ prettyPrec (-1) stat
               $$ text "while" <+> text "(" <> pretty expr <> text ");"
    pretty (CFor init cond step stat _) =
        ii $ text "for" <+> text "("
               <> either (maybeP pretty) pretty init <> semi
               <+> maybeP pretty cond <> semi
               <+> maybeP pretty step <> text ")" $+$ prettyPrec (-1) stat
    pretty (CGoto ident _) = ii $ text "goto" <+> identP ident <> semi
    pretty (CGotoPtr expr _) = ii $ text "goto" <+> text "*" <+> prettyPrec 30 expr <> semi
    pretty (CCont _) = ii $ text "continue" <> semi
    pretty (CBreak _) = ii $ text "break" <> semi
    pretty (CReturn Nothing _) = ii $ text "return" <> semi
    pretty (CReturn (Just e) _) = ii $ text "return" <+> pretty e <> semi
    pretty (CAsm asmStmt _) = pretty asmStmt
    prettyPrec p (CCompound bis _) =
        let inner = text "{" $+$ vcat (map pretty bis) $$ text "}"
        in  if p == -1 then inner else ii inner
    prettyPrec _ p = pretty p

instance Pretty CAsmStmt where
    pretty (CAsmStmt tyQual expr outOps inOps clobbers _) =
        ii $ text "__asm__" <+> 
             maybeP pretty tyQual <>
             parens asmStmt <> semi
      where
        asmStmt = pretty expr <+> 
                  (if all null [inOps,outOps] && null clobbers then empty else ops)
        ops     =  text ":" <+> hcat (punctuate comma (map pretty outOps)) <+>
                   text ":" <+> hcat (punctuate comma (map pretty inOps)) <+>
                   (if null clobbers then empty else clobs)
        clobs   =  text ":" <+> hcat (punctuate comma (map pretty clobbers))

instance Pretty CAsmOperand where
    -- asm_operand :~ [operand-name] "constraint" ( expr )  
    pretty (CAsmOperand mArgName cnstr expr _) =
        maybeP (\argName -> text "[" <> identP argName <> text "]") mArgName <+>
        pretty cnstr <+>
        parens (pretty expr)

instance Pretty CBlockItem where
    pretty (CBlockStmt stat) = pretty stat
    pretty (CBlockDecl decl) = ii $ pretty decl <> semi
    pretty (CNestedFunDef fundef) = ii $ pretty fundef

instance Pretty CDecl where
    pretty (CDecl specs divs _) =
        hsep (map pretty specs) <+> hsep (punctuate comma (map p divs)) where
        p (declr, init, expr) =
            maybeP pretty declr <+>
            maybeP ((text "=" <+>) . pretty) init <+>
            maybeP ((text ":" <+>) . pretty) expr

instance Pretty CDeclSpec where
    pretty (CStorageSpec sp) = pretty sp
    pretty (CTypeSpec sp) = pretty sp
    pretty (CTypeQual qu) = pretty qu

instance Pretty CStorageSpec where
    pretty (CAuto _)     = text "auto"
    pretty (CRegister _) = text "register"
    pretty (CStatic _)   = text "static"
    pretty (CExtern _)   = text "extern"
    pretty (CTypedef _)  = text "typedef"
    pretty (CThread _)   = text "thread"

instance Pretty CTypeSpec where
    pretty (CVoidType _)        = text "void"
    pretty (CCharType _)        = text "char"
    pretty (CShortType _)       = text "short"
    pretty (CIntType _)         = text "int"
    pretty (CLongType _)        = text "long"
    pretty (CFloatType _)       = text "float"
    pretty (CDoubleType _)      = text "double"
    pretty (CSignedType _)      = text "signed"
    pretty (CUnsigType _)       = text "unsigned"
    pretty (CBoolType _)        = text "_Bool"
    pretty (CComplexType _)     = text "_Complex"
    pretty (CSUType union _)    = pretty union
    pretty (CEnumType enum _)   = pretty enum
    pretty (CTypeDef ident _)   = identP ident
    pretty (CTypeOfExpr expr _) =
        text "typeof" <> text "(" <> pretty expr <> text ")"
    pretty (CTypeOfType decl _) =
        text "typeof" <> text "(" <> pretty decl <> text ")"

instance Pretty CTypeQual where
    pretty (CConstQual _) = text "const"
    pretty (CVolatQual _) = text "volatile"
    pretty (CRestrQual _) = text "__restrict"
    pretty (CInlinQual _) = text "inline"

instance Pretty CStructUnion where
    pretty (CStruct tag ident [] _) = pretty tag <+> maybeP identP ident
    pretty (CStruct tag ident decls _) = vcat [
        pretty tag <+> maybeP identP ident <+> text "{",
        ii $ sep (map (<> semi) (map pretty decls)),
        text "}"]

instance Pretty CStructTag where
    pretty CStructTag = text "struct"
    pretty CUnionTag  = text "union"

instance Pretty CEnum where
    pretty (CEnum ident [] _) = text "enum" <+> maybeP identP ident
    pretty (CEnum ident vals _) = vcat [
        text "enum" <+> maybeP identP ident <+> text "{",
        ii $ sep (punctuate comma (map p vals)),
        text "}"] where
        p (ident, expr) = identP ident <+> maybeP ((text "=" <+>) . pretty) expr

instance Pretty CDeclr where
    prettyPrec p (CVarDeclr ident _) = maybeP identP ident
    prettyPrec p (CPtrDeclr quals declr _) =
        parenPrec p 5 $ text "*" <> hsep (map pretty quals)
                      <+> prettyPrec 5 declr
    prettyPrec p (CArrDeclr declr quals expr _) =
        parenPrec p 5 $ hsep (map pretty quals) <+> prettyPrec 6 declr
                      <> text "[" <> maybeP pretty expr <> text "]"
    prettyPrec p (CFunDeclr declr decls (Right isVariadic) _) =
        prettyPrec 6 declr <> text "("
            <> sep (punctuate comma (map pretty decls))
            <> (if isVariadic then text "," <+> text "..." else empty) <> text ")"
    prettyPrec p (CFunDeclr declr decls (Left oldStyleIds) _) =
        prettyPrec 6 declr <> text "("
            <> sep (punctuate comma (map identP oldStyleIds))
            <> text ")"
            <> (if null decls then empty else error "inconsistent AST")

instance Pretty CInit where
    pretty (CInitExpr expr _) = pretty expr
    pretty (CInitList initl _) =
        text "{" <+> hsep (punctuate comma (map p initl)) <+> text "}" where
        p (desigs, init) = hsep (map pretty desigs) <> pretty init

instance Pretty CDesignator where
    pretty (CArrDesig expr _) = text "[" <> pretty expr <> text "]"
    pretty (CMemberDesig ident _) = text "." <> identP ident
    pretty (CRangeDesig expr1 expr2 _) =
        text "[" <> pretty expr1 <> text ".." <> pretty expr2 <> text "]"

instance Pretty CExpr where
    prettyPrec p (CComma exprs _) =
        parenPrec p (-1) $ hsep (punctuate comma (map (prettyPrec 2) exprs))
    prettyPrec p (CAssign op expr1 expr2 _) =
        parenPrec p 2 $ prettyPrec 3 expr1 <+> pretty op <+> prettyPrec 2 expr2
    prettyPrec p (CCond expr1 expr2 expr3 _) =
        parenPrec p 2 $ prettyPrec 4 expr1 <+> text "?" -- NB: assignment only has a higher precedence if cond is on the rhs
           <+> maybeP pretty expr2 <+> text ":" <+> prettyPrec 4 expr3
    prettyPrec p (CBinary op expr1 expr2 _) =
        let prec = binPrec op
        in  parenPrec p prec $ prettyPrec prec expr1
                             <+> pretty op <+> prettyPrec (prec + 1) expr2
    prettyPrec p (CCast decl expr _) =
        parenPrec p 25 $ text "(" <> pretty decl <> text ")"
                       <> prettyPrec 25 expr
    prettyPrec p (CUnary CPostIncOp expr _) =
        parenPrec p 26 $ prettyPrec 26 expr <> text "++"
    prettyPrec p (CUnary CPostDecOp expr _) =
        parenPrec p 26 $ prettyPrec 26 expr <> text "--"
    prettyPrec p (CUnary op expr _) =
        parenPrec p 25 $ pretty op <> prettyPrec 25 expr
    prettyPrec p (CSizeofExpr expr _) =
        parenPrec p 25 $ text "sizeof" <> text "(" <> pretty expr <> text ")"
    prettyPrec p (CSizeofType decl _) =
        parenPrec p 25 $ text "sizeof" <> text "(" <> pretty decl <> text ")"
    prettyPrec p (CAlignofExpr expr _) =
        parenPrec p 25 $ text "alignof" <> pretty expr
    prettyPrec p (CAlignofType decl _) =
        parenPrec p 25 $ text "alignof" <> pretty decl
    prettyPrec p (CIndex expr1 expr2 _) =
        parenPrec p 26 $ prettyPrec 26 expr1
                       <> text "[" <> pretty expr2 <> text "]"
    prettyPrec p (CCall expr args _) =
        parenPrec p 30 $ prettyPrec 30 expr <> text "("
            <> (sep . punctuate comma . map pretty) args <> text ")"
    prettyPrec p (CMember expr ident deref _) =
        parenPrec p 26 $ prettyPrec 26 expr
                       <> text (if deref then "->" else ".") <> identP ident
    prettyPrec p (CVar ident _) = identP ident
    prettyPrec p (CConst const _) = pretty const
    prettyPrec p (CCompoundLit decl initl _) =
        parens (pretty decl) <+> (braces . hsep . punctuate comma) (map p initl) where
        p ([], init)           = pretty init  
        p ([struct_mem], init) = pretty struct_mem <+> text "=" <+> pretty init  
        p (badDecls , init)  = 
            error $ "Inconsistent AST: more than one left-hand-side in CCompoundLit init-list entry" ++
                    (show $ (hsep $ punctuate comma $ map pretty badDecls) <+> text "=" <+> pretty init)
        
    prettyPrec p (CStatExpr stat _) =
        text "(" <> pretty stat <> text ")"
    
    -- unary_expr :- && ident  {- address of label -}
    prettyPrec p (CLabAddrExpr ident _) = text "&&" <> identP ident
    
    prettyPrec p (CBuiltinExpr builtin) = pretty builtin

instance Pretty CBuiltin where
    pretty (CBuiltinVaArg expr ty_name _) = 
        text "__builtin_va_arg" <+>
        (parens $ pretty expr <> comma <+> pretty ty_name)
    -- The first desig has to be a member field.
    pretty (CBuiltinOffsetOf ty_name (CMemberDesig field1 _ : desigs) _) = 
        text "__builtin_offsetof" <+>
        (parens $ pretty ty_name <> comma <+> identP field1 <> hcat (map pretty desigs) )
    pretty (CBuiltinOffsetOf ty_name otherDesigs _) = 
        error $ "Inconsistent AST: Cannot interpret designators in offsetOf: "++ show (hcat$ map pretty otherDesigs)
    pretty (CBuiltinTypesCompatible ty1 ty2 _) =
        text "__builtin_types_compatible_p" <+>
        (parens $ pretty ty1 <> comma <+> pretty ty2)

instance Pretty CAssignOp where
    pretty CAssignOp = text "="
    pretty CMulAssOp = text "*="
    pretty CDivAssOp = text "/="
    pretty CRmdAssOp = text "%="
    pretty CAddAssOp = text "+="
    pretty CSubAssOp = text "-="
    pretty CShlAssOp = text "<<="
    pretty CShrAssOp = text ">>="
    pretty CAndAssOp = text "&="
    pretty CXorAssOp = text "^="
    pretty COrAssOp  = text "|="

instance Pretty CBinaryOp where
    pretty CMulOp = text "*"
    pretty CDivOp = text "/"
    pretty CRmdOp = text "%"
    pretty CAddOp = text "+"
    pretty CSubOp = text "-"
    pretty CShlOp = text "<<"
    pretty CShrOp = text ">>"
    pretty CLeOp  = text "<"
    pretty CGrOp  = text ">"
    pretty CLeqOp = text "<="
    pretty CGeqOp = text ">="
    pretty CEqOp  = text "=="
    pretty CNeqOp = text "!="
    pretty CAndOp = text "&"
    pretty CXorOp = text "^"
    pretty COrOp  = text "|"
    pretty CLndOp = text "&&"
    pretty CLorOp = text "||"

instance Pretty CUnaryOp where
    pretty CPreIncOp  = text "++"
    pretty CPreDecOp  = text "--"
    pretty CPostIncOp = text "++"
    pretty CPostDecOp = text "--"
    pretty CAdrOp     = text "&"
    pretty CIndOp     = text "*"
    pretty CPlusOp    = text "+"
    pretty CMinOp     = text "-"
    pretty CCompOp    = text "~"
    pretty CNegOp     = text "!"

instance Pretty CConst where
    pretty (CIntConst   int _) = text (showIntConstant int "")
    pretty (CCharConst  chr _) = text (showCharConstant chr "")
    pretty (CFloatConst flt _) = text flt
    pretty (CStrConst   str _) = text (showStringLiteral str "")

instance Pretty CStrLit where
    pretty (CStrLit   str _) = text (showStringLiteral str "")
    
-- precedence of C operators
binPrec :: CBinaryOp -> Int
binPrec CMulOp = 20
binPrec CDivOp = 20
binPrec CRmdOp = 20
binPrec CAddOp = 19
binPrec CSubOp = 19
binPrec CShlOp = 18
binPrec CShrOp = 18
binPrec CLeOp  = 17
binPrec CGrOp  = 17
binPrec CLeqOp = 17
binPrec CGeqOp = 17
binPrec CEqOp  = 16
binPrec CNeqOp = 16
binPrec CAndOp = 15
binPrec CXorOp = 14
binPrec COrOp  = 13
binPrec CLndOp = 12
binPrec CLorOp = 11
