{-# LANGUAGE Rank2Types, ScopedTypeVariables, FlexibleInstances  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GenericTree
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  ALPHA !!!
-- Portability :  unspecified
--
-- This module can be used to debug AST trees, by converting them into a
-- generic tree with debug information.
-----------------------------------------------------------------------------
module GenericTree where
import Language.C
import Language.C.Syntax.AST
import Data.Tree
import Data.Typeable
import Data.Maybe

data AstNode =
      AstNode String (TypeRep, forall a. (Typeable a) => a -> (Maybe a)) (Maybe NodeInfo)
    | ListNode String
    | ConstNode CConst
    | IdentNode Ident
    | InfoNode String

dynRep :: (Typeable a) => a -> (TypeRep, forall b. (Typeable b) => b -> (Maybe b))
dynRep a = (typeOf a, \_ -> cast a)

leaf :: AstNode -> Tree AstNode
leaf n = Node n []

constLeaf :: CConst -> Tree AstNode
constLeaf = leaf . ConstNode

ideLeaf = leaf . IdentNode

infoLeaf :: String -> Tree AstNode
infoLeaf = leaf . InfoNode

renderLeaf :: (Typeable a, CNode a, Pretty v) => String -> a -> v -> Tree AstNode
renderLeaf lab t v = leaf $ AstNode (addLab.show.pretty $ v) (dynRep t) (Just $ nodeInfo t)
    where addLab s | null lab = s
                   | otherwise = lab ++ ": " ++ s

emptyList :: Tree AstNode
emptyList = Node (ListNode "<empty>") []

listNode :: (TreeView a) => String -> [a] -> Tree AstNode
listNode lab = Node (ListNode lab) . map (treeView (init lab))

attrsNode :: [CAttr] -> Tree AstNode
attrsNode = listNode "attributes"

node :: (Typeable t, CNode t) => String -> t -> [Tree AstNode] -> Tree AstNode
node lab v kids = Node (AstNode lab (dynRep v) (Just $ nodeInfo v)) (filter (not.isEmptyList) kids) where
    isEmptyList (Node (ListNode _) []) = True
    isEmptyList _ = False

instance Show AstNode where
    show (AstNode name _ _) = name
    show (ListNode str) = str
    show (ConstNode c) = (show.pretty) c
    show (IdentNode i) = show i
    show (InfoNode s) = s

class TreeView n where
    treeView :: String -> n -> Tree AstNode

instance TreeView (Tree AstNode) where
    treeView _ = id

instance TreeView Ident where
    treeView _ = leaf . IdentNode

instance TreeView CTranslUnit where
    treeView name t@(CTranslUnit decls ni) =
        node name t $ map (treeView "") decls

instance TreeView CExtDecl where
    treeView _ (CDeclExt decl) = treeView "ext-decl" decl
    treeView _ (CFDefExt fundef) = treeView "ext-fundef" fundef
    treeView _ t@(CAsmExt asm ni) = node "ext-asm" t [constLeaf $ liftStrLit asm]

instance TreeView CFunDef where
    treeView lab t@(CFunDef specs declr params stat ni) =
        node lab t $
            [ listNode "fun-def-specs" specs,
              treeView "fun-def-declr" declr,
              listNode "old-style-param-decls" params,
              treeView "fun-def-stmt" stat ]

instance TreeView CDecl where
   treeView lab t@(CDecl specs decllist ni) = node lab t $
        [ listNode "decl-specs" specs,
          listNode "declaration-body" (map declListEntry decllist)
        ]
    where
    declListEntry (Just declr,Nothing,Nothing) = treeView "declr" declr
    declListEntry (declr,initi,bitsize) = listNode "decl-entry" $
        catMaybes
        [ fmap (treeView "declr") declr,
          fmap (treeView "initializer") initi,
          fmap (treeView "bitsize") bitsize
        ]

instance TreeView CDeclr where
    treeView lab t@(CDeclr ide derived asm attrs ni) =
        node lab t $
            [ maybe emptyList ideLeaf ide,
              listNode "type-derivations" derived,
              attrsNode attrs
            ]

instance TreeView CDerivedDeclr where
    treeView lab t@(CPtrDeclr quals ni) = node "ptr-deriv" t $ map (treeView "ptr-qual") quals
    treeView lab t@(CArrDeclr quals sz ni) = node "arr-deriv" t $
        [ listNode "arr-quals" quals,
          treeView "arr-size" sz ]
    treeView lab t@(CFunDeclr params attrs ni) = node "fun-deriv" t (paramsView++attrsView)
        where
        paramsView =
            case params of
                Right (params,variadic) -> [listNode "params" params] ++ (if variadic then [infoLeaf "variadic"] else [])
                Left oldStyle -> [listNode "old-style-param-names" oldStyle]
        attrsView = [listNode "fun-attrs" attrs]

instance TreeView CArrSize where
    treeView lab (CNoArrSize True) = infoLeaf $ "variable size"
    treeView lab (CNoArrSize False) = emptyList
    treeView lab (CArrSize False c) = treeView lab c
    treeView lab (CArrSize True c) = Node (ListNode lab) [ infoLeaf "static size", treeView "size-expr" c ]

instance TreeView CStat where
    treeView _ t =
        case t of
            CLabel ide stat attrs _ -> node "label" t [treeView "label-ident" ide,attrsNode attrs,treeView "label-stmt" stat]
            CCase e s _ -> node "case" t [treeView "case-expr" e, treeView "case-body" s]
            CCases el eu s _ -> node "cases" t [treeView "case-expr-lower" el, treeView "case-expr-upper" eu, treeView "case-body" s]
            CCompound i bis _ -> node "block" t [listNode "idents" i,listNode "block-items" bis]
            CDefault s _ -> node "case-default" t [treeView "default-body" s]
            CExpr Nothing _ ->  node "empty-expr-stmt" t []
            CExpr (Just e) _ -> node "expr-stmt" t [ treeView "expr" e ]
            CIf ife thens elses _ -> node "if-stmt" t $ [ treeView "if-expr" ife, treeView "then-stmt" thens ] ++
                                                        (maybe [] return $ fmap (treeView "else-stmt") elses)
            CSwitch es s _ -> node "switch-stmt" t $ [treeView "switch-body" s]
            CWhile e s doWhile _ -> node (if doWhile then "do-while" else "while") t $
                                        [treeView "while-guard" e, treeView "while-body" s]
            CFor eInit eGuard eUpd stat _ -> node "for" t $ [ maybe (infoLeaf "no-for-init") id (initTree eInit),
                                                              maybe (infoLeaf "no-for-guard") (treeView "for-guard") eGuard,
                                                              maybe (infoLeaf "no-for-update") (treeView "for-update") eUpd]
            CGoto ide _ -> node "goto" t [treeView "goto-label" ide]
            CGotoPtr expr _ -> node "goto-ptr" t [treeView "goto-expr" expr]
            CCont _ -> node "continue" t []
            CBreak _ -> node "break" t []
            CReturn e _ -> node "return" t $ maybe [] return $ fmap (treeView "return-expr") e
            CAsm asm _ -> node "asm-stmt" t [treeView "asm" asm]
     where
     initTree (Left eInit) = fmap (treeView "for-init") eInit
     initTree (Right initStmt) = Just (treeView "for-c99-init" initStmt)

instance TreeView CBlockItem where
    treeView lab (CBlockStmt stmt) = treeView "block-stmt" stmt
    treeView lab (CBlockDecl decl) = treeView "block-decl" decl
    treeView lab (CNestedFunDef fundef) = treeView "nested-fun-def" fundef

instance TreeView CDeclSpec where
    treeView _ t@(CStorageSpec v) = renderLeaf "" t v
    treeView _ t@(CTypeSpec v)    = renderLeaf "" t v
    treeView _ t@(CTypeQual v)    = renderLeaf "" t v
instance TreeView CAttr where
    treeView lab t = renderLeaf lab t t
instance TreeView CTypeQual where
    treeView lab t = renderLeaf "" t t
instance TreeView CInit where
    treeView lab t = renderLeaf lab t t
instance TreeView CExpr where
    treeView lab t = renderLeaf lab t t
instance TreeView CAsmStmt where
    treeView lab t = renderLeaf lab t t
-- data CArrSize
-- = CNoArrSize Bool
-- | CArrSize Bool CExpr
-- data CInit
-- = CInitExpr CExpr NodeInfo
-- | CInitList CInitList NodeInfo
-- type CInitList = [([CDesignator], CInit)]
-- data CDesignator
-- = CArrDesig CExpr NodeInfo
-- | CMemberDesig Ident NodeInfo
-- | CRangeDesig CExpr CExpr NodeInfo
-- data CAsmStmt = CAsmStmt (Maybe CTypeQual) CStrLit [CAsmOperand] [CAsmOperand] [CStrLit] NodeInfo
-- data CAsmOperand = CAsmOperand (Maybe Ident) CStrLit CExpr NodeInfo
-- data CExpr
-- = CComma [CExpr] NodeInfo
-- | CAssign CAssignOp CExpr CExpr NodeInfo
-- | CCond CExpr (Maybe CExpr) CExpr NodeInfo
-- | CBinary CBinaryOp CExpr CExpr NodeInfo
-- | CCast CDecl CExpr NodeInfo
-- | CUnary CUnaryOp CExpr NodeInfo
-- | CSizeofExpr CExpr NodeInfo
-- | CSizeofType CDecl NodeInfo
-- | CAlignofExpr CExpr NodeInfo
-- | CAlignofType CDecl NodeInfo
-- | CComplexReal CExpr NodeInfo
-- | CComplexImag CExpr NodeInfo
-- | CIndex CExpr CExpr NodeInfo
-- | CCall CExpr [CExpr] NodeInfo
-- | CMember CExpr Ident Bool NodeInfo
-- | CVar Ident NodeInfo
-- | CConst CConst
-- | CCompoundLit CDecl CInitList NodeInfo
-- | CStatExpr CStat NodeInfo
-- | CLabAddrExpr Ident NodeInfo
-- | CBuiltinExpr CBuiltin
-- data CAssignOp
-- = CAssignOp
-- | CMulAssOp
-- | CDivAssOp
-- | CRmdAssOp
-- | CAddAssOp
-- | CSubAssOp
-- | CShlAssOp
-- | CShrAssOp
-- | CAndAssOp
-- | CXorAssOp
-- | COrAssOp
-- data CBinaryOp
-- = CMulOp
-- | CDivOp
-- | CRmdOp
-- | CAddOp
-- | CSubOp
-- | CShlOp
-- | CShrOp
-- | CLeOp
-- | CGrOp
-- | CLeqOp
-- | CGeqOp
-- | CEqOp
-- | CNeqOp
-- | CAndOp
-- | CXorOp
-- | COrOp
-- | CLndOp
-- | CLorOp
-- data CUnaryOp
-- = CPreIncOp
-- | CPreDecOp
-- | CPostIncOp
-- | CPostDecOp
-- | CAdrOp
-- | CIndOp
-- | CPlusOp
-- | CMinOp
-- | CCompOp
-- | CNegOp
-- data CBuiltin
-- = CBuiltinVaArg CExpr CDecl NodeInfo
-- | CBuiltinOffsetOf CDecl [CDesignator] NodeInfo
-- | CBuiltinTypesCompatible CDecl CDecl NodeInfo
-- data CConst
-- = CIntConst CInteger NodeInfo
-- | CCharConst CChar NodeInfo
-- | CFloatConst CFloat NodeInfo
-- | CStrConst CString NodeInfo
-- data CStrLit = CStrLit CString NodeInfo
--
-- data CStructUnion = CStruct CStructTag (Maybe Ident) (Maybe [CDecl]) [CAttr] NodeInfo
-- data CStructTag
-- = CStructTag
-- | CUnionTag
-- data CEnum = CEnum (Maybe Ident) (Maybe [(Ident, Maybe CExpr)]) [CAttr] NodeInfo
-- partitionDeclSpecs :: [CDeclSpec] -> ([CStorageSpec], [CAttr], [CTypeQual], [CTypeSpec], Bool)
-- data CStorageSpec
-- = CAuto NodeInfo
-- | CRegister NodeInfo
-- | CStatic NodeInfo
-- | CExtern NodeInfo
-- | CTypedef NodeInfo
-- | CThread NodeInfo
-- data CTypeSpec
-- = CVoidType NodeInfo
-- | CCharType NodeInfo
-- | CShortType NodeInfo
-- | CIntType NodeInfo
-- | CLongType NodeInfo
-- | CFloatType NodeInfo
-- | CDoubleType NodeInfo
-- | CSignedType NodeInfo
-- | CUnsigType NodeInfo
-- | CBoolType NodeInfo
-- | CComplexType NodeInfo
-- | CSUType CStructUnion NodeInfo
-- | CEnumType CEnum NodeInfo
-- | CTypeDef Ident NodeInfo
-- | CTypeOfExpr CExpr NodeInfo
-- | CTypeOfType CDecl NodeInfo
-- isSUEDef :: CTypeSpec -> Bool
-- data CTypeQual
-- = CConstQual NodeInfo
-- | CVolatQual NodeInfo
-- | CRestrQual NodeInfo
-- | CInlineQual NodeInfo
-- | CAttrQual CAttr
-- data CAttr = CAttr Ident [CExpr] NodeInfo
