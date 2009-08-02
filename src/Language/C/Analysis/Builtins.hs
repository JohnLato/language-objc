module Language.C.Analysis.Builtins (builtins) where

import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Analysis.DefTable
import Language.C.Analysis.SemRep
import Language.C.Analysis.TypeUtils

builtins :: DefTable
builtins = foldr doIdent (foldr doTypeDef emptyDefTable typedefs) idents
  where doTypeDef d = snd . defineTypeDef (identOfTypeDef d) d
        doIdent   d = snd . defineGlobalIdent (declIdent d) d
        dName     s = VarName (internalIdent s) Nothing
        param ty    = ParamDecl (VarDecl
                                 NoName
                                 (DeclAttrs False (Auto False) [])
                                 ty) undefNode
        fnAttrs     = DeclAttrs False (FunLinkage ExternalLinkage) []
        varAttrs    = DeclAttrs False (Static InternalLinkage False) []
        fnType r as = FunctionType (FunType r (map param as) False [])
        func n r as = Declaration
                      (Decl
                       (VarDecl (dName n) fnAttrs (fnType r as))
                       undefNode)
        var n t     = Declaration
                      (Decl (VarDecl (dName n) varAttrs t) undefNode)
        typedef n t = TypeDef (internalIdent n) t [] undefNode
        typedefs    = [ typedef "__builtin_va_list"
                                valistType
                      ]
        idents      = [ func "__builtin_expect"
                             (integral TyLong)
                             [integral TyLong, integral TyLong]
                      , func "__builtin_fabs"
                             (floating TyDouble)
                             [floating TyDouble]
                      , func "__builtin_fabsf"
                             (floating TyFloat)
                             [floating TyFloat]
                      , func "__builtin_fabsl"
                             (floating TyLDouble)
                             [floating TyLDouble]
                      , func "__builtin_inf" (floating TyDouble) []
                      , func "__builtin_inff" (floating TyFloat) []
                      , func "__builtin_infl" (floating TyLDouble) []
                      , func "__builtin_huge_val" (floating TyDouble) []
                      , func "__builtin_huge_valf" (floating TyFloat) []
                      , func "__builtin_huge_vall" (floating TyLDouble) []
                      , func "__builtin_va_start"
                             voidType
                             [ valistType , voidPtr ]
                      , func "__builtin_va_end"
                             voidType
                             [valistType]
                      , func "__builtin_va_copy"
                             voidType
                             [ valistType, valistType ]
                      , func "__builtin_alloca"
                             voidPtr
                             [ size_tType ]
                      , func "__builtin_memcpy"
                             voidPtr
                             [ voidPtr
                             , constVoidPtr
                             , size_tType
                             ]
                      , func "__builtin_strchr"
                             charPtr
                             [ constCharPtr, integral TyInt]
                      , func "__builtin_strncpy"
                             charPtr
                             [ constCharPtr -- XXX: restrict
                             , constCharPtr -- XXX: restrict
                             , size_tType
                             ]
                      , func "__builtin_strncat"
                             charPtr
                             [ constCharPtr -- XXX: restrict
                             , constCharPtr -- XXX: restrict
                             , size_tType
                             ]
                      , func "__builtin_strcmp"
                             (integral TyInt)
                             [ constCharPtr, constCharPtr ]
                      , func "__builtin_bzero"
                             voidType
                             [ voidPtr, size_tType ]
                      , func "__builtin_constant_p"
                             (integral TyInt)
                             [DirectType (TyBuiltin TyAny) noTypeQuals]
                      -- XXX: I don't know if the following has the
                      -- correct type. It doesn't seem to be
                      -- documented.
                      , func "__builtin_extract_return_addr"
                             voidPtr
                             [ voidPtr ]
                      , func "__builtin_return_address"
                             voidPtr
                             [ integral TyUInt ]
                      , func "__builtin_frame_address"
                             voidPtr
                             [ integral TyUInt ]
                      , func "__builtin_expect"
                             (integral TyLong)
                             [ integral TyLong, integral TyLong ]
                      , func "__builtin_prefetch"
                             voidType
                             [ constVoidPtr ]
                      , var "__func__"
                            stringType
                      , var "__PRETTY_FUNCTION__"
                            stringType
                      ]
