{-# LANGUAGE RelaxedPolyRec #-}
module Language.C.Analysis.ConstEval where

import Control.Monad
import Data.Bits
import Data.Maybe
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import {-# SOURCE #-} Language.C.Analysis.AstAnalysis (tExpr, ExprSide(..))
import Language.C.Analysis.Debug
import Language.C.Analysis.DeclAnalysis
import Language.C.Analysis.DefTable
import Language.C.Data
import Language.C.Pretty
import Language.C.Analysis.SemRep
import Language.C.Analysis.TravMonad
import Text.PrettyPrint.HughesPJ

data MachineDesc =
  MachineDesc
  { boolSize       :: Integer
  , charSize       :: Integer
  , shortSize      :: Integer
  , intSize        :: Integer
  , longSize       :: Integer
  , longLongSize   :: Integer
  , floatSize      :: Integer
  , doubleSize     :: Integer
  , longDoubleSize :: Integer
  , ptrSize        :: Integer
  }

iSize :: MachineDesc -> IntType -> Integer
iSize md TyBool = boolSize md
iSize md TyChar = charSize md
iSize md TySChar = charSize md
iSize md TyUChar = charSize md
iSize md TyShort = shortSize md
iSize md TyUShort = shortSize md
iSize md TyInt = intSize md
iSize md TyUInt = intSize md
iSize md TyLong = longSize md
iSize md TyULong = longSize md
iSize md TyLLong = longLongSize md
iSize md TyULLong = longLongSize md

fSize :: MachineDesc -> FloatType -> Integer
fSize md TyFloat = floatSize md
fSize md TyDouble = doubleSize md
fSize md TyLDouble = longDoubleSize md

builtinSize :: MachineDesc -> BuiltinType -> Integer
builtinSize md TyVaList = ptrSize md
builtinSize md TyAny    = ptrSize md

intExpr :: (Pos n, MonadName m) => n -> Integer -> m CExpr
intExpr n i =
  genName >>= \name ->
    return $ CConst $ CIntConst (cInteger i) (mkNodeInfo (posOf n) name)

fieldOffset :: MonadTrav m => MachineDesc -> Type -> Ident -> m Integer
fieldOffset md (DirectType (TyComp ctr) _) m =
  do dt <- getDefTable
     case lookupTag (sueRef ctr) dt of
       Just (Left _)   -> astError (nodeInfo ctr)
                          "composite declared but not defined"
       Just (Right (CompDef (CompType _ UnionTag _ _ _))) -> return 0
       Just (Right (CompDef (CompType _ StructTag ms _ ni))) ->
         do let before = takeWhile ((== m) . declIdent) ms
            -- XXX: handle padding
            sizes <- mapM (sizeofType md ni) (map declType before)
            return $ sum sizes
       Just (Right (EnumDef _)) -> astError (nodeInfo ctr) "field of enum?"
       Nothing         -> astError (nodeInfo ctr) "unknown composite"
fieldOffset _ t i = astError (nodeInfo i) $
                    "field of non-composite: " ++ render (pretty t)

sizeofType :: (MonadTrav m, CNode n) => MachineDesc -> n -> Type -> m Integer
sizeofType _  _ (DirectType TyVoid _) = return 0
sizeofType md _ (DirectType (TyIntegral it) _) = return $ iSize md it
sizeofType md _ (DirectType (TyFloating ft) _) = return $ fSize md ft
sizeofType md _ (DirectType (TyComplex ft) _) = return $ 2 * fSize md ft
sizeofType md _ (DirectType (TyComp ctr) _) = compSize md ctr
sizeofType md _ (DirectType (TyEnum _) _) = return $ iSize md TyInt
sizeofType md _ (DirectType (TyBuiltin b) _) = return $ builtinSize md b
sizeofType md _ (PtrType _ _ _)  = return $ ptrSize md
sizeofType md n (ArrayType bt (ArraySize _ sz) _ _) =
  do sz' <- constEval md sz
     case sz' of
       CConst (CIntConst i _) ->
         do s <- sizeofType md n bt
            return $ getCInteger i * s
       _ -> astError (nodeInfo sz) $
            "array size is not a constant: " ++ (render . pretty) sz
sizeofType md n (TypeDefType (TypeDefRef _ (Just t) _)) = sizeofType md n t
sizeofType _ n t = astError (nodeInfo n) $
                 "can't find size of type: " ++ (render . pretty) t

compSize :: MonadTrav m => MachineDesc -> CompTypeRef -> m Integer
compSize md ctr =
  do dt <- getDefTable
     case lookupTag (sueRef ctr) dt of
       Just (Left _)   -> astError (nodeInfo ctr)
                          "composite declared but not defined"
       Just (Right (CompDef (CompType _ tag ms _ ni))) ->
         do let ts = map declType ms
            sizes <- mapM (sizeofType md ni) ts
            -- XXX: handle padding
            case tag of
              StructTag -> return $ sum sizes
              UnionTag  -> return $ maximum sizes
       Just (Right (EnumDef _)) -> return $ iSize md TyInt
       Nothing         -> astError (nodeInfo ctr) "unknown composite"


{- Expression evaluation -}

-- Use the withWordBytes function to wrap the results around to the
-- correct word size
intOp :: CBinaryOp -> Integer -> Integer -> Integer
intOp CAddOp i1 i2 = i1 + i2
intOp CSubOp i1 i2 = i1 - i2
intOp CMulOp i1 i2 = i1 * i2
intOp CDivOp i1 i2 = i1 `div` i2
intOp CRmdOp i1 i2 = i1 `mod` i2
intOp CShlOp i1 i2 = i1 `shiftL` fromInteger i2
intOp CShrOp i1 i2 = i1 `shiftR` fromInteger i2
intOp CLeOp  i1 i2 = toInteger $ fromEnum $ i1 < i2
intOp CGrOp  i1 i2 = toInteger $ fromEnum $ i1 > i2
intOp CLeqOp i1 i2 = toInteger $ fromEnum $ i1 <= i2
intOp CGeqOp i1 i2 = toInteger $ fromEnum $ i1 >= i2
intOp CEqOp  i1 i2 = toInteger $ fromEnum $ i1 == i2
intOp CNeqOp i1 i2 = toInteger $ fromEnum $ i1 /= i2
intOp CAndOp i1 i2 = i1 .&. i2
intOp CXorOp i1 i2 = i1 `xor` i2
intOp COrOp  i1 i2 = i1 .|. i2
intOp CLndOp i1 i2 = toInteger $ fromEnum $ (i1 /= 0) && (i2 /= 0)
intOp CLorOp i1 i2 = toInteger $ fromEnum $ (i1 /= 0) || (i2 /= 0)

-- Use the withWordBytes function to wrap the results around to the
-- correct word size
intUnOp :: CUnaryOp -> Integer -> Maybe Integer
intUnOp CPlusOp i = Just i
intUnOp CMinOp  i = Just $ -i
intUnOp CCompOp i = Just $ complement i
intUnOp CNegOp  i = Just $ toInteger $ fromEnum $ i == 0
intUnOp _       _ = Nothing

withWordBytes :: Integer -> Integer -> Integer
withWordBytes bytes n = n .&. (2^(bytes * 8) - 1)

boolValue :: CExpr -> Maybe Bool
boolValue (CConst (CIntConst i _))  = Just $ getCInteger i /= 0
boolValue (CConst (CCharConst c _)) = Just $ getCCharAsInt c /= 0
boolValue (CConst (CStrConst _ _))  = Just True
boolValue _                         = Nothing

intValue :: CExpr -> Maybe Integer
intValue (CConst (CIntConst i _))  = Just $ getCInteger i
intValue (CConst (CCharConst c _)) = Just $ getCCharAsInt c
intValue _                         = Nothing

constEval :: MonadTrav m => MachineDesc -> CExpr -> m CExpr
constEval md (CCond e1 me2 e3 ni) =
  do e1'  <- constEval md e1
     me2' <- maybe (return Nothing) (\e -> Just `liftM` constEval md e) me2
     e3'  <- constEval md e3
     case boolValue e1' of
       Just True  -> return $ fromMaybe e1' me2'
       Just False -> return e3'
       Nothing    -> return $ CCond e1' me2' e3' ni
constEval md e@(CBinary op e1 e2 ni) =
  do e1' <- constEval md e1
     e2' <- constEval md e2
     t <- tExpr [] RValue e
     bytes <- sizeofType md e t
     case (intValue e1', intValue e2') of
       (Just i1, Just i2) -> intExpr ni (withWordBytes bytes (intOp op i1 i2))
       (_, _)             -> return $ CBinary op e1' e2' ni
constEval md (CUnary op e ni) =
  do e' <- constEval md e
     t <- tExpr [] RValue e
     bytes <- sizeofType md e t
     case intValue e' of
       Just i  -> case intUnOp op i of
                    Just i' -> intExpr ni (withWordBytes bytes i')
                    Nothing -> astError ni
                               "invalid unary operator applied to constant"
       Nothing -> return $ CUnary op e' ni
constEval md (CCast d e ni) =
  do e' <- constEval md e
     return $ CCast d e' ni
constEval md (CSizeofExpr e ni) =
  do t <- tExpr [] RValue e
     sz <- sizeofType md e t
     intExpr ni sz
constEval md (CSizeofType d ni) =
  do t <- analyseTypeDecl d
     sz <- sizeofType md d t
     intExpr ni sz
-- Eventually, we'll do these, too
--constEval md (CAlignofExpr e ni) =
--constEval md (CAlignofType t ni) =
--constEval md (CIndex b i ni) =
--constEval md (CMember e m deref ni) =
--constEval md (CVar i ni) =
constEval md e@(CVar i _)       =
  do t <- tExpr [] RValue e
     case derefTypeDef t of
       DirectType (TyEnum etr) _ ->
         do dt <- getDefTable
            case lookupTag (sueRef etr) dt of
              Just (Right (EnumDef (EnumType _ es _ _))) ->
                do ecs <- mapM enumConst es
                   maybe (return e) return (lookup i ecs)
              _ -> return e
       _ -> return e
  where enumConst (Enumerator n e' _ _) = do c <- constEval md e'
                                             return (n, c)
constEval _ e = return e
