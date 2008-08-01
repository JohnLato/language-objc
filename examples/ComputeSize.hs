{-# LANGUAGE PatternGuards #-}
-- /ComputeSize 'comp' compute_size.c | gcc -x c -o compute_size_hs - && ./compute_size_hs 
module Main where
import System.Environment ; import System.IO
import System.FilePath    ;
import Control.Arrow      ; import Control.Monad
import Data.Map (Map)     ; import qualified Data.Map as Map
import Data.Maybe         ; import Data.Function (fix)
import Data.Generics      ; import Data.List

import Language.C   -- Language.C.{Data,Syntax,Pretty,Parser,InputStream}
import Language.C.Analysis        -- analysis API
import Language.C.System.GCC      -- preprocessor used
import Language.C.Analysis.Export -- [starting point for exporting SemRep to AST]

main :: IO ()
main = do
    let usage = error "Example Usage: ./ScanFile 'pattern' -I/usr/include my_file.c"
    args <- getArgs
    when (length args < 2) usage
    let (pat,args')   = (head &&& tail) args
    let (opts,c_file) = (init &&& last) args'

    let compiler = newGCC "gcc" 
    ast <- parseFile compiler Nothing opts c_file >>= checkResult "[parsing]"
    
    (globals,warnings) <- (runTrav_ >>> checkResult "[analysis]") $ analyseAST ast
    mapM (hPutStrLn stderr . show) warnings
    putStrLn "#include <stdio.h>"
    print $ pretty (generateSizeTests pat globals)
    where
    checkResult :: (Show a) => String -> (Either a b) -> IO b
    checkResult label = either (error . (label++) . show) return

ni :: NodeInfo
ni = internalNode
generateSizeTests :: String -> GlobalDecls -> CTranslUnit
generateSizeTests pat globals = 
      flip CTranslUnit ni $
      -- define all neccessary composite type
      map defineComp referenced_comps
      -- define typedefs
      ++ (map (uncurry defineTyDef) (Map.assocs reverse_typedefs))
      -- generate size tests for named comps
      ++ [ genSizeTest reverse_typedefs (Map.elems comps_of_interest) ]
    where
    comps = Map.mapMaybe fromComp (gTags globals)
    comps_of_interest  = filterDefs pat comps
    referenced_comps   = computeRefClosure comps comps_of_interest
    reverse_typedefs   = Map.fromList . mapMaybe fromCompTyDef . Map.elems . filterDefs pat $ gTypedefs globals
    fromComp (CompDef struct_union) = Just struct_union
    fromComp (EnumDef _) = Nothing
    fromCompTyDef (TypeDef' name ty _ _) =
      case ty of 
        (DirectType (TyComp (CompTypeDecl ref tag _)) _ _) -> Just (ref,name)
        _ -> Nothing
    
filterDefs :: (CNode v, Ord k) => String -> Map k v -> Map k v
filterDefs pat = Map.filter isInCFile
    where
    isInCFile = (pat `isPrefixOf`) . takeBaseName . fileOfNode

-- a small fixpoint algorithm to find the correct order and all references
computeRefClosure :: Map SUERef CompType -> Map SUERef CompType -> [CompType]
computeRefClosure all_comps initial_comps = 
    fixCont addReferenced ([], Map.elems initial_comps, (Map.empty,Map.empty))
    where
    fixCont f = fix $ \close args -> 
        let args'@(result',todo',_) = f args in (if null todo' then reverse result' else close args')
    addReferenced (result,[],ms) = (result,[],ms)
    addReferenced (result,(t:ts),(visit,enter)) | Map.member (sueRef t) enter = (result,ts,(visit,enter))
                                                | Map.member (sueRef t) visit = 
                                                (t:result,ts,(visit,Map.insert (sueRef t) t enter))
                                                | otherwise = 
        let refd = referenced t in (result, refd++(t:ts), (Map.insert (sueRef t) t visit,enter))
    referenced (CompType _ _ members _ _) = mapMaybe getRefdComp members
    getRefdComp memberDecl = fromDirectType (declType memberDecl) >>= fromCompTy
    fromCompTy (TyComp (CompTypeDecl ref _ _)) 
        | (Just r) <- Map.lookup ref all_comps = Just r
        | otherwise = error $ "Internal Error: Could not find definition for "++show ref
    fromCompTy _ = Nothing
    fromDirectType (DirectType tyname _ _) = Just tyname
    fromDirectType (TypeDefType (TypeDefRef _ ref _)) = (fromDirectType.fromJust) ref
    fromDirectType _ = Nothing

defineComp :: CompType -> CExtDecl
defineComp ty = CDeclExt (CDecl (map CTypeSpec (exportCompType $ derefTypeDefs ty)) [] ni)
    where
    derefTypeDefs ty = everywhere (mkT derefTypeDef `extT` replaceEnum) ty
    derefTypeDef (TypeDefType (TypeDefRef _ (Just ty) _)) = ty
    derefTypeDef ty = ty
    replaceEnum (TyEnum _) = TyIntegral TyInt
    replaceEnum dty = dty
    
defineTyDef :: SUERef -> Ident -> CExtDecl
defineTyDef ref tydef = CDeclExt (CDecl specs [(Just$ CDeclr (Just tydef) [] Nothing [] ni, Nothing, Nothing)] ni)
  where 
  specs = [CStorageSpec (CTypedef ni),CTypeSpec (CSUType struct_ty ni)]
  struct_ty = CStruct CStructTag (Just$ internalIdent (show ref)) Nothing [] ni

-- This is were we'd like to have quasi-quoting.
-- For now, as we lack any code generation facilies, we'll parse a string :)
genSizeTest :: Map SUERef Ident -> [CompType] -> CExtDecl
genSizeTest typedefs tys = either (error.show) fromExtDecl $
                  parseC (inputStreamFromString test) (Position "genSizeTest" 1 1) 
    where
    fromExtDecl (CTranslUnit [decl] _ ) = decl
    fromExtDecl (CTranslUnit decls _) = error $ "Expected one declaration, but found: "++show (length decls)
    test = "int main() {" ++ concatMap checkSize tys ++ "}"
    checkSize (CompType sue_ref tag _ _ _) =
      case getTagStr sue_ref tag of
        Nothing  -> ""
        Just tag_str -> "printf(\""++ tag_str ++": %lu\\n\",sizeof(" ++ tag_str ++ ")); ";
    getTagStr ref@(AnonymousType _) tag =
      case Map.lookup ref typedefs of
        Just tyident -> Just (identToString tyident)
        Nothing      -> Nothing -- ignoring inaccessible anonymous type
    getTagStr ref@(NamedType _) tag =
      Just (show tag ++ " " ++ show ref)    

compileAndRunAST _ _ file = 
    case file of
        (CTranslUnit decls _) -> mapM_ (print . pretty) decls
