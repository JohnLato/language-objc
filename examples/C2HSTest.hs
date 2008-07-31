module Main where
import Language.C
import Language.C.Analysis
import Language.C.Analysis.Export
import Language.C.System.GCC

import System.IO
import System.Environment
import System.FilePath
import Data.Maybe
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map


import Control.Arrow ((>>>),(&&&))
import Data.List (partition,foldl',isInfixOf)
import Text.PrettyPrint.HughesPJ

main :: IO ()
main = do
    let usage = error "Example Usage: ./C2HSTest (sizeof|getset|getsetdeep) header_file_pattern -I/usr/include header_file"
    args <- getArgs
    when (length args < 3) usage

    -- get cpp options and input file
    let (mode:pat:args') = args
    let (opts,file) = (init &&& last) args'

    -- parse the header files
    ast <- parseFile (newGCC "gcc") Nothing opts file
           >>= checkResult ("[parsing]" ++ show file)
    -- analyze
    (globals,warnings) <- (runTrav_ >>> checkResult "[analysis]") $ analyseAST ast
    mapM (hPutStrLn stderr . show) warnings
    -- filter interesting decls
    let globals' = filterGlobalDecls ((pat `isInfixOf`) . fileOfNode) globals
    let tydef_lookup = tydefLookup globals'
    -- switch mode and generate codes
    case mode of
      "sizeof" -> generateSizeofTest tydef_lookup file globals'
      "getset" -> generateGetSetTest tydef_lookup False file globals'
      "getsetrec" -> generateGetSetTest tydef_lookup True file globals'
    where
    checkResult :: (Show a) => String -> (Either a b) -> IO b
    checkResult label = either (error . (label++) . show) return


-- FIXME: reverse typedef lookup seems to be a common pattern, so we should add some support for it
type TydefMap = Map SUERef Ident
tydefLookup :: GlobalDecls -> TydefMap
tydefLookup globs =  Map.fromList . mapMaybe revMap . Map.elems $ gTypedefs globs
  where
  revMap (TypeDef' ident (DirectType (TyComp comptype) _ _) _ _) = Just (sueRef comptype,ident)
  revMap (TypeDef' ident (DirectType (TyEnum enumtype) _ _) _ _) = Just (sueRef enumtype,ident)
  revMap _ = Nothing
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x
writeDoc :: FilePath -> Doc -> IO ()
writeDoc fp = writeFile fp . ((++"\n") . show)
sueName :: TydefMap -> SUERef -> Maybe String
sueName tydef_rlm ref = 
  case Map.lookup ref tydef_rlm of
      Just ident -> Just (identToString ident)
      Nothing    -> Nothing 


-- Sizeof test:
-- C: 
--  > #include HEADER-FILE
--  > int main() { printf("type: %lu\n",sizeof(type); ... } 
-- CHS:
--  > main = mapM_ (\t -> printf "type: %lu\n" {#sizeof type #})
generateSizeofTest :: TydefMap -> FilePath -> GlobalDecls -> IO ()
generateSizeofTest tydefs file globs = 
  do
    writeDoc c_file (includeHeader file $+$ (pretty $ cMain (map printSizeC tags)))
    writeDoc chs_file (chsPreamble $+$ chsMain (map printSizeCHS tags))
  where
  (c_file,chs_file) = (takeBaseName file ++ ".c", takeBaseName file ++ "_test.chs")
  tags = mapMaybe (\tagdef -> fmap ((,) tagdef) (sueName tydefs . sueRef $ tagdef)) (Map.elems (gTags globs))
  -- TODO: ...
  printSizeC (tagdef, name) = cExpr $ call "printf" [cStr (name++": %lu\n"), cSizeof (getCType tagdef name) ]
  getCType tagdef name = TypeDefType $ TypeDefRef (mkIde name) (Just . directType $ typeOfTagDef tagdef) internalNode
  printSizeCHS (tagdef, name) = hsep [ text "printf", doubleQuotes (text (name ++": %d\\n")), 
                                       parens (chsDirective ["sizeof",name] <+> colon <> colon <+> text "Int") ]
data PathComponent = Access VarName | Deref
generateGetSetTest :: TydefMap -> Bool -> FilePath -> GlobalDecls -> IO ()
generateGetSetTest = undefined

-- genHaskellCode :: FilePath -> GlobalDecls -> Doc
-- genHaskellCode file globs = 
--   text "{-# LANGUAGE CPP #-}" $+$
--   text testPrelude $+$
--   (text "main =" $+$ nest 4 (text "do" <+> vcat (printSizes++getters)))
--   where
--   printSizes = map printSize . mapMaybe (sueName.sueRef) . Map.elems $ gTags globs
--   printSize key = text "print" <+> parens (doubleQuotes (text $ "#sizeof " ++ key ++ ": ") <+> 
--                   text "++ show" <+> c2hsSize key)
--   c2hsSize key = braces (text "#"<>text "sizeof"<+>text key<>text "#")
--   maxSelects = 100 -- lp: 111604
--   getters = map printGetter . concat . map (take maxSelects . pathes) . Map.elems $ gTags globs
--   printGetter s = text "test" <+> doubleQuotes (text s) <+> text "$" $$
--                   nest 8 (
--                     braces (text "#"<>text "get"<+>text s<+> text "#") <+>
--                     text "structPtr" )
--   -- compute the set of non-recursive accessor paths for a type
--   pathes (CompTag (CompType ref kind members _ _)) 
--     | (Just c2hsName) <- getC2hsName ref =
--         map (mkPath c2hsName) $ concatMap (recPaths Map.empty) members
--     | otherwise = []
--   pathes (EnumTag (EnumType _ _ _ _)) = []
--   -- get the name c2hs uses to refer to the type in getters/setters
--   getC2hsName ref@(AnonymousType _) = sueName ref 
--   getC2hsName ref@(NamedType ident) = Just $ identToString ident
--   -- path for all members, remembering visited members
--   recPaths m (MemberDecl (VarDecl name _ ty) _ _) = 
--     let m' = Map.insert (identOfVarName name) 1 m in
--     directAccessAllowed ty [Access name] ++ map (Access name :) (recPathOf m' ty)
--   directAccessAllowed ty l = 
--     case ty of    
--       DirectType TyVoid _ _     -> [] -- no direct access to void type
--       DirectType (TyComp _) _ _ -> [] -- no support for unmarshalling composite types
--       TypeDefType (TypeDefRef _ (Just ty') _) -> directAccessAllowed ty' l
--       FunctionType _ -> []
--       _ -> [l]
--   recPathOf m (DirectType tyname _ _) = recPathOfDT m tyname
--   recPathOf m (PtrType ty _ _) = directAccessAllowed ty [Deref] ++ map (Deref:) (recPathOf m ty)
--   recPathOf m (ArrayType ty _ _ _) = [] -- not support by c2hs 
--                                   -- [Deref] : map (Deref:) (recPathOf m ty)
--   recPathOf m (FunctionType _) = []
--   recPathOf m (TypeDefType (TypeDefRef tydef (Just ty) _)) = recPathOf m ty
--   recPathOfDT m (TyComp (CompTypeDecl ref _ _)) = concatMap (recPaths m) (membersOf m ref)
--   recPathOfDT m _ = []
--   membersOf m ref = case Map.lookup ref (gTags globs) of
--                       Just (CompTag (CompType ref kind members _ _)) -> 
--                         filter (not . flip Map.member m . identOfVarName . declName) members
--                       _ -> []
--   mkPath name path = foldr showPathSelect name (reverse path)
--   showPathSelect (Access name) p = (if safeHead p == Just '*' then "(" ++ p ++ ")" else p) ++ "." ++
--                                    (identToString (identOfVarName name))
--   showPathSelect Deref p = "*" ++ p -- * binds weakest in c2hs
--   removeBraces ('(':s) = init s
--   removeBraces s = s
-- testPrelude = "module Main where \n"
--     ++ "type PtrDerefPath = ([Int],[Int])\n"
--     ++ "type CChar = PtrDerefPath\n"
--     ++ "type CUChar = PtrDerefPath\n"
--     ++ "type CSChar = PtrDerefPath\n"
--     ++ "type CShort = PtrDerefPath\n"
--     ++ "type CUShort = PtrDerefPath\n"
--     ++ "type CInt = PtrDerefPath\n"
--     ++ "type CUInt = PtrDerefPath\n"
--     ++ "type CLong = PtrDerefPath\n"
--     ++ "type CULong = PtrDerefPath\n"
--     ++ "type CLLong = PtrDerefPath\n"
--     ++ "type CULLong = PtrDerefPath\n"
--     ++ "type CFloat = PtrDerefPath\n"
--     ++ "type CDouble = PtrDerefPath\n"
--     ++ "type FunPtr a = PtrDerefPath\n"
--     ++ "type Ptr a = PtrDerefPath\n"
--     ++ "structPtr = ([],[])\n"
--     ++ "peekByteOff :: Ptr a -> Int -> IO (Ptr a)\n"
--     ++ "peekByteOff (path,bits) offs = return (offs : path,bits)\n"
--     ++ "shiftL :: CInt -> Int -> CInt\n"
--     ++ "shiftL (offs,bits) k = (offs,k:bits)\n"
--     ++ "shiftR :: CInt -> Int -> CInt\n"
--     ++ "shiftR (offs,bits) k = (offs,(-k):bits)\n"
--     ++ "showDerefPath (offs,bits) = show (reverse offs) ++ \" / \" ++ show (reverse bits)\n"
--     ++ "test :: String -> IO PtrDerefPath -> IO ()\n"
--     ++ "test msg action = \n"
--     ++ "  do\n"
--     ++ "    path <- action\n"
--     ++ "    putStrLn $ msg ++ \": \" ++ (showDerefPath path)\n"

-- (Building CHS files)
chsPreamble :: Doc
chsPreamble = 
  text "module Main where" $+$
  vcat (map (\modul -> text "import" <+> text modul) ["Text.PrettyPrint.HughesPJ","Text.Printf"])
chsMain :: [Doc] -> Doc
chsMain do_lines = 
  text "main :: IO ()" $+$
  text "main =" $+$
  (nest 2 (text "do" $$ (nest 2 $ vcat do_lines)))
chsDirective :: [String] -> Doc
chsDirective strs = braces ( sharp <> hsep (map text strs) <> sharp ) 
  where sharp = text "#"

-- (Building C Files)
includeHeader :: FilePath -> Doc
includeHeader fp = text "#include" <+> doubleQuotes (text fp)
cMain :: [CStat] -> CExtDecl
cMain stmts = 
  CFDefExt $
    CFunDef 
      tyspec
      (simpleDeclr "main" typedeclrs)
      []
      (compound stmts)
      internalNode
  where
  simpleDeclr name typedeclrs = CDeclr (Just $ mkIde name) typedeclrs Nothing [] internalNode
  intType :: IntType -> Type
  intType intTy = directType (TyIntegral intTy)
  compound stmts = CCompound [] (map CBlockStmt stmts) internalNode
  (tyspec,typedeclrs) = exportType $ FunctionType (FunType (intType TyInt) [] False [])
directType :: TypeName -> Type
directType tyname = DirectType tyname noTypeQuals []
call :: String -> [CExpr] -> CExpr
call fun args = CCall (CVar (mkIde fun) internalNode) args internalNode
cStr :: String -> CExpr
cStr str = CConst (CStrConst (cstring str) internalNode) internalNode
cSizeof :: Type -> CExpr
cSizeof ty = CSizeofType (exportTypeDecl ty) internalNode
cExpr :: CExpr -> CStat
cExpr e = CExpr (Just e) internalNode
mkIde :: String -> Ident
mkIde = internalIdent
