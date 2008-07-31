-- check wheter the system is usable
module Main where
import Language.C
import Language.C.System.Preprocess
import Language.C.System.GCC
import Language.C.Analysis
import Language.C.Analysis.Pretty
import System.Environment
import Control.Monad

import Debug.Trace
import Text.PrettyPrint.HughesPJ
main = do
    infile <- liftM head getArgs
    input_stream <- errorOnLeftM "preprocessor failed with exit code" $
        runPreprocessor (newGCC "gcc") (simpleCppArgs [] infile `addCppOption` (IncludeDir "/opt/local/include"))
    ast <- errorOnLeft "Parse Error" $
        parseC input_stream (Position infile 1 1)
    (global_decls,warnings) <- errorOnLeft "Semantic Error" $
        runTrav_ $ withExtDeclHandler (analyseAST ast) $ \ext_decl ->
            trace (declTrace ext_decl) (return ())
    mapM print warnings
    print $ pretty global_decls
        
errorOnLeft :: (Show a) => String -> (Either a b) -> IO b
errorOnLeft msg = either (error . ((msg ++ ": ")++).show) return
errorOnLeftM msg action = action >>= errorOnLeft msg

declTrace :: DeclEvent -> String
declTrace event = render $ case event of
                                TagEvent tag_def    -> (text $ "Tag: " ++ show (sueRef tag_def) ++ file tag_def)
                                DeclEvent ident_decl -> (text $ "Decl: " ++ show (identOfDecl ident_decl) ++ file ident_decl)
                                AsmEvent block      -> (text "asm")
    where
    file :: (CNode a) => a -> String
    file = show . posOfNode . nodeInfo
