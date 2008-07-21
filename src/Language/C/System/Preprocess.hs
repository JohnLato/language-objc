-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Wrapper.Preprocess
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  experimental
-- Portability :  unspecified
--
-- Wrapper for invoking a preprocessor
-----------------------------------------------------------------------------
module Language.C.System.Preprocess (
    CppOption(..),
    CppArgs(..),defaultCppArgs,addCppOption,
    Preprocessor,
    runPreprocessor,
)
where
import Language.C.InputStream
import System.Exit
import System.Directory
import System.FilePath
import System.Environment
import System.IO
import Control.Exception
import Control.Monad
-- | file extension of a preprocessed file
preprocessedExt :: String
preprocessedExt = ".i"

-- | Generic Options for the preprocessor
data CppOption =
        IncludeDir FilePath
      | Define String String
      | Undefine String
      | IncludeFile FilePath
      
-- | Generic arguments for the preprocessor
data CppArgs = CppArgs { 
        cppOptions :: [CppOption], 
        cppTmpDir  :: Maybe FilePath,
        inputFile  :: FilePath,
        outputFile :: Maybe FilePath
    }
    
defaultCppArgs :: FilePath -> CppArgs
defaultCppArgs input_file = CppArgs { inputFile = input_file, cppOptions = [], outputFile = Nothing, cppTmpDir = Nothing }

addCppOption :: CppArgs -> CppOption -> CppArgs
addCppOption cpp_args opt = cpp_args { cppOptions = opt : (cppOptions cpp_args) }

type Preprocessor = CppArgs -> [String] -> IO ExitCode

runPreprocessor :: Preprocessor -> CppArgs -> [String] -> IO (Either ExitCode InputStream)
runPreprocessor cpp cpp_args extra_args = do
    bracket
        getActualOutFile
        -- remove outfile if it was temporary
        removeTmpOutFile
        -- invoke preprocessor
        invokeCpp
    where
    getActualOutFile :: IO FilePath
    getActualOutFile = maybe (mkOutputFile (cppTmpDir cpp_args) (inputFile cpp_args)) return (outputFile cpp_args)
    invokeCpp actual_out_file = do
        exit_code <- cpp (cpp_args { outputFile = Just actual_out_file}) extra_args
        case exit_code of
            ExitSuccess   -> liftM Right (readInputStream actual_out_file)
            ExitFailure _ -> return $ Left exit_code
    removeTmpOutFile out_file = maybe (removeFile out_file) (\_ -> return ()) (outputFile cpp_args)
        
-- | create an output file, given  @Maybe tmpdir@ and @inputfile@
mkOutputFile :: Maybe FilePath -> FilePath -> IO FilePath
mkOutputFile tmp_dir_opt input_file =
    do tmpDir <- getTempDir tmp_dir_opt
       mkTmpFile tmpDir (getOutputFileName input_file)
    where
    getTempDir (Just tmpdir) = return tmpdir
    getTempDir Nothing       = getTemporaryDirectory

-- | compute output file name from input file name
getOutputFileName :: FilePath -> FilePath
getOutputFileName fp | hasExtension fp = replaceExtension filename preprocessedExt
                     | otherwise       = addExtension filename preprocessedExt
    where
    filename = takeFileName fp
-- | create a temporary file
mkTmpFile :: FilePath -> FilePath -> IO FilePath
mkTmpFile tmp_dir file_templ = do
    putStrLn $ "TmpDir: "++tmp_dir
    putStrLn $ "FileTempl: "++file_templ
    (path,file_handle) <- openTempFile tmp_dir file_templ
    hClose file_handle
    return path
