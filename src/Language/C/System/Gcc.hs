-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.System.Gcc
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  experimental
-- Portability :  unspecified
--
-- Invoking gcc for preprocessing and compiling
-----------------------------------------------------------------------------
module Language.C.System.Gcc (
    gccCPP, -- :: FilePath -> Preprocessor
)
where
import Language.C.System.Preprocess
import Data.Maybe
import System.Cmd

gccCPP :: FilePath -> Preprocessor
gccCPP gcc_path cpp_args extra_args = do
    let args = buildCppArgs cpp_args extra_args
    rawSystem gcc_path args

buildCppArgs :: CppArgs -> [String] -> [String]
buildCppArgs (CppArgs options _tmpdir input_file output_file_opt) extra_args = do
       (concatMap tOption options)
    ++ outputFileOpt
    ++ ["-E", input_file]
    ++ extra_args
    where
    tOption (IncludeDir incl)  = ["-I",incl]
    tOption (Define key value) = [ "-D" ++ key ++ (if null value then "" else "=" ++ value) ]
    tOption (Undefine key)     = [ "-U" ++ key ]
    tOption (IncludeFile f)    = [ "-include", f]
    outputFileOpt = concat [ ["-o",output_file] | output_file <- maybeToList output_file_opt ]

