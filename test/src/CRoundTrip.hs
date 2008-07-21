{-# OPTIONS -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  CRoundtrip.hs (executable)
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
--
-- This module is invoked just like gcc. It preprocesses the C source file in the given argument list,
-- parses it, pretty prints it again, and compares the two ASTs.
--
-- Tests are logged, and serialized into a result file.
-- If `CRoundtrip' finishes without runtime error, it always returns ExitSuccess.
--
-- see 'TestEnvironment'.
-----------------------------------------------------------------------------
module Main (main)  where
import Control.Monad.State
import System.FilePath (takeBaseName)
import Text.PrettyPrint

import Language.C.Syntax.Position
import Language.C.Test.Environment
import Language.C.Test.Framework
import Language.C.Test.ParseTests
import Language.C.Test.TestMonad

main :: IO ()
main = defaultMain usage roundtripTest

usage :: Doc
usage = text "./CRoundTrip <gcc-args> file.(c|hc|i)"
        $$ (nest 4 $ text "Test Driver: preprocess, parse, pretty print, parse again, and compare ASTs")
        $+$ envHelpDoc []

roundtripTest :: [String] -> TestMonad ()
roundtripTest args =
  case mungeCcArgs args of
    Ignore         -> errorOnInit args $ "No C source file found in argument list: `cc "  ++ unwords args ++ "'"
    Unknown err    -> errorOnInit args $ "Could not munge CC args: " ++ err ++ " in  `cc "++ unwords args ++ "'"
    Groked [origFile] gccArgs -> roundtripTest' origFile gccArgs
    Groked cFiles _ -> errorOnInit args $ "More than one c source file given: "++ unwords cFiles

roundtripTest' :: FilePath -> [String] -> TestMonad ()
roundtripTest' origFile gccArgs = do
    modify $ setTmpTemplate (takeBaseName origFile)
    (cFile, preFile) <- runCPP origFile gccArgs
    modify $ setTestRunResults (emptyTestResults (takeBaseName origFile) [cFile])

    -- parse
    let parseTest1 = initializeTestResult (parseTestTemplate { testName = "01-parse" }) [origFile]
    parseResult <- runParseTest preFile (Position cFile 1 1)
    addTestM $
      setTestStatus parseTest1 $ 
        either (uncurry testFailWithReport) (testOkNoReport . snd) parseResult
    ast <- either (const exitTest) (return . fst) parseResult

    -- pretty print
    let prettyTest = initializeTestResult (ppTestTemplate { testName = "02-pretty-print" }) [origFile]
    ((prettyFile,report),metric) <- runPrettyPrint ast
    addTestM $
      setTestStatus prettyTest $
        testOkWithReport metric report 

    -- parse again (TODO: factor out code duplication with first parse test)
    let parseTest2 = initializeTestResult (parseTestTemplate { testName = "03-parse-pretty-printed" }) [prettyFile]
    parseResult2 <- runParseTest prettyFile (Position prettyFile 1 1)
    addTestM $
      setTestStatus parseTest2 $ 
        either (uncurry testFailWithReport) (testOkNoReport . snd) parseResult2
    ast2 <- either (const exitTest) (return . fst) parseResult2

    -- check equiv
    let equivTest = initializeTestResult (equivTestTemplate { testName = "04-orig-equiv-pp" }) []
    equivResult <- runEquivTest ast ast2
    addTestM $
      setTestStatus equivTest $
        either (uncurry testFailure) testOkNoReport equivResult
    return ()
