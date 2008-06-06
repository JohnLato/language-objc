{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-# OPTIONS -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  CRoundtrip.hs
-- Copyright   :  (c) 2008 Benedikt Huber
--
-- This module is invoked just like gcc. It preprocesses the .c argument,
-- parses it, pretty prints it again, and compares the two ASTs.
--
-- Tests are logged, and serialized into a result file.
-- If the CRoundtrip finishes without runtime error, it always returns ExitSuccess.
--
-- see 'TestEnvironment'.
-----------------------------------------------------------------------------
module Main (main)  where
import Control.Monad.Cont 
import Control.Monad.State
import System.Environment (getArgs)
import System.Exit
import System.Cmd (rawSystem)
import System.IO (appendFile)
import System.Directory (copyFile)
import System.FilePath (takeBaseName)

import Language.C.Toolkit.Position
import Language.C.Test.CPP
import Language.C.Test.Framework
import Language.C.Test.ParseTests
import Language.C.Test.TestEnvironment (getEnvConfig)

main :: IO ()
main = do
  (config,resultFile) <- getEnvConfig
  
  -- get arguments and run tests
  args <- getArgs
  testDat <- runTests config args (mungeCcArgs args)

  -- write results
  debug config $ "Finished test\n"

  appendFile resultFile (show (runResults testDat) ++ "\n")
  debug config $ "Wrote test results. Cleaning up.\n"

  cleanTmpFiles testDat

addTestM :: (MonadState TestData m, MonadIO m) => TestConfig -> TestResult -> m ()
addTestM config result = do
  modify $ addTest result  
  liftIO $ logger config $ show (pretty result) ++ "\n"

runTests :: TestConfig -> [String] -> MungeResult -> IO TestData
runTests iConfig args mungeResult = 
  execStateT (runContT runTests' return) emptyTestData
  where
  runTests' :: forall m. (MonadState TestData m, MonadCont m, MonadIO m) => m ()
  runTests' =

   callCC $ \cc ->
   let exitTest = cc () >> error "Internal call/cc error" in
   let errorOnInit :: String -> m a
       errorOnInit msg = do
         liftIO $ debug iConfig $ "Failed to initialize " ++ msg ++ "\n"
         liftIO $ logger iConfig $ show (initFailure msg args)
         modify (setTestRunResults (initFailure msg args))
         exitTest in
   case mungeResult of
    Ignore         -> errorOnInit $ "No C source file found in argument list: `cc "  ++ unwords args ++ "'"
    Unknown err    -> errorOnInit $ "Could not munge CC args: " ++ err ++ " in  `cc "++ unwords args ++ "'"
    Groked origFile gccArgs -> do
      
      -- initialize config
      let config = iConfig { tmpTemplate = takeBaseName origFile }
      let dbgMsg = liftIO . debug config
      
      -- copy original file (for reporting)
      cFile <- liftIO $ withTempFile' config ".c" $ \_ -> return ()
      copySuccess <- liftIOCatched (copyFile origFile cFile)
      case copySuccess of
        Left err -> errorOnInit $ "Copy failed: " ++ show err
        Right () -> dbgMsg      $ "Copy: " ++ origFile ++ " ==> " ++ cFile ++ "\n"

      -- preprocess C file
      dbgMsg $ "Preprocessing " ++ origFile ++ "\n"
      preFile     <- liftIO $ withTempFile' config ".i" $ \_hnd -> return ()
      gccExitcode <- liftIO $ rawSystem "gcc" (["-E", "-o", preFile] ++ gccArgs)
      case gccExitcode of 
        ExitSuccess       ->  
          modify $ addTmpFile preFile
        ExitFailure fCode ->
          errorOnInit $ "C preprocessor failed: " ++ "`gcc -E -o " ++ preFile ++ " " ++ origFile ++ 
                        "' returned exit code `" ++ show fCode ++ "'"

      modify $ setTestRunResults (emptyTestResults (takeBaseName origFile) [cFile])

      -- parse
      let parseTest1 = initializeTestResult (parseTestTemplate { testName = "01-parse" }) [origFile]
      parseResult <- liftIO$ runParseTest config preFile (Position cFile 1 1)
      addTestM config $
        setTestStatus parseTest1 $ 
          either (uncurry testFailWithReport) (testOkNoReport . snd) parseResult
      ast <- either (const exitTest) (return . fst) parseResult

      -- pretty print
      let prettyTest = initializeTestResult (ppTestTemplate { testName = "02-pretty-print" }) [origFile]
      ((prettyFile,report),metric) <- runPrettyPrint config ast
      addTestM config $
        setTestStatus prettyTest $
          testOkWithReport metric report 

      -- parse again (TODO: factor out code duplication with first parse test)
      let parseTest2 = initializeTestResult (parseTestTemplate { testName = "03-parse-pretty-printed" }) [prettyFile]
      parseResult2 <- liftIO$ runParseTest config prettyFile (Position prettyFile 1 1)
      addTestM config $
        setTestStatus parseTest2 $ 
          either (uncurry testFailWithReport) (testOkNoReport . snd) parseResult2
      ast2 <- either (const exitTest) (return . fst) parseResult2

      -- check equiv
      let equivTest = initializeTestResult (equivTestTemplate { testName = "04-orig-equiv-pp" }) []
      equivResult <- runEquivTest config ast ast2
      addTestM config $
        setTestStatus equivTest $
          either (uncurry testFailure) testOkNoReport equivResult
      return ()

liftIOCatched :: (MonadIO m) => IO a -> m (Either IOError a)
liftIOCatched a = liftIO $ liftM Right a `catch` (return . Left)
