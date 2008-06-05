{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TestFramework
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD
-- This module provides a small framework for testing the C parser.
-------------------------------------------------------------------------------------------------------
module Language.C.Test.Framework (
-- * Test descriptions
Test(..),testTemplate,
-- * Test results
TestResult(..),initializeTestResult,setTestStatus,
-- * Status of a test
TestStatus(..),testError,isTestError,testFailure,testOk,isTestOk,
-- * Test runs, i.e. a sequence of consecutive (usually dependent) tests of a single test object
TestRun(..),hasTestResults,initFailure,emptyTestResults,insertTest,
-- * Test Configurations
TestConfig(..),withTempFile',
-- * Temporary test data
TestData(..),emptyTestData,cleanTmpFiles,initTestData,setTestRunResults,addTest,addTmpFile,
-- ReExport pretty from the Language.C library
Pretty(..),
-- ReExport the formatting stuff
module Language.C.Test.Measures,
) 
where
import Control.Monad (liftM)
import Control.Monad.Error
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory
import System.Environment (getArgs)
import System.IO (openTempFile,hPutStr,hClose,hPutStrLn,Handle)
import Numeric (showFFloat)
import Text.PrettyPrint
import Language.C.AST.AST (CHeader)
import Numeric (showFFloat)
-- TODO: Refactor the Pretty Class
import Language.C.AST.Pretty (Pretty(..))

import Language.C.Test.CPP (withTempFile)
import Language.C.Test.Measures

-- =====================
-- = Test descriptions =
-- =====================
data Test = Test
  {
    testName :: String,
    testDescr :: String,
    preferredScale :: MetricScale,
    inputUnit :: UnitDescr
  }
  deriving (Show,Read)
testTemplate :: String -> String -> MetricScale -> UnitDescr -> Test
testTemplate testname testdescr preferredscale inputdim =
  Test testname testdescr preferredscale inputdim 

-- ================
-- = Test results =
-- ================
  
-- | Result of a test
data TestResult = 
  TestResult {
    testInfo :: Test,
    testArgs :: [String],
    testStatus :: TestStatus
  }
  deriving (Show,Read)
initializeTestResult :: Test -> [String] -> TestResult
initializeTestResult t args = TestResult t args (testError "not exectued")
setTestStatus :: TestResult -> TestStatus -> TestResult
setTestStatus testresult status = testresult { testStatus = status }

-- | Status of a test
data TestStatus =
    TestError String
  | TestFailure String (Maybe FilePath)
  | TestOk Double Time (Maybe FilePath)
  deriving (Show,Read)
  
testError :: String -> TestStatus
testError = TestError
isTestError :: TestStatus -> Bool
isTestError (TestError _) = True
isTestError _ = False
testFailure :: String -> (Maybe FilePath) -> TestStatus
testFailure errMsg report = TestFailure (errMsg) report
testOk :: (Real a) => a -> Time -> Maybe FilePath -> TestStatus
testOk quantity t report = TestOk (realToFrac quantity) t report
isTestOk :: TestStatus -> Bool
isTestOk (TestOk _ _ _) = True
isTestOk _ = False
formatInputSize :: Test -> Double -> String
formatInputSize testinfo q = formatUnits q (preferredScale testinfo) (inputUnit testinfo)

instance Pretty TestResult where
  pretty (TestResult testinfo testargs teststatus) =
    pretty' ( text (testName testinfo) <+> hsep (map text testargs) ) teststatus
    where
    pretty' ctx (TestError errMsg) =
      ctx <+> text ("ERROR: "++errMsg)
    pretty' ctx (TestFailure errMsg report) =
      ctx <+> text ("FAILED: ")
      $+$ (nest 4 . vcat . catMaybes)
          [ Just (ppErrorMessage errMsg),
            fmap (ppFileRef "report") report ]            
    pretty' ctx (TestOk inpsize ttime report) =
      ctx <+> text "succeeded" <+> stats
      $+$ (nest 4 . vcat . catMaybes)
          [ fmap (ppFileRef "result") report ]
      where
        stats = parens $
              text (formatInputSize testinfo inpsize ++ " in " ++ formatSeconds ttime ++ ", ")
          <+> text (formatUnitsPerTime (inpsize `per` ttime) (preferredScale testinfo) (inputUnit testinfo) (scaleSecs Unit))
          

ppErrorMessage :: String -> Doc
ppErrorMessage =  vcat . map text . filter (not . null) . lines
ppFileRef :: String -> String -> Doc
ppFileRef info file = text $ "See "++info++" file: `"++file++"'"

-- =============
-- = Test Runs =
-- =============

-- | Result of a parser test run
data TestRun = 
    FatalError {
      fatalErrMsg :: String,
      runArgs    :: [String]
    }
   | InitFailure {
      initFailMsg :: String,
      runArgs    :: [String]
    }
  | TestResults {
      testObject :: String,
      testInputFiles :: [FilePath],
      testResults :: Map String TestResult
    }
  deriving (Show,Read)

hasTestResults :: TestRun -> Bool
hasTestResults (TestResults _ _ _) = True
hasTestResults _ = False

instance Pretty TestRun where
  pretty (FatalError { fatalErrMsg = msg, runArgs = args}) =
    text ("Test aborted with fatal error: "++msg) <+> brackets (text "$CC"<+>hsep (map text args))    
  pretty (InitFailure { initFailMsg = msg, runArgs = args }) =
    text ("Test initialization failed: "++msg) <+> brackets (text "$CC"<+>hsep (map text args))
  pretty tr = vcat $ map pretty (Map.elems $ testResults tr) 

initFailure :: String -> [String] -> TestRun
initFailure msg args =
  InitFailure { runArgs = args, initFailMsg = msg }

emptyTestResults :: String -> [FilePath] -> TestRun
emptyTestResults obj inpFs = TestResults { testObject = obj, testInputFiles = inpFs, testResults = Map.empty }

-- | Insert a test
insertTest :: TestResult -> TestRun -> TestRun
insertTest _ (InitFailure _ _) = error "insertTest: initialization failed"
insertTest result trun = trun { testResults = Map.insert (testName $ testInfo result) result (testResults trun) }


-- =======================
-- = Temporary Test data =
-- =======================
data TestConfig = TestConfig 
  {
    debug :: String -> IO (),
    logger :: String -> IO (),
    tmpDir :: FilePath,
    tmpTemplate :: String
  }
withTempFile' :: TestConfig -> String -> (Handle -> IO ()) -> IO FilePath
withTempFile' config ext = withTempFile (tmpDir config) (tmpTemplate config ++ ext)

-- | Test intermediate data
data TestData = TestData
  {
    tempFiles :: [FilePath],
    runResults :: TestRun
  }
emptyTestData :: TestData
emptyTestData = TestData [] undefined
cleanTmpFiles :: TestData -> IO ()
cleanTmpFiles td = mapM_ removeFile (tempFiles td)
initTestData :: TestRun -> TestData
initTestData runsInit = emptyTestData { runResults = runsInit }
setTestRunResults :: TestRun -> TestData -> TestData
setTestRunResults tr testData = testData { runResults = tr }
addTest :: TestResult -> (TestData -> TestData)
addTest result testData = testData { runResults = insertTest result (runResults testData) }
addTmpFile :: FilePath -> (TestData -> TestData)
addTmpFile tmpFile testData = testData { tempFiles = (tmpFile : tempFiles testData) } 
