{-# OPTIONS -XPatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  RenderTests
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  
--
-- This module renders test results into HTML files, much like pugs smoke tests.

-- TODO: Use stylesheets, not bgcolor attribute
-- TODO: Performance is sub-optimal
-- TODO: It would be nice to have Javascript filter and sort for detailled view.
-- TOOD: Display performance in detailled view too (maybe only if differs significantly from the average performance)
-----------------------------------------------------------------------------
module Main (main) where
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import System
import System.IO
import System.Directory (getCurrentDirectory)
import System.FilePath
import Numeric (showFFloat)
import Text.Printf
import Text.XHtml

import Language.C.Test.Framework

-- extended Filepath.normalise
-- we want to have @normalise' "/Foo/./bar/.././../baz"@ ==> @"/baz"@
-- Do not know how to accomplish this with System.FilePath ...
normalise' :: FilePath -> FilePath
normalise' = joinPath . reverse . foldl removeDotDot [] . splitPath . normalise
  where 
    removeDotDot (dir:dirs) dotDot | dropTrailingPathSeparator dotDot == "..", not (isAbsolute dir) = dirs
    removeDotDot (dir:dirs) dot | dropTrailingPathSeparator dot == "." = (dir:dirs)
    removeDotDot dirs c = c:dirs
    
-- html helpers
floatToHtml :: (RealFloat a) => Int -> a -> Html
floatToHtml prec f = toHtml $ showFFloat (Just prec) f ""

stringCell :: String -> HtmlTable
stringCell = cell . td . toHtml

sortableTable :: String -> HtmlTable -> Html
sortableTable tid tbl = defaultTable tbl ! [ theclass "sortable", identifier tid ]
defaultTable :: HtmlTable -> Html
defaultTable tbl = table (toHtml tbl) ! [border 1, cellpadding 10]

mkTable :: [Html] -> [[Html]] -> Html
mkTable tableHeader tableRows = 
  simpleTable [border 1, cellpadding 10] [] (tableHeader : tableRows)

htmlFile :: String -> Html -> String
htmlFile title body = prettyHtml $ 
  header << 
    (
      (thetitle << title)
--    +++ (script << "") ! [ thetype "text/javascript", src "sortable.js"] -- doesn't work yet
    )
  +++ body

-- | read the dat file containing the test-results
readTestRuns :: FilePath -> IO [TestRun]
readTestRuns = liftM (map read . lines) . readFile

-- Summarize a set of test runs
data TestSetResult = TestSetResult
  {
    testSetName :: String,
    allOk         :: Int,
    someFailed    :: Int,
    initErrors    :: Int,
    fatalErrors   :: Int,
    testSummaries :: Map String TestSummary,
    testRuns      :: [TestRun]
  }
initTestSetResult :: String -> [TestRun] -> TestSetResult
initTestSetResult tsname tsruns = TestSetResult { testSetName = tsname, 
                                                    allOk = 0, someFailed = 0,
                                                    initErrors = 0, fatalErrors = 0,
                                                    testSummaries = Map.empty, testRuns = tsruns }
executedTests :: TestSetResult -> Int
executedTests tsr = allOk tsr + someFailed tsr
totalTestRuns :: TestSetResult -> Int
totalTestRuns tsr = executedTests tsr + fatalErrors tsr + initErrors tsr

-- Summarizes one specific test in a test suite
data TestSummary = TestSummary 
  {
    sTestInfo     :: Test,
    numOk         :: Int,
    numFailed     :: Int,
    totalEntities :: Double,
    totalTime     :: Time
  }
  deriving (Show,Read)

throughput :: TestSummary -> Double
throughput ts = (totalEntities ts)  `per` (totalTime ts)

numTests :: TestSummary -> Int
numTests s = numOk s + numFailed s

initSummary :: Test -> TestSummary
initSummary t = TestSummary { sTestInfo = t, numOk = 0, numFailed = 0, totalEntities = 0, totalTime = 0 }

-- =====================
-- = Compute summaries =
-- =====================
  
computeSummary :: String -> [TestRun] -> TestSetResult
computeSummary tsname testRuns = 
  foldr updateSetSummary (initTestSetResult tsname testRuns) testRuns

updateSetSummary :: TestRun -> TestSetResult -> TestSetResult
updateSetSummary (FatalError _ _) s = s { fatalErrors = fatalErrors s + 1}
updateSetSummary (InitFailure _ _) s = s { initErrors = initErrors s + 1 }
updateSetSummary (TestResults _obj _files results) s =
  updateTestCount (Map.elems results) $
  s { testSummaries = foldr addToSummary (testSummaries s) (Map.elems results) }
  where
    updateTestCount rs  s | all (isTestOk . testStatus) rs = s { allOk = allOk s + 1 }
                          | otherwise                      = s { someFailed = someFailed s + 1 }

addToSummary :: TestResult -> Map String TestSummary -> Map String TestSummary
addToSummary (TestResult testinfo _ teststatus) sums 
  | (isTestError teststatus) = sums
  | otherwise = Map.alter alterSummary (testName testinfo) sums
  where
    alterSummary Nothing = alterSummary (Just (initSummary testinfo))
    alterSummary (Just s) = Just$
      case teststatus of
        (TestError msg) -> s
        (TestFailure msg report) -> s { numFailed = succ (numFailed s) }        
        (TestOk processed elapsed_t report) -> 
          s { numOk = succ (numOk s), totalEntities = totalEntities s + processed, totalTime = (totalTime s) + elapsed_t }

-- =========
-- = Files =
-- =========
  
datFile :: String -> FilePath
datFile testname = testname ++ ".dat"

indexFile :: String
indexFile = "index.html"

testSetFile :: TestSetResult -> String
testSetFile tss = (testSetName tss) ++ ".html"
-- ====================
-- = main entry point =
-- ====================
  
main :: IO ()
main = do
  args <- getArgs
  when (length args < 2) $ do
    hPutStrLn stderr "Usage: ./SummarizeTest parser-version test-names"
    exitWith (ExitFailure 1)
  (parserVersion : tests) <- getArgs
  testRuns <- liftM (zip tests) $ mapM (readTestRuns.datFile) tests
  -- make file references relative to the current directory (for publishing)
  pwd <- getCurrentDirectory
  let normalizeFilePath = makeRelative pwd . normalise'
  
  -- compute summary
  let testResults = map (uncurry computeSummary) testRuns
  -- export index file
  writeFile indexFile $
    htmlFile ("Test result overviews") $ 
      indexContents parserVersion testResults
  -- export detailed file
  forM_ testResults $ \testResult ->
    writeFile (testSetFile testResult) $ 
      htmlFile ("Test results for "++ testSetName testResult) $ 
        detailedContents normalizeFilePath testResult        

-- ==================
-- = HTML rendering =
-- ==================
  
-- | create index.html
indexContents :: String -> [TestSetResult] -> Html
indexContents parserVersion tsresults = 
       h1 << "Test results"
  +++  p  << ("Test with Language.C, "++parserVersion)
  +++  h2 << "Overview"
  +++  overviewTable tsresults
  +++  h2 << "Test Summaries"
  +++  concatHtml (map testSummary tsresults)
  where
    overviewTable results = 
      mkTable 
        (map toHtml ["test set name","total tests", "init error", "fatal error", "tests run", "all tests ok", "some tests failed" ])
        (map overviewRow results ++ [overviewSummaryRow results])
    overviewRow tsr = (testSetLink tsr) : 
                      map (toHtml.show) [totalTestRuns tsr, initErrors tsr, fatalErrors tsr, 
                                         executedTests tsr, allOk tsr, someFailed tsr ]
    overviewSummaryRow rs = stringToHtml "Total" :
                            map (toHtml.show)  [ sumMap totalTestRuns rs, sumMap initErrors rs, sumMap fatalErrors rs,
                                                 sumMap executedTests rs, sumMap allOk rs, sumMap someFailed rs]
    sumMap f = sum . map f
    testSetLink tsr = (anchor << testSetName tsr) ! [href (testSetFile tsr)]
    testSummary tsr =
          h3 << (testSetLink tsr)
      +++ summaryView tsr

-- | create testset.html
detailedContents :: (FilePath -> FilePath) -> TestSetResult -> Html
detailedContents normRef tsr = 
      (anchor << "Contents") ! [href "index.html"]
  +++ h1 << ("Test Results for " ++ testSetName tsr)
  +++ h2 << "Summary"
  +++ summaryView tsr
  +++ h2 << "Detailed View"
  +++ detailedView normRef tsr


-- * Summary of XXX.dat
-- Executed %d out of %d tests
-- Summary-Table
summaryView :: TestSetResult -> Html
summaryView tsr = 
      p << (printf "Executed %d out of %d tests"  (length $ filter hasTestResults runs) (length runs) :: String)
  +++  (defaultTable $ summaryTable (Map.elems $ testSummaries tsr))
  where
    runs = testRuns tsr

    
summaryTable :: [TestSummary] -> HtmlTable
summaryTable summaries = aboves $ header : map mkRow summaries
  where
    header = besides $ map (td . toHtml) $ words "Test Ok Failed InputSize Time Throughput"
    mkRow = besides . map td . summaryEntries
    summaryEntries ts =
      let testinfo = sTestInfo ts in
      map stringToHtml
      [
        testName testinfo, 
        show$ numOk ts, 
        show$ numFailed ts, 
        formatUnitsSafe (totalEntities ts) (preferredScale testinfo) (inputUnit testinfo),
        formatTimeSafe (totalTime ts) (scaleSecs Unit),
        formatUnitsPerSecond (throughput ts) (preferredScale testinfo) (inputUnit testinfo) 
      ]

-- 
-- create HTML for detailled view
-- Table
-- | test_1 | ... | test_n |
-- |  2 KLoC / 4 s (green) | Failed (link-to-report) (red) | NotAvailable (gray)
-- | columnspan (InitError: ) :gray  
detailedView :: (FilePath -> FilePath) -> TestSetResult -> Html
detailedView normRef tsr =
       h1 (toHtml$ "Detailed Report")
  +++  sortableTable "detailed_view_table" (detailedTable (Set.toList allKeys) (testRuns tsr))
  where
    allKeys = Set.fromList . map (testName . sTestInfo) . Map.elems . testSummaries $ tsr

    detailedTable testkeys runs =
      aboves $ (detailedHeader ("Test Objective" : "Input Files" : testkeys)
               : map (detailedRow testkeys) runs)
    detailedHeader testkeys = besides $ map (th <<) testkeys

    detailedRow _testkeys (FatalError msg args) = cell $
      (td << linesToHtml (("Fatal Error " ++ show args) : lines msg)) ! [ bgcolor "#F08080" {- LightCoral -} ]

    detailedRow _testkeys (InitFailure msg args) = cell $
      (td << linesToHtml (("Test Initialization Failure on " ++ show args) : lines msg)) ! [ bgcolor "lightblue" ]

    detailedRow testkeys (TestResults testobject filesUnderTest results) = 
      (stringCell testobject)
      `beside` (filesCell filesUnderTest)
      `beside` (besides $ map (detailedCell results) testkeys)  

    filesCell :: [FilePath] -> HtmlTable
    filesCell = cell . td . concatHtml . map fileref where
      fileref fp = (anchor << takeFileName fp) ! [href $ normRef fp] +++ br

    detailedCell :: (Map.Map String TestResult) -> String -> HtmlTable
    detailedCell results key =
      cell$ case Map.lookup key results of
        Nothing                                        -> td (toHtml "n/a") ! [bgcolor "lightgrey"]
        Just (TestResult testinfo testargs teststatus) -> statusCell teststatus

    statusCell  :: TestStatus -> Html
    statusCell (TestError errMsg)              = (td << errMsg) ! [bgcolor "blue"]
    statusCell (TestFailure errMsg reportfile) = (td << failureCell errMsg reportfile) ! [bgcolor "red"]
    statusCell (TestOk sz elapsed mResultFile) = (td << okCell mResultFile) ! [bgcolor "green"]

    failureCell :: String -> Maybe FilePath -> Html
    failureCell errMsg (Just report) = anchor (toHtml "Failure")  ! [href $ normRef report, title errMsg]
    failureCell errMsg Nothing = toHtml $ "Failure: "++errMsg

    okCell :: Maybe FilePath -> Html
    okCell Nothing = toHtml $ "Ok"
    okCell (Just f) = anchor (toHtml "Ok") ! [href $ normRef f]
