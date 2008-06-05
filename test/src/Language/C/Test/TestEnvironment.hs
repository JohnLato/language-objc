{-# OPTIONS -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TestEnvironment
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  
--
-- This module provides access to the environment variables used for testing
--
-- CTEST_TMPDIR        : Directory to create temporary files in
-- CTEST_REPORT_FILE   : [optional] file to write test reports to (default = $CTEST_TMPDIR/report.dat)
-- CTEST_LOGFILE       : [optional] log file                      (default = stderr)
-- CTEST_DEBUG         : [optional] debug flag                    (default = False)
-----------------------------------------------------------------------------
module Language.C.Test.TestEnvironment (
getEnvConfig
)
where
import Control.Monad (liftM)
import qualified Data.Map as Map
import System.IO
import System.Environment
import System.FilePath (combine)
import Language.C.Test.Framework

tmpdirEnvVar :: String
tmpdirEnvVar = "CTEST_TMPDIR"
logfileEnvVar :: String
logfileEnvVar = "CTEST_LOGFILE"
reportFileEnvVar :: String
reportFileEnvVar = "CTEST_REPORT_FILE"
debugEnvVar :: String
debugEnvVar = "CTEST_DEBUG"

defaultReportFile :: String
defaultReportFile = "report.dat"

getEnvConfig :: IO (TestConfig, FilePath)
getEnvConfig = do
  environ <- liftM Map.fromList getEnvironment
  -- get log dir, result file and debug flag
  tmpdir  <- getEnv tmpdirEnvVar
  let resultFile = maybe (combine tmpdir defaultReportFile) id $ Map.lookup reportFileEnvVar environ
  let debugFlag  = maybe False (const True)   $ Map.lookup debugEnvVar environ
  let logFile    = maybe Nothing Just         $ Map.lookup logfileEnvVar environ
  let config = TestConfig {
                    debug  = debugAction debugFlag,
                    logger = logAction logFile,
                    tmpDir = tmpdir,
                    tmpTemplate = "ctest-report"
                  }
  return (config,resultFile)
  where
    debugAction False = \_ -> return ()
    debugAction True  = hPutStr stderr . ("[DEBUG] "++)
    logAction   Nothing = hPutStr stderr
    logAction   (Just logFile) = appendFile logFile
    
