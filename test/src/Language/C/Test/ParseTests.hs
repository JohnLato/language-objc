{-# LANGUAGE FlexibleContexts #-} 
{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ParseTests
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD
--
-- Provides a set of tests for the parser and pretty printer.
-----------------------------------------------------------------------------
module Language.C.Test.ParseTests (
-- * Misc helpers
time, lineCount, withFileExt,
-- * datatype for measuring performance
PerfMeasure,elapsedTime,processedEntities,
testFailNoReport,testFailWithReport,testOkNoReport,testOkWithReport,
-- * Tests
parseTestTemplate, runParseTest,
ppTestTemplate, runPrettyPrint,
equivTestTemplate,runEquivTest,
) where
import Control.Monad.State
import Control.Monad.Instances
import Data.List
import System.Directory 
import System.IO
import System.Cmd
import System.CPUTime 
import System.FilePath (takeBaseName)

import Language.C
import Language.C.Toolkit.Position

import Language.C.Test.CPP
import Language.C.Test.GenericAST
import Language.C.Test.Framework

-- ===================
-- = Misc            =
-- ===================


time :: IO a -> IO (a, Time)
time action = do
  start <- getCPUTime
  r <- action
  end   <- getCPUTime
  let durSecs = picoSeconds (end - start)
  return (r, durSecs)

lineCount :: FilePath -> IO Int
lineCount = liftM (length . lines) . readFile

-- | change filename extension
withFileExt :: FilePath -> String -> FilePath
withFileExt filename ext = (stripExt filename) ++ "." ++ ext where
  stripExt fn = 
    let basefn = takeBaseName fn in
    case (dropWhile (/= '.') . reverse) basefn of
      ('.' : s : ss) -> reverse (s : ss)
      _ -> basefn

newtype PerfMeasure = PerfMeasure (Integer , Time)

elapsedTime :: PerfMeasure -> Time
elapsedTime (PerfMeasure (_,t)) = t
processedEntities :: PerfMeasure -> Integer
processedEntities (PerfMeasure (sz,_)) = sz

testFailNoReport :: String -> TestStatus
testFailNoReport errMsg = testFailure errMsg (Nothing)
testFailWithReport :: String -> FilePath -> TestStatus
testFailWithReport errMsg report = testFailure errMsg (Just report)
testOkNoReport :: PerfMeasure -> TestStatus
testOkNoReport m = testOk (processedEntities m) (elapsedTime m) Nothing
testOkWithReport :: PerfMeasure -> FilePath -> TestStatus
testOkWithReport m r = testOk (processedEntities m) (elapsedTime m) (Just r)

-- ===============
-- = Parse tests =
-- ===============

parseTestTemplate :: Test
parseTestTemplate = Test
  {
    testName = "parse",
    testDescr = "parse the given preprocessed c file",
    preferredScale = Kilo,
    inputUnit = linesOfCode
  }

runParseTest :: TestConfig
             -> FilePath           -- ^ preprocesed file
             -> Position           -- ^ initial position
             -> IO (Either (String,FilePath) (CHeader,PerfMeasure)) -- ^ either (errMsg,reportFile) (ast,(locs,elapsedTime))
runParseTest conf preFile initialPos = do
      let dbgMsg = debug conf
      -- parse
      dbgMsg $ "Starting Parse of " ++ preFile ++ "\n"
      ((parse,input),elapsed) <-
        time $ do input <- readFile preFile
                  parse <- parseEval input initialPos
                  return (parse,input)
      -- check error and add test
      dbgMsg $ "Parse result : " ++ eitherStatus parse ++ "\n"
      case parse of
        Left err@(errMsgs, pos) -> do
          report <- reportParseError conf err input
          return $ Left $ (unlines (("Parse error in " ++ show pos) : errMsgs), report)
        Right header -> 
          return $ Right $ (header,PerfMeasure (locsOf input,elapsed))

reportParseError :: TestConfig -> ([String],Position) -> String -> IO FilePath
reportParseError config (errMsgs,pos) input = do
  withTempFile' config ".report" $ \hnd -> do
    pwd <- getCurrentDirectory
    contextMsg <- getContextInfo pos
    hPutStr hnd $ "failed to parse " ++ (posFile pos)
               ++ "\nwith message:\n" ++ concat errMsgs ++ " " ++ show pos
               ++ "\n" ++ contextMsg
               ++ "\nworking dir: " ++ pwd
               ++ "\npreprocessed input follows:\n\n" ++ input

-- ======================
-- = Pretty print tests =
-- ======================
ppTestTemplate :: Test
ppTestTemplate = Test
  {
    testName = "pretty-print",
    testDescr = "pretty-print the given AST",
    preferredScale = Kilo,
    inputUnit = linesOfCode
  }

runPrettyPrint :: (MonadIO m, MonadState TestData m) =>
                  TestConfig -> CHeader -> m ((FilePath, FilePath), PerfMeasure)
runPrettyPrint config ast = do
    let dbgMsg = liftIO . debug config
    -- pretty print
    dbgMsg "Pretty Print ..."
    (fullExport,t) <-
      liftIO . time $
        withTempFile' config "pp.c" $ \hnd -> 
          hPutStrLn hnd $ show (pretty ast)
    modify $ addTmpFile fullExport
    locs <- liftIO $ lineCount fullExport

    dbgMsg $ " to " ++ fullExport ++ " (" ++ show locs ++ " lines)"++ "\n"
    
    -- export the parsed file, with headers via include
    dbgMsg $ "Pretty Print [report] ... "
    smallExport <- liftIO $ withTempFile' config "ppr.c" $ \hnd ->
      hPutStrLn hnd $ show (prettyUsingInclude ast)
    dbgMsg $ "to " ++ smallExport ++ "\n"

    lc <- liftIO $ lineCount fullExport
    return ((fullExport,smallExport), PerfMeasure (fromIntegral lc,t))

-- ===============
-- = Equiv Tests =
-- ===============
equivTestTemplate :: Test
equivTestTemplate = Test
  {
    testName = "equivalence check",
    testDescr = "check if two ASTs are equivalent",
    preferredScale = Unit,
    inputUnit = topLevelDeclarations
  }

runEquivTest :: (MonadIO m, MonadState TestData m) =>
                TestConfig -> CHeader -> CHeader -> m (Either (String, Maybe FilePath) PerfMeasure)
runEquivTest config (CHeader decls1 _) (CHeader decls2 _) = do
  let dbgMsg = liftIO . debug config
  dbgMsg $ "Check AST equivalence\n"
  
  -- get generic asts
  (result,t) <- liftIO . time $ do
    let ast1 = map toGenericAST decls1
    let ast2 = map toGenericAST decls2
    if (length ast1 /= length ast2)
      then 
        return $ Left ("Length mismatch: " ++ show (length ast1) ++ " vs. " ++ show (length ast2), Nothing)
      else 
        case find (\(_, (d1,d2)) -> d1 /= d2) (zip [0..] (zip ast1 ast2)) of
          Just (ix, (decl1,decl2)) -> do
            declf1 <- withTempFile' config ".1.ast" $ \hnd -> hPutStrLn hnd (show $ pretty decl1)
            declf2 <- withTempFile' config ".2.ast" $ \hnd -> hPutStrLn hnd (show $ pretty decl2)
            diff   <- withTempFile' config ".ast_diff" $ \_hnd -> return ()
            appendFile diff ("Original declaration: " ++ show (pretty $ decls1 !! ix) ++ "\n")
            appendFile diff ("Pretty printed declaration: " ++ show (pretty $ decls2 !! ix) ++ "\n")
            system $ "diff -u '" ++ declf1 ++ "' '" ++ declf2 ++ "' >> '" ++ diff ++ "'" -- TODO: escape ' in filenames
            removeFile declf1
            removeFile declf2
            return $ Left ("Declarations do not match: ", Just diff)
          Nothing -> return $ Right (length ast1)
  return $ either Left (\decls -> Right $ PerfMeasure (fromIntegral decls, t)) result

-- ===========
-- = Helpers =
-- ===========

--  make sure parse is evaluated
parseEval :: String -> Position -> IO (Either ([String],Position) CHeader)
parseEval input initialPos = 
  case parseC input initialPos of 
    Left  err -> return $ Left err
    Right ok ->  return $ Right ok

eitherStatus :: Either a b -> String
eitherStatus = either (const "ERROR") (const "ok")

getContextInfo :: Position -> IO String
getContextInfo pos = do
  cnts <- readFile (posFile pos)
  return $ case drop (posRow pos - 1) (lines cnts) of
    [] -> "/* End of File */"
    (line:_) ->    line ++ "\n"
                ++ (replicate (posColumn pos - 1) ' ') ++ "^^^"
locsOf :: String -> Integer
locsOf = fromIntegral . length . lines
                