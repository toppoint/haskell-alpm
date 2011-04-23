{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Core (tests) where

import Data.Maybe
import Test.Framework

import Distribution.ArchLinux.ALPM.Core

import Util

tests :: TestSuite
tests = allHTFTests

test_initialize_release :: Assertion
test_initialize_release = alpmTest $ assertBool True

test_getVersion :: Assertion
test_getVersion = alpmTest $ do
    version <- getVersion
    assertBool $ not $ null version

test_optionGetRoot :: Assertion
test_optionGetRoot = alpmTest $ do
    root <- optionGetRoot
    assertBool $ isNothing root

test_optionSetRoot :: Assertion
test_optionSetRoot = alpmTest $ do
    let root = "/tmp/"
    me <- optionSetRoot root

    case me of
        Just e ->
            assertFailure $ "optionSetRoot: " ++ show e

        Nothing -> do
            mRoot <- optionGetRoot
            assertEqual mRoot $ Just root

test_optionGetDBPath :: Assertion
test_optionGetDBPath = alpmTest $ do
    dbPath <- optionGetDBPath
    assertBool $ isNothing dbPath

test_optionSetDBPath :: Assertion
test_optionSetDBPath = alpmTest $ do
    let dbPath = "/tmp/"
    me <- optionSetDBPath dbPath

    case me of
        Just e ->
            assertFailure $ "optionSetDBPath: " ++ show e

        Nothing -> do
            mDBPath <- optionGetDBPath
            assertEqual mDBPath $ Just dbPath

{-
optionGetCacheDirs :: IO (ALPMList a)
optionAddCacheDir :: FilePath -> IO (Maybe Error)
optionSetCacheDirs :: ALPMList a -> IO ()
optionRemoveCacheDir :: String -> IO (Maybe Error)
-}

test_optionGetLogFile :: Assertion
test_optionGetLogFile = alpmTest $ do
    logFile <- optionGetLogFile
    assertBool $ isNothing logFile

test_optionSetLogFile :: Assertion
test_optionSetLogFile = alpmTest $ do
    -- Perhaps this should fail, since /tmp/ is a directory! 
    let logFile = "/tmp/"
    me <- optionSetLogFile logFile

    case me of
        Just e ->
            assertFailure $ "optionSetLogFile: " ++ show e

        Nothing -> do
            mLogFile <- optionGetLogFile
            assertEqual mLogFile $ Just logFile

{-
optionGetLockFile :: IO String
-- /* no set_lockfile, path is determined from dbpath */

optionGetUseSyslog :: IO Bool
optionSetUseSyslog :: Bool -> IO ()

optionGetNoUpgrades :: IO (ALPMList a)
optionAddNoUpgrade :: String -> IO ()
optionSetNoUpgrades :: (ALPMList a) -> IO ()
optionRemoveNoUpgrade :: String -> IO (Maybe Error)

optionGetNoExtracts :: IO (ALPMList a)
optionAddNoExtract :: String -> IO ()
optionSetNoExtracts :: ALPMList a -> IO ()
optionRemoveNoExtract :: String -> IO (Maybe Error)

optionGetIgnorePkgs :: IO (ALPMList a)
optionAddIgnorePkg :: String -> IO ()
optionSetIgnorePkgs :: ALPMList a -> IO ()
optionRemoveIgnorePkg :: String -> IO (Maybe Error)

optionGetIgnoreGrps :: IO (ALPMList a)
optionAddIgnoreGrp :: String -> IO ()
optionSetIgnoreGrps :: (ALPMList a) -> IO ()
optionRemoveIgnoreGrp :: String -> IO (Maybe Error)

optionGetArch :: IO String
optionSetArch :: String -> IO ()

optionGetUseDelta :: IO Bool
optionSetUserDelta :: Bool -> IO ()

optionGetCheckSpace :: IO Bool
optionSetCheckSpace :: Bool -> IO ()
-}
