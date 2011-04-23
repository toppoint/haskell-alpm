{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main (main) where

import Data.Maybe
import System.Exit
import Test.Framework

import Distribution.ArchLinux.ALPM

import Util

main :: IO ()
main = do
    ec <- runTest allHTFTests
    exitWith ec

alpmTest :: ALPMTest Bool -> Assertion
alpmTest test = do
    r <- alpm test
    case r of
        Left e ->
            assertFailure $ show e

        Right b ->
            assertBool b

test_initialize_release :: Assertion
test_initialize_release = alpmTest $
    return True

test_getVersion :: Assertion
test_getVersion = alpmTest $ do
    version <- getVersion
    return $ not $ null version

test_optionGetRoot :: Assertion
test_optionGetRoot = alpmTest $ do
    root <- optionGetRoot
    return $ isNothing root

test_optionSetRoot :: Assertion
test_optionSetRoot = alpmTest $ do
    let root = "/tmp/"
    optionSetRoot root
    mRoot <- optionGetRoot
    return $ mRoot == Just root

test_optionGetDBPath :: Assertion
test_optionGetDBPath = alpmTest $ do
    dbPath <- optionGetDBPath
    return $ isNothing dbPath

test_optionSetDBPath :: Assertion
test_optionSetDBPath = alpmTest $ do
    let dbPath = "/tmp/"
    optionSetDBPath dbPath
    mDBPath <- optionGetDBPath
    return $ mDBPath == Just dbPath

{-
optionGetCacheDirs :: IO (ALPMList a)
optionAddCacheDir :: FilePath -> IO (Maybe Error)
optionSetCacheDirs :: ALPMList a -> IO ()
optionRemoveCacheDir :: String -> IO (Maybe Error)
-}

test_optionGetLogFile :: Assertion
test_optionGetLogFile = alpmTest $ do
    logFile <- optionGetLogFile
    return $ isNothing logFile

test_optionSetLogFile :: Assertion
test_optionSetLogFile = alpmTest $ do
    -- Perhaps this should fail, since /tmp/ is a directory! 
    let logFile = "/tmp/"
    optionSetLogFile logFile
    mLogFile <- optionGetLogFile
    return $ mLogFile == Just logFile

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
