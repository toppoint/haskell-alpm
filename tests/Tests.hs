{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main (main) where

import Data.Maybe
import System.Exit
import Test.Framework

import Distribution.ArchLinux.ALPM

-- main ----------------------------------------------------------------------

-- main ----------------------------------------------------------------------

main :: IO ()
main = do
    ec <- runTest allHTFTests
    exitWith ec


-- Helper --------------------------------------------------------------------

alpmTest :: ALPM Bool -> Assertion
alpmTest test = do
    r <- alpm test
    case r of
        Left e ->
            assertFailure $ show e

        Right b ->
            assertBool b

filePathIsNothingTest :: ALPM (Maybe FilePath) -> Assertion
filePathIsNothingTest getter = alpmTest $ do
    fp <- getter
    return $ isNothing fp

getAndSetFilePathTest
    :: (FilePath -> ALPM ())
    -> ALPM (Maybe FilePath)
    -> Assertion
getAndSetFilePathTest setter getter = alpmTest $ do
    let fp = "/tmp/"
    setter fp
    mfp <- getter
    return $ mfp == Just fp


-- Tests ---------------------------------------------------------------------

test_initialize_release :: Assertion
test_initialize_release = alpmTest $
    return True

test_getVersion :: Assertion
test_getVersion = alpmTest $ do
    version <- getVersion
    return $ not $ null version

test_optionRootNothing :: Assertion
test_optionRootNothing = filePathIsNothingTest optionGetRoot

test_optionRoot :: Assertion
test_optionRoot = getAndSetFilePathTest optionSetRoot optionGetRoot

test_optionDBPathNothing :: Assertion
test_optionDBPathNothing = filePathIsNothingTest optionGetDBPath

test_optionDBPath :: Assertion
test_optionDBPath = getAndSetFilePathTest optionSetDBPath optionGetDBPath

{-
optionGetCacheDirs :: IO (ALPMList a)
optionAddCacheDir :: FilePath -> IO (Maybe Error)
optionSetCacheDirs :: ALPMList a -> IO ()
optionRemoveCacheDir :: String -> IO (Maybe Error)
-}

test_optionLogFileNothing :: Assertion
test_optionLogFileNothing = filePathIsNothingTest optionGetLogFile

test_optionLogFile :: Assertion
test_optionLogFile = getAndSetFilePathTest optionSetLogFile optionGetLogFile

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
