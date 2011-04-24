{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main (main) where

import Data.Maybe
import System.Exit
import Test.Framework

import Distribution.ArchLinux.ALPM

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

success :: Assertion
success = alpmTest $ return True

stringIsNothingTest :: ALPM (Maybe String) -> Assertion
stringIsNothingTest getter = alpmTest $ do
    str <- getter
    return $ isNothing str

getAndSetStringTest
    :: (String -> ALPM ())
    -> ALPM (Maybe String)
    -> String
    -> Assertion
getAndSetStringTest setter getter str = alpmTest $ do
    setter str
    mstr <- getter
    return $ mstr == Just str

getAndSetFilePathTest
    :: (FilePath -> ALPM ())
    -> ALPM (Maybe FilePath)
    -> Assertion
getAndSetFilePathTest setter getter =
    getAndSetStringTest setter getter "/tmp/"

getStringNotNullTest :: ALPM String -> Assertion
getStringNotNullTest getter = alpmTest $ do
    str <- getter
    return $ not $ null str

getAndSetBool :: (Bool -> ALPM ()) -> ALPM Bool -> Assertion
getAndSetBool setter getter = alpmTest $ do
    setter True
    b1 <- getter
    setter False
    b2 <- getter
    setter True
    b3 <- getter
    return $ b1 && not b2 && b3


-- Tests ---------------------------------------------------------------------

test_initialize_release :: Assertion
test_initialize_release = success

test_getVersion :: Assertion
test_getVersion = getStringNotNullTest getVersion

test_optionRootNothing :: Assertion
test_optionRootNothing = stringIsNothingTest optionGetRoot

test_optionRoot :: Assertion
test_optionRoot = getAndSetFilePathTest optionSetRoot optionGetRoot

test_optionDBPathNothing :: Assertion
test_optionDBPathNothing = stringIsNothingTest optionGetDBPath

test_optionDBPath :: Assertion
test_optionDBPath = getAndSetFilePathTest optionSetDBPath optionGetDBPath

{-
optionGetCacheDirs :: IO (ALPMList a)
optionAddCacheDir :: FilePath -> IO (Maybe Error)
optionSetCacheDirs :: ALPMList a -> IO ()
optionRemoveCacheDir :: String -> IO (Maybe Error)
-}

test_optionLogFileNothing :: Assertion
test_optionLogFileNothing = stringIsNothingTest optionGetLogFile

test_optionLogFile :: Assertion
test_optionLogFile = getAndSetFilePathTest optionSetLogFile optionGetLogFile

test_optionLockFileNothig :: Assertion
test_optionLockFileNothig = stringIsNothingTest optionGetLockFile

test_optionSyslog :: Assertion
test_optionSyslog = getAndSetBool optionSetUseSyslog optionGetUseSyslog

{-
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
-}

test_optionArchNothing :: Assertion
test_optionArchNothing = stringIsNothingTest optionGetArch

test_optionArch :: Assertion
test_optionArch = getAndSetStringTest optionSetArch optionGetArch "i686"

test_optionUseDelta :: Assertion
test_optionUseDelta = getAndSetBool optionSetUserDelta optionGetUseDelta

test_optionCheckSpace :: Assertion
test_optionCheckSpace = getAndSetBool optionSetCheckSpace optionGetCheckSpace

