{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main {- (main) -} where

import Control.Monad.Trans
import Data.Maybe
import System.Exit
import Test.Framework

import Distribution.ArchLinux.ALPM

-- constants -----------------------------------------------------------------

rootPath :: FilePath
rootPath = "/"

dbPath :: FilePath
dbPath = "/var/lib/pacman/"

lockFilePath :: FilePath
lockFilePath = dbPath ++ "db.lck"


-- main ----------------------------------------------------------------------

main :: IO ()
main = do
    ec <- runTest allHTFTests
    exitWith ec


-- Helper --------------------------------------------------------------------

alpmTest :: ALPM Bool -> Assertion
alpmTest test = do
    r <- runALPM rootPath dbPath test
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

stringIsJustTest :: ALPM (Maybe String) -> String -> Assertion
stringIsJustTest getter expected = alpmTest $ do
    str <- getter
    return $ str == Just expected

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

withLocalDatabase :: (Database -> ALPM Bool) -> Assertion
withLocalDatabase action = alpmTest $ do
    db <- optionGetLocalDatabase
    action db


-- Tests ---------------------------------------------------------------------


-- Tests: General ------------------------------------------------------------

test_initialize_release :: Assertion
test_initialize_release = success

test_getVersion :: Assertion
test_getVersion = getStringNotNullTest getVersion


-- Tests: Options ------------------------------------------------------------

test_optionRoot :: Assertion
test_optionRoot = stringIsJustTest optionGetRoot rootPath

-- Test with non Nothing result


test_optionDatabasePath :: Assertion
test_optionDatabasePath = stringIsJustTest optionGetDatabasePath dbPath


test_optionLogFileNothing :: Assertion
test_optionLogFileNothing = stringIsNothingTest optionGetLogFile

test_optionLogFile :: Assertion
test_optionLogFile = getAndSetFilePathTest optionSetLogFile optionGetLogFile


test_optionLockFileNothig :: Assertion
test_optionLockFileNothig = stringIsJustTest optionGetLockFile lockFilePath

-- Better lock file test...


test_optionSyslog :: Assertion
test_optionSyslog = getAndSetBool optionSetUseSyslog optionGetUseSyslog


test_optionArchNothing :: Assertion
test_optionArchNothing = stringIsNothingTest optionGetArchitecture

test_optionArch :: Assertion
test_optionArch =
    getAndSetStringTest optionSetArchitecture optionGetArchitecture "i686"


test_optionUseDelta :: Assertion
test_optionUseDelta = getAndSetBool optionSetUserDelta optionGetUseDelta


test_optionCheckSpace :: Assertion
test_optionCheckSpace = getAndSetBool optionSetCheckSpace optionGetCheckSpace


test_optionLocalDatabase :: Assertion
test_optionLocalDatabase = withLocalDatabase $ \db -> do
    name <- databaseGetName db
    return $ name == "local"


-- Tests: Database -----------------------------------------------------------

test_databaseRegisterSyncNothing :: Assertion
test_databaseRegisterSyncNothing = alpmTest $ do
    mDb <- databaseRegisterSync "" False
    return $ isNothing mDb

test_databaseUnregister :: Assertion
test_databaseUnregister = alpmTest $ do
    mDb <- databaseRegisterSync "NewTestDb" False

    case mDb of
        Nothing ->
            return False

        Just db -> do
            databaseUnregister db
            return True


-- databaseUnregisterAll :: ALPM ()


test_databaseGetName :: Assertion
test_databaseGetName = withLocalDatabase $ \db -> do
    name <- databaseGetName db
    return $ name == "local"


test_databaseGetPackageNothing :: Assertion
test_databaseGetPackageNothing = withLocalDatabase $ \db -> do
    mPkg1 <- databaseGetPackage db ""
    mPkg2 <- databaseGetPackage db "packageNameNotToBeFound"
    return $ (isNothing mPkg1) && (isNothing mPkg2)

test_databaseGetPackageResult :: Assertion
test_databaseGetPackageResult = withLocalDatabase $ \db -> do
    mPkg <- databaseGetPackage db "pacman"
    return $ isJust mPkg


test_databaseReadGroupNothing :: Assertion
test_databaseReadGroupNothing = withLocalDatabase $ \db -> do
    mGroup1 <- databaseReadGroup db ""
    mGroup2 <- databaseReadGroup db "groupNameNotToBeFound"
    return $ (isNothing mGroup1) && (isNothing mGroup2)

test_databaseReadGroupResult :: Assertion
test_databaseReadGroupResult = withLocalDatabase $ \db -> do
    mGroup <- databaseReadGroup db "base"
    return $ isJust mGroup


-- databaseGetGroupCache :: Database -> ALPM [Group]
-- databaseSearch :: Database -> [String] -> ALPM [Package]
-- int alpm_db_set_pkgreason(pmdb_t *db, const char *name, pmpkgreason_t reason);


-- Tests: Package ------------------------------------------------------------

-- /* Info parameters */

-- int alpm_pkg_load(const char *filename, int full, pmpkg_t **pkg);
-- int alpm_pkg_free(pmpkg_t *pkg);
-- int alpm_pkg_checkmd5sum(pmpkg_t *pkg);
-- char *alpm_fetch_pkgurl(const char *url);
-- int alpm_pkg_vercmp(const char *a, const char *b);
-- alpm_list_t *alpm_pkg_compute_requiredby(pmpkg_t *pkg);

-- packageGetFilename :: Package -> ALPM FilePath
-- packageGetName :: Package -> ALPM String
-- packageGetVersion :: Package -> ALPM String
-- packageGetDescription :: Package -> ALPM String
-- packageGetURL :: Package -> ALPM String

-- time_t alpm_pkg_get_builddate(pmpkg_t *pkg);
-- time_t alpm_pkg_get_installdate(pmpkg_t *pkg);

-- packageGetPackager :: Package -> ALPM String
-- packageGetMd5Sum :: Package -> ALPM String
-- packageGetArchitecture :: Package -> ALPM String

-- off_t alpm_pkg_get_size(pmpkg_t *pkg);
-- off_t alpm_pkg_get_isize(pmpkg_t *pkg);
-- pmpkgreason_t alpm_pkg_get_reason(pmpkg_t *pkg);

-- packageGetLicenses :: Package -> ALPM [String]
-- packageGetGroups :: Package -> ALPM [String]
-- packageGetDependencies :: Package -> ALPM [Dependency]
-- packageGetOptionalDependencies :: Package -> ALPM [Dependency]
-- packageGetConflicts :: Package -> ALPM [Conflict]
-- packageGetProvides :: Package -> ALPM [String]
-- packageGetDeltas :: Package -> ALPM [Delta]
-- packageGetReplaces :: Package -> ALPM [String]
-- packageGetFiles :: Package -> ALPM [FilePath]
-- packageGetBackup :: Package -> ALPM [String]
-- packageGetDatabase :: Package -> ALPM Database

-- void *alpm_pkg_changelog_open(pmpkg_t *pkg);
-- size_t alpm_pkg_changelog_read(void *ptr, size_t size,
-- 		const pmpkg_t *pkg, const void *fp);
-- /*int alpm_pkg_changelog_feof(const pmpkg_t *pkg, void *fp);*/
-- int alpm_pkg_changelog_close(const pmpkg_t *pkg, void *fp);
-- int alpm_pkg_has_scriptlet(pmpkg_t *pkg);

-- off_t alpm_pkg_download_size(pmpkg_t *newpkg);

-- packageUnusedDeltas :: Package -> ALPM [Delta]


-- Tests: Delta --------------------------------------------------------------

-- deltaGetFrom :: Delta -> ALPM String
-- deltaGetTo :: Delta -> ALPM String
-- deltaGetFilename :: Delta -> ALPM FilePath
-- deltaGetMD5sum :: Delta -> ALPM String
-- deltaGetSize :: Delta -> ALPM Integer


-- Tests: Group --------------------------------------------------------------

-- groupGetName :: Group -> ALPM String
-- groupGetPackages :: Group -> ALPM [Package]
-- findGroupPackages :: List Database -> String -> ALPM [Group]


-- Tests: Sync ---------------------------------------------------------------

-- pmpkg_t *alpm_sync_newversion(pmpkg_t *pkg, alpm_list_t *dbs_sync);


-- Tests: Transaction --------------------------------------------------------

-- /* Transaction Event callback */
-- typedef void (*alpm_trans_cb_event)(pmtransevt_t, void *, void *);

-- /* Transaction Conversation callback */
-- typedef void (*alpm_trans_cb_conv)(pmtransconv_t, void *, void *, void *, int *);

-- /* Transaction Progress callback */
-- typedef void (*alpm_trans_cb_progress)(pmtransprog_t, const char *, int, size_t, size_t);

-- int alpm_trans_get_flags(void);
-- alpm_list_t * alpm_trans_get_add(void);
-- alpm_list_t * alpm_trans_get_remove(void);
-- int alpm_trans_init(pmtransflag_t flags, alpm_trans_cb_event cb_event, alpm_trans_cb_conv conv, alpm_trans_cb_progress cb_progress);
-- int alpm_trans_prepare(alpm_list_t **data);
-- int alpm_trans_commit(alpm_list_t **data);
-- int alpm_trans_interrupt(void);
-- int alpm_trans_release(void);

-- int alpm_sync_sysupgrade(int enable_downgrade);
-- int alpm_add_pkg(pmpkg_t *pkg);
-- int alpm_remove_pkg(pmpkg_t *pkg);


-- Tests: Dependencies and conflicts -----------------------------------------

-- alpm_list_t *alpm_checkdeps(alpm_list_t *pkglist, int reversedeps, alpm_list_t *remove, alpm_list_t *upgrade);
-- pmpkg_t *alpm_find_satisfier(alpm_list_t *pkgs, const char *depstring);
-- pmpkg_t *alpm_find_dbs_satisfier(alpm_list_t *dbs, const char *depstring);

-- missGetTarget :: DependencyMissing -> ALPM String
-- missGetDependency :: DependencyMissing -> ALPM Dependency
-- missGetCausingPackage :: DependencyMissing -> ALPM String

-- alpm_list_t *alpm_checkconflicts(alpm_list_t *pkglist);

-- conflictGetPackage1 :: Conflict -> ALPM String
-- conflictGetPackage2 :: Conflict -> ALPM String
-- conflictGetReason :: Conflict -> ALPM String

-- pmdepmod_t alpm_dep_get_mod(const pmdepend_t *dep);

-- dependencyGetName :: Dependency -> ALPM String
-- dependencyGetVersion :: Dependency -> ALPM String
-- dependencyComputeString :: Dependency -> ALPM String


-- Tests: File conflicts -----------------------------------------------------

-- fileConflictGetTaget :: FileConflict -> ALPM String
-- fileConflictGetType :: FileConflict -> ALPM FileConflictType
-- fileConflictGetFile :: FileConflict -> ALPM FilePath
-- fileConflictGetConflictingTarget :: FileConflict -> ALPM String

