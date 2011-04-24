{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm" #}

module Distribution.ArchLinux.ALPM.Internal.Core where

#include <alpm.h>

{# import Distribution.ArchLinux.ALPM.Internal.List #}
{# import Distribution.ArchLinux.ALPM.Internal.Monad #}
{# import Distribution.ArchLinux.ALPM.Internal.Types #}

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Utils (toBool, fromBool, maybePeek)

import Control.Monad
import Control.Monad.Trans


-- Helpers ------------------------------------------------------------------

getBool :: MonadALPM m => IO CInt -> m Bool
getBool getter = liftIO $ liftM toBool getter

setBool :: MonadALPM m => (CInt -> IO ()) -> Bool -> m ()
setBool setter = liftIO . setter . fromBool

getString :: MonadALPM m => IO CString -> m (Maybe String)
getString getter = liftIO $ getter >>= maybePeek peekCString

setString :: MonadALPM m => (CString -> IO CInt) -> String -> m ()
setString setter str =
    withErrorHandling $ newCString str >>= setter

setString_ :: MonadALPM m => (CString -> IO ()) -> String -> m ()
setString_ setter str = liftIO $ newCString str >>= setter

setList :: MonadALPM m => (Ptr a -> IO ()) -> List b -> m ()
setList setter = liftIO . setter . castPtr . unList

getList :: MonadALPM m => IO (Ptr a) -> m (List b)
getList getter = liftIO $ liftM (List . castPtr) $ getter

valueToString :: MonadALPM m => (a -> IO CString) -> a -> m String
valueToString converter value = liftIO $ converter value >>= peekCString

valueToList :: MonadALPM m => (a -> IO (Ptr b)) -> a -> m (List c)
valueToList converter value =
    liftIO $ liftM (List . castPtr) $ converter value

setEnum :: (Enum e, MonadALPM m) => (CInt -> IO CInt) -> e -> m ()
setEnum setter enum =
    withErrorHandling $ setter (fromIntegral $ fromEnum enum)

withErrorHandling :: MonadALPM m => IO CInt -> m ()
withErrorHandling action =
    liftIO action >>= handleError

checkForNull :: (a -> Ptr a) -> a -> Maybe a
checkForNull unObj obj
  | unObj obj == nullPtr = Nothing
  | otherwise            = Just obj

maybeToEnum :: Enum a => Int -> Maybe a
maybeToEnum n
  | n == 0    = Nothing
  | otherwise = Just $ toEnum n


-- General ------------------------------------------------------------------

-- | Get ALPM's version.
getVersion :: MonadALPM m => m String
getVersion = liftIO $
    {# call version #} >>= peekCString


-- Logging -------------------------------------------------------------------

-- typedef void (*alpm_cb_log)(pmloglevel_t, const char *, va_list);
-- int alpm_logaction(const char *fmt, ...);


-- Downloading ---------------------------------------------------------------

-- typedef void (*alpm_cb_download)(const char *filename, off_t xfered, off_t total);
-- typedef void (*alpm_cb_totaldl)(off_t total);

{-
/** A callback for downloading files
 * @param url the URL of the file to be downloaded
 * @param localpath the directory to which the file should be downloaded
 * @param force whether to force an update, even if the file is the same
 * @return 0 on success, 1 if the file exists and is identical, -1 on
 * error.
 */
-}
-- typedef int (*alpm_cb_fetch)(const char *url, const char *localpath, int force);


-- Options -------------------------------------------------------------------

-- alpm_cb_log alpm_option_get_logcb(void);
-- void alpm_option_set_logcb(alpm_cb_log cb);

-- alpm_cb_download alpm_option_get_dlcb(void);
-- void alpm_option_set_dlcb(alpm_cb_download cb);

-- alpm_cb_fetch alpm_option_get_fetchcb(void);
-- void alpm_option_set_fetchcb(alpm_cb_fetch cb);

-- alpm_cb_totaldl alpm_option_get_totaldlcb(void);
-- void alpm_option_set_totaldlcb(alpm_cb_totaldl cb);

optionGetRoot :: MonadALPM m => m (Maybe FilePath)
optionGetRoot = getString {# call option_get_root #}

optionSetRoot :: MonadALPM m => FilePath -> m ()
optionSetRoot = setString {# call option_set_root #}

optionGetDBPath :: MonadALPM m => m (Maybe FilePath)
optionGetDBPath = getString {# call option_get_dbpath #}

optionSetDBPath :: MonadALPM m => FilePath -> m ()
optionSetDBPath = setString {# call option_set_dbpath #}

optionGetCacheDirs :: MonadALPM m => m (List a)
optionGetCacheDirs = liftIO $
  liftM (List . castPtr) $ {# call option_get_cachedirs #}

optionAddCacheDir :: MonadALPM m => FilePath -> m ()
optionAddCacheDir = setString {# call option_add_cachedir #}

optionSetCacheDirs :: MonadALPM m => List a -> m ()
optionSetCacheDirs = setList {# call option_set_cachedirs #}

optionRemoveCacheDir :: MonadALPM m => FilePath -> m ()
optionRemoveCacheDir = setString {# call option_remove_cachedir #}

optionGetLogFile :: MonadALPM m => m (Maybe FilePath)
optionGetLogFile = getString {# call option_get_logfile #}

optionSetLogFile :: MonadALPM m => FilePath -> m ()
optionSetLogFile = setString {# call option_set_logfile #}

optionGetLockFile :: MonadALPM m => m (Maybe FilePath)
optionGetLockFile = getString {# call option_get_lockfile #}

-- /* no set_lockfile, path is determined from dbpath */

optionGetUseSyslog :: MonadALPM m => m Bool
optionGetUseSyslog = getBool {# call option_get_usesyslog #}

optionSetUseSyslog :: MonadALPM m => Bool -> m ()
optionSetUseSyslog = setBool {# call option_set_usesyslog #}

optionGetNoUpgrades :: MonadALPM m => m (List a)
optionGetNoUpgrades = getList {# call option_get_noupgrades #}

optionAddNoUpgrade :: MonadALPM m => String -> m ()
optionAddNoUpgrade = setString_ {# call option_add_noupgrade #}

optionSetNoUpgrades :: MonadALPM m => (List a) -> m ()
optionSetNoUpgrades = setList {# call option_set_noupgrades #}

optionRemoveNoUpgrade :: MonadALPM m => String -> m ()
optionRemoveNoUpgrade = setString {# call option_remove_noupgrade #}

optionGetNoExtracts :: MonadALPM m => m (List a)
optionGetNoExtracts = getList {# call option_get_noextracts #}

optionAddNoExtract :: MonadALPM m => String -> m ()
optionAddNoExtract = setString_ {# call option_add_noextract #}

optionSetNoExtracts :: MonadALPM m => List a -> m ()
optionSetNoExtracts = setList {# call option_set_noextracts #}

optionRemoveNoExtract :: MonadALPM m => String -> m ()
optionRemoveNoExtract = setString {# call option_remove_noextract #}

optionGetIgnorePkgs :: MonadALPM m => m (List a)
optionGetIgnorePkgs = getList {# call option_get_ignorepkgs #}

optionAddIgnorePkg :: MonadALPM m => String -> m ()
optionAddIgnorePkg = setString_ {# call option_add_ignorepkg #}

optionSetIgnorePkgs :: MonadALPM m => List a -> m ()
optionSetIgnorePkgs = setList {# call option_set_ignorepkgs #}

optionRemoveIgnorePkg :: MonadALPM m => String -> m ()
optionRemoveIgnorePkg = setString {# call option_remove_ignorepkg #}

optionGetIgnoreGrps :: MonadALPM m => m (List a)
optionGetIgnoreGrps = getList {# call option_get_ignoregrps #}

optionAddIgnoreGrp :: MonadALPM m => String -> m ()
optionAddIgnoreGrp = setString_ {# call option_add_ignoregrp #}

optionSetIgnoreGrps :: MonadALPM m => (List a) -> m ()
optionSetIgnoreGrps = setList {# call option_set_ignoregrps #}

optionRemoveIgnoreGrp :: MonadALPM m => String -> m ()
optionRemoveIgnoreGrp = setString {# call option_remove_ignoregrp #}

optionGetArch :: MonadALPM m => m (Maybe String)
optionGetArch = getString {# call option_get_arch #}

optionSetArch :: MonadALPM m => String -> m ()
optionSetArch = setString_ {# call option_set_arch #}

optionGetUseDelta :: MonadALPM m => m Bool
optionGetUseDelta = getBool {# call option_get_usedelta #}

optionSetUserDelta :: MonadALPM m => Bool -> m ()
optionSetUserDelta = setBool {# call option_set_usedelta #}

optionGetCheckSpace :: MonadALPM m => m Bool
optionGetCheckSpace = getBool {# call option_get_checkspace #}

optionSetCheckSpace :: MonadALPM m => Bool -> m ()
optionSetCheckSpace = setBool {# call option_set_checkspace #}

-- db.h
-- pmdb_t *alpm_option_get_localdb(void);
-- alpm_list_t *alpm_option_get_syncdbs(void);


-- Database ------------------------------------------------------------------

databaseRegisterSync :: MonadALPM m => String -> m (Maybe Database)
databaseRegisterSync treeName = liftIO $ do
    db <- withCString treeName $ {# call db_register_sync #}
    return $ checkForNull unpack db

databaseUnregister :: MonadALPM m => Database -> m ()
databaseUnregister db = withErrorHandling $ {# call db_unregister #} db

databaseUnregisterAll :: MonadALPM m => m ()
databaseUnregisterAll = withErrorHandling {# call db_unregister_all #}

databaseGetName :: MonadALPM m => Database -> m String
databaseGetName = valueToString {# call alpm_db_get_name #}

databaseGetUrl :: MonadALPM m => Database -> m String
databaseGetUrl = valueToString {# call alpm_db_get_url #}

databaseSetServer :: MonadALPM m => Database -> String -> m ()
databaseSetServer db = setString $ {# call db_setserver #} db

databaseUpdate :: MonadALPM m => Database -> LogLevel -> m ()
databaseUpdate db = setEnum (flip {# call db_update #} db)

-- pmpkg_t *alpm_db_get_pkg(pmdb_t *db, const char *name);
-- alpm_list_t *alpm_db_get_pkgcache(pmdb_t *db);

dbGetPKGCache :: MonadALPM m => Database -> m (List Package)
dbGetPKGCache = valueToList {# call db_get_pkgcache #}

-- pmgrp_t *alpm_db_readgrp(pmdb_t *db, const char *name);
-- alpm_list_t *alpm_db_get_grpcache(pmdb_t *db);
-- alpm_list_t *alpm_db_search(pmdb_t *db, const alpm_list_t* needles);
-- int alpm_db_set_pkgreason(pmdb_t *db, const char *name, pmpkgreason_t reason);


-- Package -------------------------------------------------------------------

-- /* Info parameters */

-- int alpm_pkg_load(const char *filename, int full, pmpkg_t **pkg);
-- int alpm_pkg_free(pmpkg_t *pkg);
-- int alpm_pkg_checkmd5sum(pmpkg_t *pkg);
-- char *alpm_fetch_pkgurl(const char *url);
-- int alpm_pkg_vercmp(const char *a, const char *b);
-- alpm_list_t *alpm_pkg_compute_requiredby(pmpkg_t *pkg);

-- const char *alpm_pkg_get_filename(pmpkg_t *pkg);
-- const char *alpm_pkg_get_name(pmpkg_t *pkg);
-- const char *alpm_pkg_get_version(pmpkg_t *pkg);
-- const char *alpm_pkg_get_desc(pmpkg_t *pkg);
-- const char *alpm_pkg_get_url(pmpkg_t *pkg);
-- time_t alpm_pkg_get_builddate(pmpkg_t *pkg);
-- time_t alpm_pkg_get_installdate(pmpkg_t *pkg);
-- const char *alpm_pkg_get_packager(pmpkg_t *pkg);
-- const char *alpm_pkg_get_md5sum(pmpkg_t *pkg);
-- const char *alpm_pkg_get_arch(pmpkg_t *pkg);
-- off_t alpm_pkg_get_size(pmpkg_t *pkg);
-- off_t alpm_pkg_get_isize(pmpkg_t *pkg);
-- pmpkgreason_t alpm_pkg_get_reason(pmpkg_t *pkg);
-- alpm_list_t *alpm_pkg_get_licenses(pmpkg_t *pkg);
-- alpm_list_t *alpm_pkg_get_groups(pmpkg_t *pkg);
-- alpm_list_t *alpm_pkg_get_depends(pmpkg_t *pkg);
-- alpm_list_t *alpm_pkg_get_optdepends(pmpkg_t *pkg);
-- alpm_list_t *alpm_pkg_get_conflicts(pmpkg_t *pkg);
-- alpm_list_t *alpm_pkg_get_provides(pmpkg_t *pkg);
-- alpm_list_t *alpm_pkg_get_deltas(pmpkg_t *pkg);
-- alpm_list_t *alpm_pkg_get_replaces(pmpkg_t *pkg);
-- alpm_list_t *alpm_pkg_get_files(pmpkg_t *pkg);
-- alpm_list_t *alpm_pkg_get_backup(pmpkg_t *pkg);
-- pmdb_t *alpm_pkg_get_db(pmpkg_t *pkg);
-- void *alpm_pkg_changelog_open(pmpkg_t *pkg);
-- size_t alpm_pkg_changelog_read(void *ptr, size_t size,
-- 		const pmpkg_t *pkg, const void *fp);
-- /*int alpm_pkg_changelog_feof(const pmpkg_t *pkg, void *fp);*/
-- int alpm_pkg_changelog_close(const pmpkg_t *pkg, void *fp);
-- int alpm_pkg_has_scriptlet(pmpkg_t *pkg);

-- off_t alpm_pkg_download_size(pmpkg_t *newpkg);
-- alpm_list_t *alpm_pkg_unused_deltas(pmpkg_t *pkg);


-- Delta ---------------------------------------------------------------------

-- const char *alpm_delta_get_from(pmdelta_t *delta);
-- const char *alpm_delta_get_to(pmdelta_t *delta);
-- const char *alpm_delta_get_filename(pmdelta_t *delta);
-- const char *alpm_delta_get_md5sum(pmdelta_t *delta);
-- off_t alpm_delta_get_size(pmdelta_t *delta);


-- Group ---------------------------------------------------------------------

-- const char *alpm_grp_get_name(const pmgrp_t *grp);
-- alpm_list_t *alpm_grp_get_pkgs(const pmgrp_t *grp);
-- alpm_list_t *alpm_find_grp_pkgs(alpm_list_t *dbs, const char *name);


-- Sync ----------------------------------------------------------------------

-- pmpkg_t *alpm_sync_newversion(pmpkg_t *pkg, alpm_list_t *dbs_sync);


-- Transaction ---------------------------------------------------------------

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


-- Dependencies and conflicts ------------------------------------------------

-- alpm_list_t *alpm_checkdeps(alpm_list_t *pkglist, int reversedeps, alpm_list_t *remove, alpm_list_t *upgrade);
-- pmpkg_t *alpm_find_satisfier(alpm_list_t *pkgs, const char *depstring);
-- pmpkg_t *alpm_find_dbs_satisfier(alpm_list_t *dbs, const char *depstring);

missGetTarget :: MonadALPM m => DependencyMissing -> m String
missGetTarget = valueToString {# call miss_get_target #}

missGetDependency :: MonadALPM m => DependencyMissing -> m Dependency
missGetDependency = liftIO . {# call miss_get_dep #}

missGetCausingPackage :: MonadALPM m => DependencyMissing -> m String
missGetCausingPackage = valueToString {# call miss_get_causingpkg #}

-- alpm_list_t *alpm_checkconflicts(alpm_list_t *pkglist);

conflictGetPackage1 :: MonadALPM m => Conflict -> m String
conflictGetPackage1 = valueToString {# call conflict_get_package1 #}

conflictGetPackage2 :: MonadALPM m => Conflict -> m String
conflictGetPackage2 = valueToString {# call conflict_get_package2 #}

conflictGetReason :: MonadALPM m => Conflict -> m String
conflictGetReason = valueToString {# call conflict_get_reason #}

-- pmdepmod_t alpm_dep_get_mod(const pmdepend_t *dep);

dependencyGetName :: MonadALPM m => Dependency -> m String
dependencyGetName = valueToString {# call dep_get_name #}

dependencyGetVersion :: MonadALPM m => Dependency -> m String
dependencyGetVersion = valueToString {# call dep_get_version #}

dependencyComputeString :: MonadALPM m => Dependency -> m String
dependencyComputeString = valueToString {# call dep_compute_string #}


-- File conflicts ------------------------------------------------------------

-- const char *alpm_fileconflict_get_target(pmfileconflict_t *conflict);
-- pmfileconflicttype_t alpm_fileconflict_get_type(pmfileconflict_t *conflict);
-- const char *alpm_fileconflict_get_file(pmfileconflict_t *conflict);
-- const char *alpm_fileconflict_get_ctarget(pmfileconflict_t *conflict);


-- Helpers -------------------------------------------------------------------

computeMd5Sum :: MonadALPM m => String -> m String
computeMd5Sum name = liftIO $
    withCString name {# call compute_md5sum #} >>= peekCString

strError :: MonadALPM m => Error -> m String
strError err = liftIO $
    {# call strerror #} (fromIntegral $ fromEnum err) >>= peekCString

strErrorLast :: MonadALPM m => m (Maybe String)
strErrorLast = getString {# call strerrorlast #}

