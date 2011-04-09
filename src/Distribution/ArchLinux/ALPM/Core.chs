{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm" #}

module Distribution.ArchLinux.ALPM.Core
where

#include <alpm.h>

{# import Distribution.ArchLinux.ALPM.Types #}
{# import Distribution.ArchLinux.ALPM.List #}

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Utils (toBool, fromBool) 

import Control.Monad (liftM)


-- General ------------------------------------------------------------------

-- | This function needs to be called first or nothing else will work.
initialize :: IO Error
initialize =
    liftM (toEnum . fromIntegral) $
        {# call initialize #}

-- | Call this function to clean up. After this the library is no longer
-- available
release :: IO Error
release =
    liftM (toEnum . fromIntegral) $
        {# call release #}

-- | Get ALPM's version.
getVersion :: IO String
getVersion =
    {# call version #}
        >>= peekCString

checkForNull :: (a -> Ptr a) -> a -> Maybe a
checkForNull unObj obj
  | unObj obj == nullPtr = Nothing
  | otherwise            = Just obj


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

optionGetRoot :: IO FilePath
optionGetRoot = 
  {# call option_get_root #} >>= peekCString

optionSetRoot :: FilePath -> IO Error
optionSetRoot fp = 
  liftM (toEnum . fromIntegral) $ 
    {# call option_set_root #} =<< newCString fp

optionGetDBPath :: IO FilePath
optionGetDBPath = 
  {# call option_get_dbpath #} >>= peekCString

optionSetDBPath :: FilePath -> IO Error
optionSetDBPath fp = 
  liftM (toEnum . fromIntegral) $ 
    {# call option_set_dbpath #} =<< newCString fp

optionGetCacheDirs :: IO ALPMList
optionGetCacheDirs = 
  {# call option_get_cachedirs #}

optionAddCacheDir :: FilePath -> IO Error
optionAddCacheDir fp = 
  liftM (toEnum . fromIntegral) $ 
    {# call option_add_cachedir #} =<< newCString fp

optionSetCacheDirs :: ALPMList -> IO ()
optionSetCacheDirs list = 
  {# call option_set_cachedirs #} $ list

optionRemoveCacheDir :: String -> IO Error
optionRemoveCacheDir str = 
  liftM (toEnum . fromIntegral) $ 
    {# call option_remove_cachedir #} =<< newCString str 

optionGetLogFile :: IO String
optionGetLogFile = 
  {# call option_get_logfile #} >>= peekCString

optionSetLogFile :: String -> IO Error
optionSetLogFile str = 
  liftM (toEnum . fromIntegral) $ 
    {# call option_set_logfile #} =<< newCString str

optionGetLockFile :: IO String
optionGetLockFile = 
  {# call option_get_lockfile #} >>= peekCString

-- /* no set_lockfile, path is determined from dbpath */

optionGetUseSyslog :: IO Bool
optionGetUseSyslog = 
  liftM toBool $ {# call option_get_usesyslog #}

optionSetUseSyslog :: Bool -> IO ()
optionSetUseSyslog b = 
  {# call option_set_usesyslog #} $ fromBool b 

optionGetNoUpgrades :: IO ALPMList
optionGetNoUpgrades = 
  {# call option_get_noupgrades #}

optionAddNoUpgrade :: String -> IO ()
optionAddNoUpgrade pkg = 
    {# call option_add_noupgrade #} =<< newCString pkg

optionSetNoUpgrades :: ALPMList -> IO ()
optionSetNoUpgrades list = 
  {# call option_set_noupgrades #} $ list

optionRemoveNoUpgrade :: String -> IO Error
optionRemoveNoUpgrade str = 
  liftM (toEnum . fromIntegral) $ 
    {# call option_remove_noupgrade #} =<< newCString str 

optionGetNoExtracts :: IO ALPMList
optionGetNoExtracts = 
  {# call option_get_noextracts #}

optionAddNoExtract :: String -> IO ()
optionAddNoExtract pkg = 
    {# call option_add_noextract #} =<< newCString pkg

optionSetNoExtracts :: ALPMList -> IO ()
optionSetNoExtracts list = 
  {# call option_set_noextracts #} $ list

optionRemoveNoExtract :: String -> IO Error
optionRemoveNoExtract str = 
  liftM (toEnum . fromIntegral) $ 
    {# call option_remove_noextract #} =<< newCString str 

optionGetIgnorePkgs :: IO ALPMList
optionGetIgnorePkgs = 
  {# call option_get_ignorepkgs #}

optionAddIgnorePkg :: String -> IO ()
optionAddIgnorePkg pkg = 
    {# call option_add_ignorepkg #} =<< newCString pkg

optionSetIgnorePkgs :: ALPMList -> IO ()
optionSetIgnorePkgs list = 
  {# call option_set_ignorepkgs #} $ list

optionRemoveIgnorePkg :: String -> IO Error
optionRemoveIgnorePkg str = 
  liftM (toEnum . fromIntegral) $ 
    {# call option_remove_ignorepkg #} =<< newCString str 

optionGetIgnoreGrps :: IO ALPMList
optionGetIgnoreGrps = 
  {# call option_get_ignoregrps #}

optionAddIgnoreGrp :: String -> IO ()
optionAddIgnoreGrp pkg = 
    {# call option_add_ignoregrp #} =<< newCString pkg

optionSetIgnoreGrps :: ALPMList -> IO ()
optionSetIgnoreGrps list = 
  {# call option_set_ignoregrps #} $ list

optionRemoveIgnoreGrp :: String -> IO Error
optionRemoveIgnoreGrp str = 
  liftM (toEnum . fromIntegral) $ 
    {# call option_remove_ignoregrp #} =<< newCString str 

optionGetArch :: IO String
optionGetArch =
    {# call option_get_arch #}
        >>= peekCString

optionSetArch :: String -> IO ()
optionSetArch arch =
    withCString arch $ {# call option_set_arch #}

optionGetUseDelta :: IO Bool
optionGetUseDelta =
    liftM toBool $ {# call option_get_usedelta #}

optionSetUserDelta :: Bool -> IO ()
optionSetUserDelta b =
    {# call option_set_usedelta #} $ fromBool b

optionGetCheckSpace :: IO Bool
optionGetCheckSpace =
    liftM toBool $ {# call option_get_checkspace #}

optionSetCheckSpace :: Bool -> IO ()
optionSetCheckSpace b =
    {# call option_set_checkspace #} $ fromBool b

-- pmdb_t *alpm_option_get_localdb(void);
-- alpm_list_t *alpm_option_get_syncdbs(void);


-- Database ------------------------------------------------------------------

databaseRegisterSync :: String -> IO (Maybe Database)
databaseRegisterSync treeName = do
    db <- withCString treeName $ {# call db_register_sync #}
    return $ checkForNull unDatabase db

databaseUnregister :: Database -> IO Error
databaseUnregister db =
    liftM (toEnum . fromIntegral) $
        {# call db_unregister #} db

databaseUnregisterAll :: IO Error
databaseUnregisterAll =
    liftM (toEnum . fromIntegral) $
        {# call db_unregister_all #}

databaseGetName :: Database -> IO String
databaseGetName db =
    {# call alpm_db_get_name #} db
        >>= peekCString

databaseGetUrl :: Database -> IO String
databaseGetUrl db =
    {# call alpm_db_get_url #} db
        >>= peekCString

databaseSetServer :: Database -> String -> IO Error
databaseSetServer db url =
    liftM (toEnum . fromIntegral) $
        withCString url $ {# call db_setserver #} db

databaseUpdate :: Database -> LogLevel -> IO Error
databaseUpdate db level =
    liftM (toEnum . fromIntegral) $
        {# call db_update #} (fromIntegral $ fromEnum level) db

-- pmpkg_t *alpm_db_get_pkg(pmdb_t *db, const char *name);
-- alpm_list_t *alpm_db_get_pkgcache(pmdb_t *db);

dbGetPKGCache :: Database -> IO ALPMList
dbGetPKGCache db = 
  {# call db_get_pkgcache #} db 

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

missGetTarget :: DependencyMissing -> IO String
missGetTarget miss =
    {# call miss_get_target #} miss
        >>= peekCString

missGetDependency :: DependencyMissing -> IO Dependency
missGetDependency =
    {# call miss_get_dep #}

missGetCausingPackage :: DependencyMissing -> IO String
missGetCausingPackage miss =
    {# call miss_get_causingpkg #} miss
        >>= peekCString

-- alpm_list_t *alpm_checkconflicts(alpm_list_t *pkglist);

conflictGetPackage1 :: Conflict -> IO String
conflictGetPackage1 conflict =
    {# call conflict_get_package1 #} conflict
        >>= peekCString

conflictGetPackage2 :: Conflict -> IO String
conflictGetPackage2 conflict =
    {# call conflict_get_package2 #} conflict
        >>= peekCString

conflictGetReason :: Conflict -> IO String
conflictGetReason conflict =
    {# call conflict_get_reason #} conflict
        >>= peekCString

-- pmdepmod_t alpm_dep_get_mod(const pmdepend_t *dep);

dependencyGetName :: Dependency -> IO String
dependencyGetName dependency =
    {# call dep_get_name #} dependency
        >>= peekCString

dependencyGetVersion :: Dependency -> IO String
dependencyGetVersion dependency =
    {# call dep_get_version #} dependency
        >>= peekCString

dependencyComputeString :: Dependency -> IO String
dependencyComputeString dependency =
    {# call dep_compute_string #} dependency
        >>= peekCString


-- File conflicts ------------------------------------------------------------

-- const char *alpm_fileconflict_get_target(pmfileconflict_t *conflict);
-- pmfileconflicttype_t alpm_fileconflict_get_type(pmfileconflict_t *conflict);
-- const char *alpm_fileconflict_get_file(pmfileconflict_t *conflict);
-- const char *alpm_fileconflict_get_ctarget(pmfileconflict_t *conflict);


-- Helpers -------------------------------------------------------------------

computeMd5Sum :: String -> IO String
computeMd5Sum name =
    withCString name {# call compute_md5sum #}
        >>= peekCString

strError :: Error -> IO String
strError err =
    {# call strerror #} (fromIntegral $ fromEnum err)
        >>= peekCString

strErrorLast :: IO String
strErrorLast =
    {# call strerrorlast #}
        >>= peekCString

