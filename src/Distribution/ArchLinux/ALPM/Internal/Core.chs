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

-- Exportable Types ---------------------------------------------------------

type CallbackDownload = String -> Int -> Int -> IO ()

-- Helpers ------------------------------------------------------------------

getBool :: IO CInt -> ALPM Bool
getBool getter = liftIO $ liftM toBool getter

setBool :: (CInt -> IO CInt) -> Bool -> ALPM ()
setBool setter = withErrorHandling . setter . fromBool

setBool_ :: (CInt -> IO a) -> Bool -> ALPM a
setBool_ setter = liftIO . setter . fromBool

getString :: IO CString -> ALPM (Maybe String)
getString getter = liftIO $ getter >>= maybePeek peekCString

setString :: (CString -> IO CInt) -> String -> ALPM ()
setString setter str =
    withErrorHandling $ newCString str >>= setter

setString_ :: (CString -> IO ()) -> String -> ALPM ()
setString_ setter str = liftIO $ newCString str >>= setter

setList :: ALPMType b => (Ptr a -> IO CInt) -> [b] -> ALPM ()
setList setter lst = withErrorHandling $ (setter . castPtr . unList) =<< toList lst 

setList_ :: ALPMType b => (Ptr a -> IO c) -> [b] -> ALPM c
setList_ setter lst = liftIO $ (setter . castPtr . unList) =<< toList lst 

valueToString :: IO CString -> ALPM String
valueToString = liftIO . (=<<) peekCString

valueToList :: ALPMType b => IO (Ptr a) -> ALPM (List b)
valueToList = liftIO . liftM (List . castPtr)

valueToListALPM :: ALPMType b => ALPM (Ptr a) -> ALPM (List b)
valueToListALPM = liftM (List . castPtr)

valueToInt :: IO CInt -> ALPM Int
valueToInt = liftIO . liftM fromIntegral

valueToInteger :: IO CLong -> ALPM Integer
valueToInteger = liftIO . liftM fromIntegral

setEnum :: Enum e => (CInt -> IO CInt) -> e -> ALPM ()
setEnum setter enum =
    withErrorHandling $ setter (fromIntegral $ fromEnum enum)

valueToEnum :: Enum e => IO CInt -> ALPM e
valueToEnum converter = liftIO $ return . toEnum . fromIntegral =<< converter

stringToMaybeValue
    :: ALPMType v
    => (CString -> IO v)
    -> String
    -> ALPM (Maybe v)
stringToMaybeValue action str = liftIO $ do
    value <- withCString str action 
    checkForNull unpack value

withErrorHandling :: IO CInt -> ALPM ()
withErrorHandling action =
    liftIO action >>= handleError

checkForNull :: (a -> IO (Ptr a)) -> a -> IO (Maybe a)
checkForNull unObj obj = do 
  result <- unObj obj
  if result == nullPtr 
    then return Nothing
    else return $ Just obj

maybeToEnum :: Enum a => Int -> Maybe a
maybeToEnum n
  | n == 0    = Nothing
  | otherwise = Just $ toEnum n

-- Callback Handlers -------------------------------------------------------

foreign import ccall "wrapper"
  _wrap_cb_download :: (CString -> CLong -> CLong -> IO ()) 
                   -> IO (FunPtr (CString -> CLong -> CLong -> IO ()))

wrap_cb_download :: CallbackDownload 
                 -> IO (FunPtr (CString -> CLong -> CLong -> IO ()))
wrap_cb_download f = _wrap_cb_download $ \ cstr cl1 cl2 -> do
  str <- peekCString cstr
  let l1 = fromIntegral cl1
      l2 = fromIntegral cl2
  f str l1 l2

-- General ------------------------------------------------------------------

-- | Get ALPM's version.
getVersion :: ALPM String
getVersion = liftIO $
    {# call version #} >>= peekCString


-- Logging -------------------------------------------------------------------

-- typedef void (*alpm_cb_log)(pmloglevel_t, const char *, va_list);
-- int alpm_logaction(const char *fmt, ...);


-- Downloading ---------------------------------------------------------------

-- typedef void (*alpm_cb_totaldl)(off_t total);

-- | Returns the callback used to report download progress.
-- alpm_cb_download alpm_option_get_dlcb(alpm_handle_t *handle);                   
-- optionGetDownloadCallback :: Handle -> ALPM CallbackDownload

-- | Sets the callback used to report download progress.
optionSetDownloadCallback :: CallbackDownload -> ALPM ()
optionSetDownloadCallback dlcb = do
  hdl <- getHandle 
  withErrorHandling $ do
    clbk <- wrap_cb_download dlcb
    {# call alpm_option_set_dlcb #} hdl clbk 


-- Options -------------------------------------------------------------------

-- alpm_cb_log alpm_option_get_logcb(void);
-- void alpm_option_set_logcb(alpm_cb_log cb);

-- alpm_cb_download alpm_option_get_dlcb(void);
-- void alpm_option_set_dlcb(alpm_cb_download cb);

-- alpm_cb_fetch alpm_option_get_fetchcb(void);
-- void alpm_option_set_fetchcb(alpm_cb_fetch cb);

-- alpm_cb_totaldl alpm_option_get_totaldlcb(void);
-- void alpm_option_set_totaldlcb(alpm_cb_totaldl cb);

optionGetRoot :: ALPM (Maybe FilePath)
optionGetRoot = getString . {# call option_get_root #} =<< getHandle

optionGetDatabasePath :: ALPM (Maybe FilePath)
optionGetDatabasePath = getString . {# call option_get_dbpath #} =<< getHandle

optionGetCacheDirs :: Handle -> ALPM (List String)
optionGetCacheDirs = valueToList . {# call option_get_cachedirs #} 

optionAddCacheDir :: FilePath -> ALPM ()
optionAddCacheDir fp = do 
  hdl <- getHandle 
  setString ({# call option_add_cachedir #} hdl) fp

optionSetCacheDirs :: [FilePath] -> ALPM ()
optionSetCacheDirs fpl =  do
  hdl <- getHandle 
  setList ({# call option_set_cachedirs #} hdl) fpl

optionRemoveCacheDir ::FilePath -> ALPM ()
optionRemoveCacheDir fp = do 
  hdl <- getHandle
  setString ({# call option_remove_cachedir #} hdl) fp

optionGetLogFile :: ALPM (Maybe FilePath)
optionGetLogFile = do
  hdl <- getHandle
  getString ({# call option_get_logfile #} hdl)

optionSetLogFile :: FilePath -> ALPM ()
optionSetLogFile fp = do
  hdl <- getHandle
  setString ({# call option_set_logfile #} hdl) fp

optionGetLockFile :: ALPM (Maybe FilePath)
optionGetLockFile = do 
  hdl <- getHandle
  getString ({# call option_get_lockfile #} hdl)

-- /* no set_lockfile, path is determined from dbpath */

optionGetUseSyslog :: Handle -> ALPM Bool
optionGetUseSyslog = getBool . {# call option_get_usesyslog #}

optionSetUseSyslog :: Handle -> Bool -> ALPM ()
optionSetUseSyslog = setBool . {# call option_set_usesyslog #}

optionGetNoUpgrades :: Handle -> ALPM (List String)
optionGetNoUpgrades = valueToList . {# call option_get_noupgrades #}

optionAddNoUpgrade :: Handle -> String -> ALPM ()
optionAddNoUpgrade = setString . {# call option_add_noupgrade #}

optionSetNoUpgrades :: Handle -> [String] -> ALPM ()
optionSetNoUpgrades = setList .  {# call option_set_noupgrades #}

optionRemoveNoUpgrade :: Handle -> String -> ALPM ()
optionRemoveNoUpgrade = setString . {# call option_remove_noupgrade #}

optionGetNoExtracts :: Handle -> ALPM (List String)
optionGetNoExtracts = valueToList .  {# call option_get_noextracts #}

optionAddNoExtract :: Handle -> String -> ALPM ()
optionAddNoExtract =  setString . {# call option_add_noextract #}

optionSetNoExtracts :: Handle -> [String] -> ALPM ()
optionSetNoExtracts = setList . {# call option_set_noextracts #}

optionRemoveNoExtract :: Handle -> String -> ALPM ()
optionRemoveNoExtract = setString . {# call option_remove_noextract #}

optionGetIgnorePkgs :: Handle -> ALPM (List String)
optionGetIgnorePkgs = valueToList . {# call option_get_ignorepkgs #}

optionAddIgnorePkg :: Handle -> String -> ALPM ()
optionAddIgnorePkg = setString . {# call option_add_ignorepkg #}

optionSetIgnorePkgs :: Handle -> [String] -> ALPM ()
optionSetIgnorePkgs = setList . {# call option_set_ignorepkgs #}

optionRemoveIgnorePkg :: Handle -> String -> ALPM ()
optionRemoveIgnorePkg = setString . {# call option_remove_ignorepkg #}

optionGetIgnoreGrps :: Handle -> ALPM (List String)
optionGetIgnoreGrps = valueToList . {# call option_get_ignoregroups #}

optionAddIgnoreGrp :: Handle -> String -> ALPM ()
optionAddIgnoreGrp = setString . {# call option_add_ignoregroup #}

optionSetIgnoreGrps :: Handle -> [String] -> ALPM ()
optionSetIgnoreGrps = setList . {# call option_set_ignoregroups #}

optionRemoveIgnoreGrp :: Handle -> String -> ALPM ()
optionRemoveIgnoreGrp = setString . {# call option_remove_ignoregroup #}

optionGetArchitecture :: Handle -> ALPM (Maybe String)
optionGetArchitecture = getString . {# call option_get_arch #}

optionSetArchitecture :: Handle -> String -> ALPM ()
optionSetArchitecture = setString .  {# call option_set_arch #}

optionGetUseDelta :: Handle -> ALPM Bool
optionGetUseDelta = getBool . {# call option_get_usedelta #}

optionSetUserDelta :: Handle -> Bool -> ALPM ()
optionSetUserDelta = setBool .  {# call option_set_usedelta #}

optionGetCheckSpace :: Handle -> ALPM Bool
optionGetCheckSpace = getBool . {# call option_get_checkspace #}

optionSetCheckSpace :: Handle -> Bool -> ALPM ()
optionSetCheckSpace = setBool .  {# call option_set_checkspace #}

optionGetLocalDatabase :: Handle -> ALPM Database
optionGetLocalDatabase = liftIO .  {# call option_get_localdb #}

-- db.h
-- alpm_list_t *alpm_option_get_syncdbs(void);


-- Database ------------------------------------------------------------------

databaseRegisterSync ::Handle ->  String -> Int -> ALPM (Maybe Database)
databaseRegisterSync hdl name pgp = stringToMaybeValue 
  (\ cname -> {# call db_register_sync #} hdl cname (fromIntegral pgp))
  name 

databaseUnregister :: Database -> ALPM ()
databaseUnregister db = withErrorHandling $ {# call db_unregister #} db

databaseUnregisterAll :: Handle ->  ALPM ()
databaseUnregisterAll = withErrorHandling . {# call db_unregister_all #}

databaseGetName :: Database -> ALPM String
databaseGetName = valueToString . {# call alpm_db_get_name #}

databaseGetServers :: Database -> ALPM (List String)
databaseGetServers = valueToList . {# call alpm_db_get_servers #}

databaseAddServer :: Database -> String -> ALPM ()
databaseAddServer db = setString $ {# call db_add_server #} db

databaseUpdate :: Database -> LogLevel -> ALPM ()
databaseUpdate db = setEnum (flip {# call db_update #} db)

databaseGetPackage :: Database -> String -> ALPM (Maybe Package)
databaseGetPackage db = stringToMaybeValue $ {# call db_get_pkg #} db

databaseGetPackageCache :: Database -> ALPM (List Package)
databaseGetPackageCache = valueToList . {# call db_get_pkgcache #}

databaseReadGroup :: Database -> String -> ALPM (Maybe Group)
databaseReadGroup db = stringToMaybeValue $ {# call db_readgroup #} db

databaseGetGroupCache :: Database -> ALPM (List Group)
databaseGetGroupCache = valueToList . {# call db_get_groupcache #}

databaseSearch :: Database -> [String] -> ALPM (List Package)
databaseSearch db = valueToListALPM . setList_ ({# call db_search #} db)

-- int alpm_db_set_pkgreason(pmdb_t *db, const char *name, pmpkgreason_t reason);


-- Package -------------------------------------------------------------------

-- /* Info parameters */

-- int alpm_pkg_load(const char *filename, int full, pmpkg_t **pkg);
-- int alpm_pkg_free(pmpkg_t *pkg);
-- int alpm_pkg_checkmd5sum(pmpkg_t *pkg);
-- char *alpm_fetch_pkgurl(const char *url);
-- int alpm_pkg_vercmp(const char *a, const char *b);
-- alpm_list_t *alpm_pkg_compute_requiredby(pmpkg_t *pkg);

packageGetFilename :: Package -> ALPM FilePath
packageGetFilename = valueToString . {# call pkg_get_filename #}

packageGetName :: Package -> ALPM String
packageGetName = valueToString . {# call pkg_get_name #} 

packageGetVersion :: Package -> ALPM String
packageGetVersion = valueToString . {# call pkg_get_version #}

packageGetDescription :: Package -> ALPM String
packageGetDescription = valueToString . {# call pkg_get_desc #}

packageGetURL :: Package -> ALPM String
packageGetURL = valueToString . {# call pkg_get_url #}

-- time_t alpm_pkg_get_builddate(pmpkg_t *pkg);
-- time_t alpm_pkg_get_installdate(pmpkg_t *pkg);

packageGetPackager :: Package -> ALPM String
packageGetPackager = valueToString . {# call pkg_get_packager #}

packageGetMd5Sum :: Package -> ALPM String
packageGetMd5Sum = valueToString . {# call pkg_get_md5sum #}

packageGetArchitecture :: Package -> ALPM String
packageGetArchitecture = valueToString . {# call pkg_get_arch #}

-- off_t alpm_pkg_get_size(pmpkg_t *pkg);
-- off_t alpm_pkg_get_isize(pmpkg_t *pkg);

packageGetLicenses :: Package -> ALPM (List String)
packageGetLicenses = valueToList . {# call pkg_get_licenses #}

packageGetGroups :: Package -> ALPM (List String)
packageGetGroups = valueToList . {# call pkg_get_groups #}

packageGetDependencies :: Package -> ALPM (List Dependency)
packageGetDependencies = valueToList . {# call pkg_get_depends #}

packageGetOptionalDependencies :: Package -> ALPM (List Dependency)
packageGetOptionalDependencies = valueToList . {# call pkg_get_optdepends #}

packageGetConflicts :: Package -> ALPM (List Conflict)
packageGetConflicts = valueToList . {# call pkg_get_conflicts #}

packageGetProvides :: Package -> ALPM (List String)
packageGetProvides = valueToList . {# call pkg_get_provides #}

packageGetDeltas :: Package -> ALPM (List Delta)
packageGetDeltas = valueToList . {# call pkg_get_deltas #}

packageGetReplaces :: Package -> ALPM (List String)
packageGetReplaces = valueToList . {# call pkg_get_replaces #}

packageGetFiles :: Package -> ALPM (List FilePath)
packageGetFiles = valueToList . {# call pkg_get_files #}

packageGetBackup :: Package -> ALPM (List String)
packageGetBackup = valueToList . {# call pkg_get_backup #}

packageGetDatabase :: Package -> ALPM Database
packageGetDatabase = liftIO . {# call pkg_get_db #}

-- void *alpm_pkg_changelog_open(pmpkg_t *pkg);
-- size_t alpm_pkg_changelog_read(void *ptr, size_t size,
-- 		const pmpkg_t *pkg, const void *fp);
-- /*int alpm_pkg_changelog_feof(const pmpkg_t *pkg, void *fp);*/
-- int alpm_pkg_changelog_close(const pmpkg_t *pkg, void *fp);
-- int alpm_pkg_has_scriptlet(pmpkg_t *pkg);

-- off_t alpm_pkg_download_size(pmpkg_t *newpkg);

packageUnusedDeltas :: Package -> ALPM (List Delta)
packageUnusedDeltas = valueToList . {# call pkg_unused_deltas #}

-- Delta ---------------------------------------------------------------------

-- Group ---------------------------------------------------------------------

findGroupPackages :: List Database -> String -> ALPM (List Group)
findGroupPackages db str = valueToList . 
    withCString str $ \cstr ->
      {# call find_group_pkgs #} (castPtr $ unList db)  cstr


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

-- alpm_list_t *alpm_checkconflicts(pmhandle_t *handle, alpm_list_t *pkglist);

dependencyComputeString :: Dependency -> ALPM String
dependencyComputeString = valueToString . {# call dep_compute_string #}


-- Helpers -------------------------------------------------------------------

computeMd5Sum :: String -> ALPM String
computeMd5Sum name = liftIO $
    withCString name {# call compute_md5sum #} >>= peekCString

strError :: Error -> ALPM String
strError err = liftIO $
    {# call strerror #} (fromIntegral $ fromEnum err) >>= peekCString
