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
type CallbackTotalDownload = Int -> IO ()
type CallbackFetch = String -> String -> Bool -> IO Int

-- typedef void (*alpm_cb_progress)(alpm_progress_t, const char *, int, size_t, size_t);
type CallbackProgress = Progress -> String -> Int -> Int -> Int -> IO ()

-- typedef void (*alpm_cb_question)(alpm_question_t, void *, void *, void *, int *);
-- type CallbackQuestion = Question -> ...... ?

-- typedef void (*alpm_cb_event)(alpm_event_t, void *, void *);
-- typedef void (*alpm_cb_log)(alpm_loglevel_t, const char *, va_list);



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
setList setter lst = withErrorHandling $ (setter . castPtr . unList)
                     =<< toList lst 

setList_ :: ALPMType b => (Ptr a -> IO c) -> [b] -> ALPM c
setList_ setter lst = liftIO $ (setter . castPtr . unList) =<< toList lst 

withList :: (Ptr a -> IO CInt) -> List b -> ALPM ()
withList setter lst = withErrorHandling $ (setter . castPtr . unList) lst

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

foreign import ccall "wrapper" 
  _wrap_cb_total_download :: (CLong -> IO ())
                          -> IO (FunPtr (CLong -> IO ()))

wrap_cb_total_download :: CallbackTotalDownload 
                       -> IO (FunPtr (CLong -> IO ()))
wrap_cb_total_download f = _wrap_cb_total_download $ \ ctd -> do
  let td = fromIntegral ctd
  f td
foreign import ccall "wrapper"
  _wrap_cb_fetch :: (CString -> CString -> CInt -> IO CInt)
                 -> IO (FunPtr ((CString -> CString -> CInt -> IO CInt)))

wrap_cb_fetch :: CallbackFetch -> IO (FunPtr ((CString -> CString -> CInt -> IO CInt)))
wrap_cb_fetch f = _wrap_cb_fetch $ \ cs1 cs2 ci -> do
  s1 <- peekCString cs1
  s2 <- peekCString cs2
  let i = toBool ci
  return . fromIntegral =<<  f s1 s2 i

-- type CallbackProgress = Progress -> String -> Int -> Int -> Int -> IO ()
foreign import ccall "wrapper" 
  _wrap_cb_progress :: (CInt -> CString -> CInt -> CUInt -> CUInt -> IO ())
                    -> IO (FunPtr (CInt -> CString -> CInt -> CUInt -> CUInt -> IO ()))

wrap_cb_progress :: CallbackProgress 
                 -> IO (FunPtr (CInt -> CString -> CInt -> CUInt -> CUInt -> IO ()))
wrap_cb_progress f = _wrap_cb_progress $ \ pg cstr ci1 ci2 ci3 -> do
  str <- peekCString cstr
  let i1 = fromIntegral ci1
      i2 = fromIntegral ci2
      i3 = fromIntegral ci3
      epg = toEnum $ fromIntegral pg
  f epg str i1 i2 i3


-- General ------------------------------------------------------------------

-- | Get ALPM's version.
getVersion :: ALPM String
getVersion = liftIO $
    {# call version #} >>= peekCString


-- Logging -------------------------------------------------------------------

-- Downloading ---------------------------------------------------------------

-- | Fetch a remote pkg.
--   throws an exception on error.
fetchPackageURL :: String        -- ^ URL of the package to download
                -> ALPM FilePath -- ^ the downloaded filepath on success.
fetchPackageURL url = do
  hdl <- getHandle
  maybeResult <- liftIO $ do
    curl <- newCString url
    result <- {# call fetch_pkgurl #} hdl curl
    maybePeek peekCString result
  case maybeResult of
    Just filepath -> return filepath
    Nothing       -> throw (UnknownError 0)


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

optionGetCacheDirs :: ALPM (List String)
optionGetCacheDirs = valueToList . {# call option_get_cachedirs #}
                    =<< getHandle

optionAddCacheDir :: FilePath -> ALPM ()
optionAddCacheDir fp = do 
  hdl <- getHandle 
  setString ({# call option_add_cachedir #} hdl) fp

optionSetCacheDirs :: [FilePath] -> ALPM ()
optionSetCacheDirs fpl =  do
  hdl <- getHandle 
  setList ({# call option_set_cachedirs #} hdl) fpl

optionRemoveCacheDir :: FilePath -> ALPM ()
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

optionGetUseSyslog :: ALPM Bool
optionGetUseSyslog = getBool . {# call option_get_usesyslog #} =<< getHandle

optionSetUseSyslog :: Bool -> ALPM ()
optionSetUseSyslog b = do
    hdl <- getHandle
    setBool ({# call option_set_usesyslog #} hdl) b

optionGetNoUpgrades :: ALPM (List String)
optionGetNoUpgrades = valueToList . {# call option_get_noupgrades #} =<< getHandle

optionAddNoUpgrade :: String -> ALPM ()
optionAddNoUpgrade nu = do
    hdl <- getHandle
    setString ({# call option_add_noupgrade #} hdl) nu

optionSetNoUpgrades :: [String] -> ALPM ()
optionSetNoUpgrades nus = do
    hdl <- getHandle
    setList ({# call option_set_noupgrades #} hdl) nus

optionRemoveNoUpgrade :: String -> ALPM ()
optionRemoveNoUpgrade nu = do
    hdl <- getHandle
    setString ({# call option_remove_noupgrade #} hdl) nu

optionGetNoExtracts :: ALPM (List String)
optionGetNoExtracts = valueToList . {# call option_get_noextracts #} =<< getHandle

optionAddNoExtract :: String -> ALPM ()
optionAddNoExtract ne = do
    hdl <- getHandle
    setString ({# call option_add_noextract #} hdl) ne

optionSetNoExtracts :: [String] -> ALPM ()
optionSetNoExtracts nes = do
    hdl <- getHandle
    setList ({# call option_set_noextracts #} hdl) nes

optionRemoveNoExtract :: String -> ALPM ()
optionRemoveNoExtract ne = do
    hdl <- getHandle
    setString ({# call option_remove_noextract #} hdl) ne

optionGetIgnorePkgs :: ALPM (List String)
optionGetIgnorePkgs =
    valueToList . {# call option_get_ignorepkgs #} =<< getHandle

optionAddIgnorePkg :: String -> ALPM ()
optionAddIgnorePkg pkg = do
    hdl <- getHandle
    setString ({# call option_add_ignorepkg #} hdl) pkg

optionSetIgnorePkgs :: [String] -> ALPM ()
optionSetIgnorePkgs ps = do
    hdl <- getHandle
    setList ({# call option_set_ignorepkgs #} hdl) ps

optionRemoveIgnorePkg :: String -> ALPM ()
optionRemoveIgnorePkg pkg = do
    hdl <- getHandle
    setString ({# call option_remove_ignorepkg #} hdl) pkg

optionGetIgnoreGrps :: ALPM (List String)
optionGetIgnoreGrps =
    valueToList . {# call option_get_ignoregroups #} =<< getHandle

optionAddIgnoreGrp :: String -> ALPM ()
optionAddIgnoreGrp group = do
    hdl <- getHandle
    setString ({# call option_add_ignoregroup #} hdl) group

optionSetIgnoreGrps :: [String] -> ALPM ()
optionSetIgnoreGrps gs = do
    hdl <- getHandle
    setList ({# call option_set_ignoregroups #} hdl) gs

optionRemoveIgnoreGrp :: String -> ALPM ()
optionRemoveIgnoreGrp group = do
    hdl <- getHandle
    setString ({# call option_remove_ignoregroup #} hdl) group

optionGetArchitecture :: ALPM (Maybe String)
optionGetArchitecture = getString . {# call option_get_arch #} =<< getHandle

optionSetArchitecture :: String -> ALPM ()
optionSetArchitecture arch = do
    hdl <- getHandle
    setString ({# call option_set_arch #} hdl) arch

optionGetUseDelta :: ALPM Bool
optionGetUseDelta = getBool . {# call option_get_usedelta #} =<< getHandle

optionSetUserDelta :: Bool -> ALPM ()
optionSetUserDelta b = do
    hdl <- getHandle
    setBool ({# call option_set_usedelta #} hdl) b

optionGetCheckSpace :: ALPM Bool
optionGetCheckSpace = getBool . {# call option_get_checkspace #} =<< getHandle

optionSetCheckSpace :: Bool -> ALPM ()
optionSetCheckSpace b = do
    hdl <- getHandle
    setBool ({# call option_set_checkspace #} hdl) b

optionGetLocalDatabase :: ALPM Database
optionGetLocalDatabase = liftIO . {# call option_get_localdb #} =<< getHandle

-- | Returns the callback used to report download progress.
-- alpm_cb_download alpm_option_get_dlcb(alpm_handle_t *handle);                   
-- optionGetDownloadCallback :: ALPM CallbackDownload

-- | Sets the callback used to report download progress.
optionSetDownloadCallback :: CallbackDownload -> ALPM ()
optionSetDownloadCallback dlcb = do
  hdl <- getHandle 
  withErrorHandling $ do
    clbk <- wrap_cb_download dlcb
    {# call alpm_option_set_dlcb #} hdl clbk 

-- |Returns the callback used to report total download size. 
--  alpm_cb_totaldl alpm_option_get_totaldlcb(alpm_handle_t *handle);
-- optionGetTotalDownloadCallback :: 

-- | Sets the callback used to report total download size. 
optionSetTotalDownloadCallback :: CallbackTotalDownload -> ALPM ()
optionSetTotalDownloadCallback tdcb = do
  hdl <- getHandle
  withErrorHandling $ do
    clbk <- wrap_cb_total_download tdcb
    {# call option_set_totaldlcb #} hdl clbk

-- | Returns the downloading callback. 
-- alpm_cb_fetch alpm_option_get_fetchcb(alpm_handle_t *handle);
-- optionGetFetchCallback :: 

-- | Sets the downloading callback.
optionSetFetchCallback :: CallbackFetch -> ALPM ()
optionSetFetchCallback fcb = do
  hdl <- getHandle 
  withErrorHandling $ do 
    clbk <- wrap_cb_fetch fcb
    {# call option_set_fetchcb #} hdl clbk

-- | Returns the callback used for operation progress.
--  alpm_cb_progress alpm_option_get_progresscb(alpm_handle_t *handle);

-- |Sets the callback used for operation progress. 
optionSetProgressCallback :: CallbackProgress -> ALPM ()
optionSetProgressCallback pcb = do
  hdl <- getHandle
  withErrorHandling $ do
    clbk <- wrap_cb_progress pcb
    {# call option_set_progresscb #} hdl clbk

-- Database ------------------------------------------------------------------

databaseRegisterSync :: String -> Bool -> ALPM (Maybe Database)
databaseRegisterSync name pgp = do
    hdl <- getHandle
    stringToMaybeValue 
        (\ cname -> {# call db_register_sync #} hdl cname (fromBool pgp))
        name 

databaseUnregister :: Database -> ALPM ()
databaseUnregister db = withErrorHandling $ {# call db_unregister #} db

databaseUnregisterAll :: ALPM ()
databaseUnregisterAll = withErrorHandling . {# call db_unregister_all #}
                        =<< getHandle

databaseGetName :: Database -> ALPM String
databaseGetName = valueToString . {# call alpm_db_get_name #}

-- | Get the signature verification level for a database.
--  Will return the default verification level if this database is set up
-- with ALPM_SIG_USE_DEFAULT.
databaseGetSignaturVerificationLevel :: Database -> ALPM SigLevel
databaseGetSignaturVerificationLevel = 
  valueToEnum . {# call db_get_siglevel #}

-- | Check the validity of a database.
--   This is most useful for sync databases and verifying signature status.
--  If invalid, the handle error code will be thrown.
databaseGetValid :: Database -> ALPM ()
databaseGetValid = 
  withErrorHandling . {# call db_get_valid #}

databaseGetServers :: Database -> ALPM (List String)
databaseGetServers = valueToList . {# call alpm_db_get_servers #}

databaseSetServers :: Database -> List String -> ALPM ()
databaseSetServers = withList . {# call db_set_servers #} 

databaseAddServer :: Database -> String -> ALPM ()
databaseAddServer = setString . {# call db_add_server #} 

databaseRemoveServer  :: Database -> String -> ALPM ()
databaseRemoveServer =  setString . {# call db_remove_server #}

databaseUpdate :: Database -> LogLevel -> ALPM ()
databaseUpdate = setEnum . (flip {# call db_update #})

databaseGetPackage :: Database -> String -> ALPM (Maybe Package)
databaseGetPackage = stringToMaybeValue . {# call db_get_pkg #}

databaseGetPackageCache :: Database -> ALPM (List Package)
databaseGetPackageCache = valueToList . {# call db_get_pkgcache #}

databaseReadGroup :: Database -> String -> ALPM (Maybe Group)
databaseReadGroup = stringToMaybeValue . {# call db_readgroup #} 

databaseGetGroupCache :: Database -> ALPM (List Group)
databaseGetGroupCache = valueToList . {# call db_get_groupcache #}

databaseSearch :: Database -> [String] -> ALPM (List Package)
databaseSearch db = valueToListALPM . setList_ ({# call db_search #} db)

-- int alpm_db_set_pkgreason(pmdb_t *db, const char *name, pmpkgreason_t reason);
databaseSetPackageReason :: Package -> PkgReason -> ALPM ()
databaseSetPackageReason pkg reason = do
  hdl <- getHandle
  setEnum ({# call db_set_pkgreason #} hdl pkg) reason


-- Package -------------------------------------------------------------------


-- int alpm_pkg_load(alpm_handle_t *handle, const char *filename, int full,	alpm_siglevel_t level, alpm_pkg_t **pkg);
-- int alpm_pkg_free(pmpkg_t *pkg);

-- | Check the integrity (with md5) of a package from the sync cache.
packageCheckMD5Sum :: Package -> ALPM Bool
packageCheckMD5Sum = liftM toBool . liftIO . {# call pkg_checkmd5sum #} 

-- int alpm_pkg_vercmp(const char *a, const char *b);
-- alpm_list_t *alpm_pkg_compute_requiredby(alpm_pkg_t *pkg);

packageGetFilename :: Package -> ALPM FilePath
packageGetFilename = valueToString . {# call pkg_get_filename #}

packageGetName :: Package -> ALPM String
packageGetName = valueToString . {# call pkg_get_name #} 

packageGetVersion :: Package -> ALPM String
packageGetVersion = valueToString . {# call pkg_get_version #}

-- alpm_pkgfrom_t alpm_pkg_get_origin(alpm_pkg_t *pkg);

packageGetDescription :: Package -> ALPM String
packageGetDescription = valueToString . {# call pkg_get_desc #}

packageGetURL :: Package -> ALPM String
packageGetURL = valueToString . {# call pkg_get_url #}

-- time_t alpm_pkg_get_builddate(pmpkg_t *pkg);
-- time_t alpm_pkg_get_installdate(pmpkg_t *pkg);

packageGetPackager :: Package -> ALPM String
packageGetPackager = valueToString . {# call pkg_get_packager #}

packageGetMD5Sum :: Package -> ALPM String
packageGetMD5Sum = valueToString . {# call pkg_get_md5sum #}

packageGetSHA256Sum :: Package -> ALPM String
packageGetSHA256Sum = valueToString . {# call pkg_get_sha256sum #}

packageGetArchitecture :: Package -> ALPM String
packageGetArchitecture = valueToString . {# call pkg_get_arch #}

-- | Returns the size of the package. This is only available for sync database
--  packages and package files, not those loaded from the local database.
packageGetSize :: Package -> ALPM Int
packageGetSize = liftM fromIntegral . liftIO . {# call pkg_get_size #}

-- | Returns the installed size of the package.
packageGetInstallSize :: Package -> ALPM Int
packageGetInstallSize = liftM fromIntegral . liftIO . {# call pkg_get_isize #}

-- alpm_pkgreason_t alpm_pkg_get_reason(alpm_pkg_t *pkg);

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

-- const char *alpm_pkg_get_base64_sig(alpm_pkg_t *pkg);

-- void *alpm_pkg_changelog_open(pmpkg_t *pkg);
-- size_t alpm_pkg_changelog_read(void *ptr, size_t size,
-- 		const pmpkg_t *pkg, const void *fp);
-- /*int alpm_pkg_changelog_feof(const pmpkg_t *pkg, void *fp);*/
-- int alpm_pkg_changelog_close(const pmpkg_t *pkg, void *fp);
-- int alpm_pkg_has_scriptlet(pmpkg_t *pkg);

-- off_t alpm_pkg_download_size(pmpkg_t *newpkg);

packageUnusedDeltas :: Package -> ALPM (List Delta)
packageUnusedDeltas = valueToList . {# call pkg_unused_deltas #}

-- Signatures ---------------------------------------------------------------

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

