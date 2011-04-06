{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm" #}

module Distribution.ArchLinux.ALPM.Core
where

#include <alpm.h>

{# import Distribution.ArchLinux.ALPM.Types #}

import Foreign.C
import Foreign.Ptr

-- | This function needs to be called first or nothing else will work.
initialize :: IO CInt
initialize =
    {# call initialize #}

-- | Call this function to clean up. After this the library is no longer
-- available
release :: IO CInt
release =
    {# call release #}

getVersion :: IO String
getVersion =
    {# call version #}
        >>= peekCString

{-
/*
 * Logging facilities
 */

typedef void (*alpm_cb_log)(pmloglevel_t, const char *, va_list);
int alpm_logaction(const char *fmt, ...);

/*
 * Downloading
 */

typedef void (*alpm_cb_download)(const char *filename,
		off_t xfered, off_t total);
typedef void (*alpm_cb_totaldl)(off_t total);
/** A callback for downloading files
 * @param url the URL of the file to be downloaded
 * @param localpath the directory to which the file should be downloaded
 * @param force whether to force an update, even if the file is the same
 * @return 0 on success, 1 if the file exists and is identical, -1 on
 * error.
 */
typedef int (*alpm_cb_fetch)(const char *url, const char *localpath,
		int force);

/*
 * Options
 */

alpm_cb_log alpm_option_get_logcb(void);
void alpm_option_set_logcb(alpm_cb_log cb);

alpm_cb_download alpm_option_get_dlcb(void);
void alpm_option_set_dlcb(alpm_cb_download cb);

alpm_cb_fetch alpm_option_get_fetchcb(void);
void alpm_option_set_fetchcb(alpm_cb_fetch cb);

alpm_cb_totaldl alpm_option_get_totaldlcb(void);
void alpm_option_set_totaldlcb(alpm_cb_totaldl cb);

const char *alpm_option_get_root(void);
int alpm_option_set_root(const char *root);

const char *alpm_option_get_dbpath(void);
int alpm_option_set_dbpath(const char *dbpath);

alpm_list_t *alpm_option_get_cachedirs(void);
int alpm_option_add_cachedir(const char *cachedir);
void alpm_option_set_cachedirs(alpm_list_t *cachedirs);
int alpm_option_remove_cachedir(const char *cachedir);

const char *alpm_option_get_logfile(void);
int alpm_option_set_logfile(const char *logfile);

const char *alpm_option_get_lockfile(void);
/* no set_lockfile, path is determined from dbpath */

int alpm_option_get_usesyslog(void);
void alpm_option_set_usesyslog(int usesyslog);

alpm_list_t *alpm_option_get_noupgrades(void);
void alpm_option_add_noupgrade(const char *pkg);
void alpm_option_set_noupgrades(alpm_list_t *noupgrade);
int alpm_option_remove_noupgrade(const char *pkg);

alpm_list_t *alpm_option_get_noextracts(void);
void alpm_option_add_noextract(const char *pkg);
void alpm_option_set_noextracts(alpm_list_t *noextract);
int alpm_option_remove_noextract(const char *pkg);

alpm_list_t *alpm_option_get_ignorepkgs(void);
void alpm_option_add_ignorepkg(const char *pkg);
void alpm_option_set_ignorepkgs(alpm_list_t *ignorepkgs);
int alpm_option_remove_ignorepkg(const char *pkg);

alpm_list_t *alpm_option_get_ignoregrps(void);
void alpm_option_add_ignoregrp(const char *grp);
void alpm_option_set_ignoregrps(alpm_list_t *ignoregrps);
int alpm_option_remove_ignoregrp(const char *grp);

const char *alpm_option_get_arch(void);
void alpm_option_set_arch(const char *arch);

int alpm_option_get_usedelta(void);
void alpm_option_set_usedelta(int usedelta);

int alpm_option_get_checkspace(void);
void alpm_option_set_checkspace(int checkspace);

pmdb_t *alpm_option_get_localdb(void);
alpm_list_t *alpm_option_get_syncdbs(void);
-}

checkForNull :: (a -> Ptr a) -> a -> Maybe a
checkForNull unObj obj
  | unObj obj == nullPtr = Nothing
  | otherwise            = Just obj

databaseRegisterSync :: String -> IO (Maybe Database)
databaseRegisterSync treeName = do
    db <- withCString treeName $ {# call db_register_sync #}
    return $ checkForNull unDatabase db

databaseUnregister :: Database -> IO CInt
databaseUnregister db =
    {# call db_unregister #} db

{-
/*
 * Databases
 */

int alpm_db_unregister_all(void);

const char *alpm_db_get_name(const pmdb_t *db);
const char *alpm_db_get_url(const pmdb_t *db);

int alpm_db_setserver(pmdb_t *db, const char *url);

int alpm_db_update(int level, pmdb_t *db);

pmpkg_t *alpm_db_get_pkg(pmdb_t *db, const char *name);
alpm_list_t *alpm_db_get_pkgcache(pmdb_t *db);

pmgrp_t *alpm_db_readgrp(pmdb_t *db, const char *name);
alpm_list_t *alpm_db_get_grpcache(pmdb_t *db);
alpm_list_t *alpm_db_search(pmdb_t *db, const alpm_list_t* needles);
int alpm_db_set_pkgreason(pmdb_t *db, const char *name, pmpkgreason_t reason);
-}

{-
/*
 * Packages
 */

/* Info parameters */

int alpm_pkg_load(const char *filename, int full, pmpkg_t **pkg);
int alpm_pkg_free(pmpkg_t *pkg);
int alpm_pkg_checkmd5sum(pmpkg_t *pkg);
char *alpm_fetch_pkgurl(const char *url);
int alpm_pkg_vercmp(const char *a, const char *b);
alpm_list_t *alpm_pkg_compute_requiredby(pmpkg_t *pkg);

const char *alpm_pkg_get_filename(pmpkg_t *pkg);
const char *alpm_pkg_get_name(pmpkg_t *pkg);
const char *alpm_pkg_get_version(pmpkg_t *pkg);
const char *alpm_pkg_get_desc(pmpkg_t *pkg);
const char *alpm_pkg_get_url(pmpkg_t *pkg);
time_t alpm_pkg_get_builddate(pmpkg_t *pkg);
time_t alpm_pkg_get_installdate(pmpkg_t *pkg);
const char *alpm_pkg_get_packager(pmpkg_t *pkg);
const char *alpm_pkg_get_md5sum(pmpkg_t *pkg);
const char *alpm_pkg_get_arch(pmpkg_t *pkg);
off_t alpm_pkg_get_size(pmpkg_t *pkg);
off_t alpm_pkg_get_isize(pmpkg_t *pkg);
pmpkgreason_t alpm_pkg_get_reason(pmpkg_t *pkg);
alpm_list_t *alpm_pkg_get_licenses(pmpkg_t *pkg);
alpm_list_t *alpm_pkg_get_groups(pmpkg_t *pkg);
alpm_list_t *alpm_pkg_get_depends(pmpkg_t *pkg);
alpm_list_t *alpm_pkg_get_optdepends(pmpkg_t *pkg);
alpm_list_t *alpm_pkg_get_conflicts(pmpkg_t *pkg);
alpm_list_t *alpm_pkg_get_provides(pmpkg_t *pkg);
alpm_list_t *alpm_pkg_get_deltas(pmpkg_t *pkg);
alpm_list_t *alpm_pkg_get_replaces(pmpkg_t *pkg);
alpm_list_t *alpm_pkg_get_files(pmpkg_t *pkg);
alpm_list_t *alpm_pkg_get_backup(pmpkg_t *pkg);
pmdb_t *alpm_pkg_get_db(pmpkg_t *pkg);
void *alpm_pkg_changelog_open(pmpkg_t *pkg);
size_t alpm_pkg_changelog_read(void *ptr, size_t size,
		const pmpkg_t *pkg, const void *fp);
/*int alpm_pkg_changelog_feof(const pmpkg_t *pkg, void *fp);*/
int alpm_pkg_changelog_close(const pmpkg_t *pkg, void *fp);
int alpm_pkg_has_scriptlet(pmpkg_t *pkg);

off_t alpm_pkg_download_size(pmpkg_t *newpkg);
alpm_list_t *alpm_pkg_unused_deltas(pmpkg_t *pkg);

/*
 * Deltas
 */

const char *alpm_delta_get_from(pmdelta_t *delta);
const char *alpm_delta_get_to(pmdelta_t *delta);
const char *alpm_delta_get_filename(pmdelta_t *delta);
const char *alpm_delta_get_md5sum(pmdelta_t *delta);
off_t alpm_delta_get_size(pmdelta_t *delta);

/*
 * Groups
 */
const char *alpm_grp_get_name(const pmgrp_t *grp);
alpm_list_t *alpm_grp_get_pkgs(const pmgrp_t *grp);
alpm_list_t *alpm_find_grp_pkgs(alpm_list_t *dbs, const char *name);

/*
 * Sync
 */

pmpkg_t *alpm_sync_newversion(pmpkg_t *pkg, alpm_list_t *dbs_sync);

/*
 * Transactions
 */

/* Transaction Event callback */
typedef void (*alpm_trans_cb_event)(pmtransevt_t, void *, void *);

/* Transaction Conversation callback */
typedef void (*alpm_trans_cb_conv)(pmtransconv_t, void *, void *,
                                   void *, int *);

/* Transaction Progress callback */
typedef void (*alpm_trans_cb_progress)(pmtransprog_t, const char *, int, size_t, size_t);

int alpm_trans_get_flags(void);
alpm_list_t * alpm_trans_get_add(void);
alpm_list_t * alpm_trans_get_remove(void);
int alpm_trans_init(pmtransflag_t flags,
                    alpm_trans_cb_event cb_event, alpm_trans_cb_conv conv,
                    alpm_trans_cb_progress cb_progress);
int alpm_trans_prepare(alpm_list_t **data);
int alpm_trans_commit(alpm_list_t **data);
int alpm_trans_interrupt(void);
int alpm_trans_release(void);

int alpm_sync_sysupgrade(int enable_downgrade);
int alpm_add_pkg(pmpkg_t *pkg);
int alpm_remove_pkg(pmpkg_t *pkg);

/*
 * Dependencies and conflicts
 */

alpm_list_t *alpm_checkdeps(alpm_list_t *pkglist, int reversedeps,
		alpm_list_t *remove, alpm_list_t *upgrade);
pmpkg_t *alpm_find_satisfier(alpm_list_t *pkgs, const char *depstring);
pmpkg_t *alpm_find_dbs_satisfier(alpm_list_t *dbs, const char *depstring);

const char *alpm_miss_get_target(const pmdepmissing_t *miss);
pmdepend_t *alpm_miss_get_dep(pmdepmissing_t *miss);
const char *alpm_miss_get_causingpkg(const pmdepmissing_t *miss);

alpm_list_t *alpm_checkconflicts(alpm_list_t *pkglist);

const char *alpm_conflict_get_package1(pmconflict_t *conflict);
const char *alpm_conflict_get_package2(pmconflict_t *conflict);
const char *alpm_conflict_get_reason(pmconflict_t *conflict);

pmdepmod_t alpm_dep_get_mod(const pmdepend_t *dep);
const char *alpm_dep_get_name(const pmdepend_t *dep);
const char *alpm_dep_get_version(const pmdepend_t *dep);
char *alpm_dep_compute_string(const pmdepend_t *dep);

/*
 * File conflicts
 */

const char *alpm_fileconflict_get_target(pmfileconflict_t *conflict);
pmfileconflicttype_t alpm_fileconflict_get_type(pmfileconflict_t *conflict);
const char *alpm_fileconflict_get_file(pmfileconflict_t *conflict);
const char *alpm_fileconflict_get_ctarget(pmfileconflict_t *conflict);

/*
 * Helpers
 */

/* checksums */
char *alpm_compute_md5sum(const char *name);

const char *alpm_strerror(int err);
const char *alpm_strerrorlast(void);
-}

