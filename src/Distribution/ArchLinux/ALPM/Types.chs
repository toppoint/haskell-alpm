{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm" #}

module Distribution.ArchLinux.ALPM.Types
    ( 
      -- * Types
      Database
    , unDatabase

    , Package
    , Delta
    , Group
    , Transaction
    , Dependency
    , DependencyMissing
    , Conflict
    , FileConflict


      -- * Enums
    , LogLevel (..)
    , PkgReason (..)
    , TransactionFlag (..)
    , TransactionEvent (..)
    , TransactionConversation (..)
    , TransactionProgress (..)
    , DependencyModule (..)
    , FileConflictType (..)
    , Error (..)
    )
where

#include <alpm.h>

import Foreign

-- Types ---------------------------------------------------------------------

{# pointer *pmdb_t as Database newtype #}

unDatabase :: Database -> Ptr Database
unDatabase (Database ptr) = ptr

{# pointer *pmpkg_t as Package newtype #}

{# pointer *pmdelta_t as Delta newtype #}

{# pointer *pmgrp_t as Group newtype #}

{# pointer *pmtrans_t as Transaction newtype #}

{# pointer *pmdepend_t as Dependency newtype #}

{# pointer *pmdepmissing_t as DependencyMissing newtype #}

{# pointer *pmconflict_t as Conflict newtype #}

{# pointer *pmfileconflict_t as FileConflict newtype #}

-- Enums ---------------------------------------------------------------------

-- | The log levels.
{# enum pmloglevel_t as LogLevel {underscoreToCase}
    with prefix = "PM_" deriving (Eq, Read, Show) #}

-- | Reason why the package was installed.
{# enum pmpkgreason_t as PkgReason {underscoreToCase}
     with prefix = "PM_" deriving (Eq, Read, Show) #}

-- | Flags.
{# enum pmtransflag_t as TransactionFlag {underscoreToCase}
     with prefix = "PM_" deriving (Eq, Read, Show) #}

-- | Transaction events.
{# enum pmtransevt_t as TransactionEvent {underscoreToCase}
     with prefix = "PM_" deriving (Eq, Read, Show) #}

-- | Transaction conversations.
{# enum pmtransconv_t as TransactionConversation {underscoreToCase}
     with prefix = "PM_" deriving (Eq, Read, Show) #}

-- | Transaction progress.
{# enum pmtransprog_t as TransactionProgress {underscoreToCase}
     with prefix = "PM_" deriving (Eq, Read, Show) #}

-- | Dependencies and conflicts.
{# enum pmdepmod_t as DependencyModule {underscoreToCase}
    with prefix = "PM_" deriving (Eq, Read, Show) #}

-- | File conflicts.
{# enum pmfileconflicttype_t as FileConflictType {underscoreToCase}
    with prefix = "PM_" deriving (Eq, Read, Show) #}

-- | Errors.
{# enum _pmerrno_t as Error {underscoreToCase}
    with prefix = "PM_" deriving (Eq, Read, Show) #}

