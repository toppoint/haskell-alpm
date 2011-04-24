{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}

{# context lib="alpm" prefix="alpm" #}

module Distribution.ArchLinux.ALPM.Internal.Types
    ( 
      -- * ArchLinux Package Managment Type Class
      ALPMType (..)

      -- * Types
    , Database
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

import Control.Monad
import Foreign
import Foreign.C.String

-- Packable Type -------------------------------------------------------------

class ALPMType a where
    unpack :: a -> IO (Ptr a)
    pack   :: Ptr a -> IO a 

-- Types ---------------------------------------------------------------------

instance ALPMType String where
    unpack = liftM castPtr .  newCString 
    pack   = peekCString . castPtr 

{# pointer *pmdb_t as Database newtype #}

instance ALPMType Database where
    unpack (Database ptr) = return ptr
    pack = return . Database

{# pointer *pmpkg_t as Package newtype #}

instance ALPMType Package where
    unpack (Package ptr) = return ptr
    pack = return . Package

{# pointer *pmdelta_t as Delta newtype #}

instance ALPMType Delta where
    unpack (Delta ptr) = return ptr
    pack = return . Delta

{# pointer *pmgrp_t as Group newtype #}

instance ALPMType Group where
    unpack (Group ptr) = return ptr
    pack = return . Group

{# pointer *pmtrans_t as Transaction newtype #}

instance ALPMType Transaction where
    unpack (Transaction ptr) = return ptr
    pack = return . Transaction

{# pointer *pmdepend_t as Dependency newtype #}

instance ALPMType Dependency where
    unpack (Dependency ptr) = return ptr
    pack = return . Dependency

{# pointer *pmdepmissing_t as DependencyMissing newtype #}

instance ALPMType DependencyMissing where
    unpack (DependencyMissing ptr) = return ptr
    pack = return . DependencyMissing

{# pointer *pmconflict_t as Conflict newtype #}

instance ALPMType Conflict where
    unpack (Conflict ptr) = return ptr
    pack = return . Conflict

{# pointer *pmfileconflict_t as FileConflict newtype #}

instance ALPMType FileConflict where
    unpack (FileConflict ptr) = return ptr
    pack = return . FileConflict

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

