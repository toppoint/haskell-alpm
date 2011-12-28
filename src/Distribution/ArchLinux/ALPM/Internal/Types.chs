{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances, FlexibleInstances #-}

{# context lib="alpm" prefix="alpm" #}

module Distribution.ArchLinux.ALPM.Internal.Types
    ( 
      -- * ArchLinux Package Managment Type Class
      ALPMType (..)

      -- * Types
    , Conflict
    , Database
    , Delta
    , Dependency
    , DependencyMissing
    , FileConflict
    , Group
    , Handle 
    , Package
    , Transaction

      -- * Enums
    , DependencyModule (..)
    , Error (..)
    , Event (..)
    , FileConflictType (..)
    , LogLevel (..)
    , PkgReason (..)
    , Progress (..)
    , Question (..)
    , TransactionFlag (..)
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

{# pointer *alpm_db_t as Database newtype #}

instance ALPMType Database where
    unpack (Database ptr) = return ptr
    pack = return . Database

{# pointer *alpm_pkg_t as Package newtype #}

instance ALPMType Package where
    unpack (Package ptr) = return ptr
    pack = return . Package

{# pointer *alpm_delta_t as Delta newtype #}

instance ALPMType Delta where
    unpack (Delta ptr) = return ptr
    pack = return . Delta

{# pointer *alpm_group_t as Group newtype #}

instance ALPMType Group where
    unpack (Group ptr) = return ptr
    pack = return . Group

{# pointer *alpm_trans_t as Transaction newtype #}

instance ALPMType Transaction where
    unpack (Transaction ptr) = return ptr
    pack = return . Transaction

{# pointer *alpm_depend_t as Dependency newtype #}

instance ALPMType Dependency where
    unpack (Dependency ptr) = return ptr
    pack = return . Dependency

{# pointer *alpm_depmissing_t as DependencyMissing newtype #}

instance ALPMType DependencyMissing where
    unpack (DependencyMissing ptr) = return ptr
    pack = return . DependencyMissing

{# pointer *alpm_conflict_t as Conflict newtype #}

instance ALPMType Conflict where
    unpack (Conflict ptr) = return ptr
    pack = return . Conflict

{# pointer *alpm_fileconflict_t as FileConflict newtype #}

instance ALPMType FileConflict where
    unpack (FileConflict ptr) = return ptr
    pack = return . FileConflict

{# pointer *alpm_handle_t as Handle newtype #}

instance ALPMType Handle where
    unpack (Handle ptr) = return ptr
    pack = return .Handle  

-- Enums ---------------------------------------------------------------------

-- | The log levels.
{# enum alpm_loglevel_t as LogLevel {underscoreToCase}
    with prefix = "PM_" deriving (Eq, Read, Show) #}

-- | Reason why the package was installed.
{# enum alpm_pkgreason_t as PkgReason {underscoreToCase}
     with prefix = "PM_" deriving (Eq, Read, Show) #}

-- | Flags.
{# enum alpm_transflag_t as TransactionFlag {underscoreToCase}
     with prefix = "PM_" deriving (Eq, Read, Show) #}

-- | Events
{#enum alpm_event_t as Event {underscoreToCase}
    with prefix = "ALPM_" deriving (Eq,Read,Show) #}

-- | Question
{#enum alpm_question_t as Question {underscoreToCase}
    with prefix = "ALPM_" deriving (Eq,Read,Show) #}

-- | Progress
{#enum alpm_progress_t as Progress {underscoreToCase}
    with prefix = "ALPM_" deriving (Eq,Read,Show) #}

-- | Dependencies and conflicts.
{# enum alpm_depmod_t as DependencyModule {underscoreToCase}
    with prefix = "PM_" deriving (Eq, Read, Show) #}

-- | File conflicts.
{# enum alpm_fileconflicttype_t as FileConflictType {underscoreToCase}
    with prefix = "PM_" deriving (Eq, Read, Show) #}

-- | Errors.
{# enum _alpm_errno_t as Error {underscoreToCase}
    with prefix = "PM_" deriving (Eq, Read, Show) #}

