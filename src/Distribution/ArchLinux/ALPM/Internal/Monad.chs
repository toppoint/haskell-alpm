{-# LANGUAGE
        FlexibleContexts,
        ForeignFunctionInterface,
        GeneralizedNewtypeDeriving
  #-}

{# context lib="alpm" prefix="alpm" #}

module Distribution.ArchLinux.ALPM.Internal.Monad
    (
      -- * Types
      ALPM
    , Exception (..)

      -- * Functions
    , runALPM
    , runDefaultALPM
    , handleError
    , getHandle

    , throw
    , catch
    )
where

#include <alpm.h>

import Prelude hiding (catch)

import qualified Control.Monad.Error as E
import qualified Control.Monad.Reader as R

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

{# import Distribution.ArchLinux.ALPM.Internal.Types #}


-- Types ---------------------------------------------------------------------

newtype ALPM a = ALPM (E.ErrorT Exception (R.ReaderT Handle IO) a)
  deriving (Monad, R.MonadIO, E.MonadError Exception, R.MonadReader Handle)

data Exception = UnknownError Int
               | Error Error
  deriving (Eq, Read, Show)

instance E.Error Exception where
    noMsg = UnknownError 0
    strMsg _ = UnknownError 0

-- Functions -----------------------------------------------------------------

-- | Run an alpm function with given root and database location. 
runALPM
    :: String                   -- ^ pacman installation root directory
    -> String                   -- ^ pacman db directory
    -> ALPM a                   -- ^ action to perform
    -> IO (Either Exception a)  -- ^ result of action or error that occured
runALPM root dbdir (ALPM action) = 
    withCString dbdir $ \ dbdir' -> 
        withCString root  $ \ root' -> do
            perr <- malloc :: IO (Ptr CInt)
            poke perr 0

            hdl <- {# call initialize #} root' dbdir' perr

            err <- peek perr
            free perr

            if err /= 0 
                then if err > 0 
                    then return . Left . Error . toEnum . fromIntegral $ err
                    else return . Left . UnknownError . fromIntegral $ err
                else do
                    result <- R.runReaderT (E.runErrorT action) hdl
                    _ <- {# call release #} hdl
                    return result 

-- | Run an alpm function with default enviroment settings.
runDefaultALPM :: ALPM a -> IO (Either Exception a)
runDefaultALPM = runALPM "/" "/var/lib/pacman"

-- | Handle Errorcode
handleError :: CInt -> ALPM ()
handleError 0 = return ()
handleError e
  | e < 0     = throw $ UnknownError $ fromIntegral e
  | otherwise = throw $ Error $ toEnum $ fromIntegral e

-- | Return the enviroment handle
getHandle :: ALPM Handle
getHandle = R.ask

-- | Throw an exception
throw :: Exception -> ALPM a
throw = E.throwError

-- | Catch an exception
catch :: ALPM a -> ALPM (Either Exception a)
catch action = E.catchError (action >>= return . Right) (return . Left)
