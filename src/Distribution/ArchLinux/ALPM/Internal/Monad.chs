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
    , alpm
    , handleError

    , throw
    , catch
    )
where

#include <alpm.h>

import Prelude hiding (catch)

import qualified Control.Monad.Error as E
import Control.Monad.Trans
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

{# import Distribution.ArchLinux.ALPM.Internal.Types #}


-- Types ---------------------------------------------------------------------

newtype ALPM a = ALPM (E.ErrorT Exception IO a)
  deriving (Monad, MonadIO, E.MonadError Exception)

data Exception = UnknownError Int
               | Error Error
  deriving (Eq, Read, Show)

instance E.Error Exception where
    noMsg = UnknownError 0
    strMsg _ = UnknownError 0


-- Functions -----------------------------------------------------------------

run  :: ALPM a -> IO (Either Exception a)
run (ALPM action) = E.runErrorT action

alpm :: ALPM a -> IO (Either Exception a)
alpm a = run $ do
    handle <- initialize "/" "/var/lib/pacman"
    r <- catch a
    _ <- catch (release handle)

    case r of
        Left e ->
            throw e

        Right r' ->
            return r'

handleError :: CInt -> ALPM ()
handleError 0 = return ()
handleError e
  | e < 0     = throw $ UnknownError $ fromIntegral e
  | otherwise = throw $ Error $ toEnum $ fromIntegral e

throw :: Exception -> ALPM a
throw = E.throwError

catch :: ALPM a -> ALPM (Either Exception a)
catch action = E.catchError (action >>= return . Right) (return . Left)

-- | This function needs to be called first or nothing else will work.
initialize :: String -> String -> ALPM Handle
initialize root dbdir = do
  (err,hdl) <- liftIO init' 
  handleError err
  return hdl
 where init' :: IO (CInt,Handle)
       init' = withCString dbdir $ \ dbdir' ->
        withCString root $
          \root' -> do 
             perr <-  malloc :: IO (Ptr CInt)
             hdl <- {# call initialize #} root' dbdir' perr
             err <- peek perr
             return (err,hdl)

-- | Call this function to clean up. After this the library is no longer
-- available
release :: Handle -> ALPM ()
release handle = do
    e <- liftIO $ {# call release #} handle
    handleError e

