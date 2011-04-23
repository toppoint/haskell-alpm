{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Util
    ( ALPMTest
    )
where

import qualified Control.Monad.Error as E
import Control.Monad.Trans
import Test.HUnit
import Test.HUnit.Lang

import Distribution.ArchLinux.ALPM

newtype ALPMTest a = ALPMTest (E.ErrorT Exception IO a)
  deriving (Monad, MonadIO, E.MonadError Exception)

instance E.Error Exception where
    noMsg = UnknownError 0
    strMsg _ = UnknownError 0

instance MonadALPM ALPMTest where
    run (ALPMTest action) = E.runErrorT action
    throw = E.throwError
    catch action = E.catchError (action >>= return . Right) (return . Left)

