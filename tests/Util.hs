module Util
    ( alpmTest
    )
where

import Test.HUnit
import Test.HUnit.Lang

import Distribution.ArchLinux.ALPM.Core

alpmTest :: IO () -> IO ()
alpmTest tst = do
    ie <- initialize

    case ie of
        Just e ->
            assertFailure $ "initialize: " ++ show e

        Nothing -> do
            tr <- performTestCase tst
            re <- release

            case re of
                Just e ->
                    assertFailure $ "release: " ++ show e

                Nothing ->
                    case tr of
                        Nothing ->
                            -- success
                            assertBool "" True

                        Just (True, msg) ->
                            -- failure
                            assertFailure msg

                        Just (False, msg) ->
                            -- error
                            assertBool msg False

