module Main (main) where

import System.Exit
import Test.Framework

import qualified Core
import qualified List

tests :: TestSuite
tests = makeAnonTestSuite $ map testSuiteAsTest
    [ Core.tests
    , List.tests
    ]

main :: IO ()
main = do
    ec <- runTest tests
    exitWith ec

