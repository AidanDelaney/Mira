module Main
where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck

import Test.HUnit
import Test.QuickCheck

import Tests.TestBuildNfa as TestBuildNfa
import Tests.TestMinimiseDfa as TestMinimiseDfa

main = defaultMain tests

tests = [ TestBuildNfa.suite
        , TestMinimiseDfa.suite
        ]
