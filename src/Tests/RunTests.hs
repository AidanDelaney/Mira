module Main
where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Test.HUnit
import Test.QuickCheck

import Language.Mira.TestBuildNfa as TestBuildNfa
import Language.Mira.TestMinimiseDfa as TestMinimiseDfa

main = defaultMain tests

tests = [ TestBuildNfa.suite
        , TestMinimiseDfa.suite
        ]
