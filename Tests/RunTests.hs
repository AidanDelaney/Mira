module RunTests
where

import Test.HUnit
import Test.QuickCheck
import Tests.TestBuildNfa as TestBuildNfa
import Tests.TestMinimiseDfa as TestMinimiseDfa

main = do
     runTestTT TestBuildNfa.suite
     quickCheck TestMinimiseDfa.prop_minimiseAccept
