module RunTests
where

import Test.HUnit
import Tests.TestBuildNfa as TestBuildNfa

main = 
     runTestTT TestBuildNfa.suite