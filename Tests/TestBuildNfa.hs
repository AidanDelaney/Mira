--------------------------------------------------------------------------
--                                                                      --
--        TestBuildNfa.hs                                               --
--                                                                      --
--        Test the implementation of code that builds Nfa               --
--                                                                      --
--        (c) 2009 Aidan Delaney <a.j.delaney@brighton.ac.uk>           --
--                                                                      --
--------------------------------------------------------------------------

module Tests.TestBuildNfa
where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import NfaTypes
import BuildNfa (m_or, m_and)
import Sets

sigma_star_nfa = 
	       NFA (makeSet [0]) (makeSet [Move 0 'a' 0, Move 0 'b' 0]) 0 (makeSet [0])

a_plus_nfa = 
	   NFA (makeSet [0,1]) (makeSet [Move 0 'a' 1, Move 1 'a' 1]) 0 (makeSet [1])

a_plus_emove_nfa = 
		 NFA (makeSet [0,1]) (makeSet [Move 0 'a' 1, Move 1 'a' 1]) 0 (makeSet [1])

simple_or_result =
		 NFA (makeSet [0..4]) (makeSet [Emove 0 1, Move 1 'a' 1, Move 1 'b' 1, Emove 1 4, Emove 0 2, Move 2 'a' 3, Move 3 'a' 3, Emove 3 4 ]) 0 (makeSet [4])

simple_and_result =
		 NFA (makeSet [0,1]) (makeSet [Move 0 'a' 1, Move 1 'a' 1]) 0 (makeSet [1])

emove_and_result =
		 NFA (makeSet [0,1]) (makeSet [Move 0 'a' 1, Move 1 'a' 1]) 0 (makeSet [1])

test_simple_nfa_or = 
	       assertEqual "" simple_or_result (m_or sigma_star_nfa  a_plus_nfa)

test_simple_nfa_and = 
	       assertEqual "" simple_and_result (m_and sigma_star_nfa  a_plus_nfa)

test_emove_nfa_and = 
		   assertEqual "" emove_and_result (m_and a_plus_nfa a_plus_emove_nfa)

suite = testGroup "basic HUnit tests"
  [ testCase "Test simple NFA Or" test_simple_nfa_or
  , testCase "Test simple NFA And" test_simple_nfa_and
  , testCase "Test And deals with Emove" test_emove_nfa_and
  ]
