--------------------------------------------------------------------------
--                                                                      --
--        TestMinimiseDfa.hs                                            --
--                                                                      --
--        Test the implementation of MinimiseDfa                        --
--                                                                      --
--        (c) 2009 Eric Kow <E.Y.Kow@brighton.ac.uk>                    --
--                                                                      --
--------------------------------------------------------------------------

module Tests.TestMinimiseDfa
where

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import NfaTypes
import MinimiseDfa
import ImplementNfa
import NfaToDfa

suite = testGroup "DFA minimisation"
          [ testProperty "minimised automaton accepts same strings"  prop_minimiseAccept,
	  testProperty "a make_deterministic and an nfa accept the same strings" prop_deterministicAccept,
	  testProperty "minimise is deterministic"  prop_minimiseDeterministic ]

prop_minimiseAccept :: Nfa Int -> String -> Bool
prop_minimiseAccept nfa str = accepts ((minimise . make_deterministic) nfa) str == accepts nfa str

prop_deterministicAccept :: Nfa Int -> String -> Bool
prop_deterministicAccept nfa str = accepts (make_deterministic nfa) str == accepts nfa str

prop_minimiseDeterministic :: Nfa Int -> Bool
prop_minimiseDeterministic nfa = (minimise . minimise) nfa == minimise nfa
