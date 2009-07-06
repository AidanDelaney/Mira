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

suite = testGroup "DFA minimisation"
          [ testProperty "minimised automaton accepts same strings"  prop_minimiseAccept ]

prop_minimiseAccept :: Nfa Int -> String -> Bool
prop_minimiseAccept nfa str = trans (minimise nfa) str == trans nfa str
