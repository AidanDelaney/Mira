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
import NfaTypes
import MinimiseDfa
import ImplementNfa

prop_minimiseAccept :: Nfa Int -> String -> Bool
prop_minimiseAccept nfa str = trans (minimise nfa) str == trans nfa str
