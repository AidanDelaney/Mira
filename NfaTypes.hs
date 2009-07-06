
-------------------------------------------------------------------------- 
--									--
--	NfaTypes.hs							--
--									--
--	The type of NFAs, defined using the Set library of sets.	--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--									--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--									--
--	Type definitions.						--
--									--
--	States are of arbitrary type, though we will usually represent	--
--	them as numbers, or as sets of numbers, when we form a dfa	--
--	from an nfa, for example.					--
--									--
--	Strings are lists of characters.				--
--									--
--	Moves are either on a character (Move) or are epsilon moves	--
--	(Emove).							--
--									--
--	An NFA has four components					--
--		the set of its states					--
--		the set of its moves					--
--		the start state						--
--		the set of terminal states				--
--									--
-------------------------------------------------------------------------- 

module NfaTypes where

import Control.Monad ( ap, replicateM, filterM )
import Test.QuickCheck (Arbitrary(..), sized, vector, oneof, elements)

import Sets

data Move a = Move a Char a | Emove a a
	      deriving (Eq,Ord,Show)

data Nfa a  = NFA (Set a) (Set (Move a)) a (Set a)
	      deriving (Eq,Show)

instance (Ord a, Arbitrary a) => Arbitrary (Nfa a) where
 arbitrary = sized aut
  where
   aut sz =
     do msz <- arbitrary
        fsz <- elements [1..sz] -- seems hokey
        --
        states  <- vector sz
        mvs     <- replicateM msz $ move states
        start   <- elements states
        final   <- take fsz `fmap` sublist states -- subset
        return $ NFA (makeSet states) (makeSet mvs) start (makeSet final)
   move sts = oneof [ Move  `fmap` elements sts `ap` arbitrary `ap` elements sts
                    , Emove `fmap` elements sts `ap` elements sts ]
   sublist = filterM (const arbitrary)
