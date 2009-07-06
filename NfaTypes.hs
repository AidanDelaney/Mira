
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

import qualified Data.Set as Set
import Data.Set ( Set, union )

import Control.Monad ( ap, replicateM, filterM )
import Test.QuickCheck (Arbitrary(..), sized, vector, oneof, elements)

data Move a = Move a Char a | Emove a a
	      deriving (Eq,Ord,Show)

data Nfa a  = NFA { states :: Set a
                  , moves :: Set (Move a)
                  , startstate :: a
                  , finalstates :: Set a
                  }
	      deriving (Eq,Show)

instance (Ord a, Arbitrary a) => Arbitrary (Nfa a) where
 arbitrary = sized aut
  where
   aut 0  =
     do start  <- arbitrary
        return $ NFA (Set.fromList []) (Set.fromList []) start (Set.fromList [])
   aut msz =
     do ssz <- elements [1..msz] -- seems hokey
        fsz <- elements [1..msz] -- seems hokey
        --
        states  <- vector ssz
        mvs     <- replicateM msz $ move states
        start   <- elements states
        final   <- take fsz `fmap` sublist states -- subset
        return $ NFA (Set.fromList states) (Set.fromList mvs) start (Set.fromList final)
   move sts = oneof [ Move  `fmap` elements sts `ap` arbitrary `ap` elements sts
                    , Emove `fmap` elements sts `ap` elements sts ]
   sublist = filterM (const arbitrary)
