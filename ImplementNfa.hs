
-------------------------------------------------------------------------- 
--									--
--	ImplementNfa.hs							--
--									--
--	Implementing an NFA.						--
--									--
--	Regular expressions are defined in regexp, and the type of	--
--	NFAs in NfaTypes. The implementation of Sets used is in	the	--
--	sets module. The module NfaLib contains functions used by both	--
--	this module and the module NfaToDfa which converts to a 	--
--	deterministic machine.						--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--									--
-------------------------------------------------------------------------- 

module ImplementNfa where

import qualified Data.Set as Set
import Data.Set ( Set, singleton, intersection, empty )

import RegExp
import NfaTypes
import NfaLib


-------------------------------------------------------------------------- 
--									--
--	Trans runs down a string, applying onetrans repeatedly for the	--
--	characters in the string, starting from the start state.	--
--	The result is therefore the set of all states accessible from	--
--	the start state by accepting the items in the string; the	--
--	result can be the empty set, of course.				--
--									--
-------------------------------------------------------------------------- 

trans :: Ord a => Nfa a -> String -> Set a

trans mach str = foldl step startset str
		 where
		 step set ch = onetrans mach ch set
		 startset = closure mach (singleton (startstate mach))

-- | 'accepts' @mach str@ is @True@ if the automaton @mach@ accepts the
--   string @str@
accepts :: Ord a => Nfa a -> String -> Bool
accepts mach str = not (empty == (trans mach str `intersection` finalstates mach))

-------------------------------------------------------------------------- 
--	Thanks are due to Sven Martin for pointing out the omission 	--
--	of the closure in the definition of startset.			--
-------------------------------------------------------------------------- 

-------------------------------------------------------------------------- 
--	Turn the result of trans into printable form.			--
-------------------------------------------------------------------------- 

print_trans :: Nfa Int -> String -> [Char]

print_trans mach str = show (Set.toList (trans mach str))


