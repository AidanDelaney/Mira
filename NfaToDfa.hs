
-------------------------------------------------------------------------- 
--									--
--	NfaToDfa.hs							--
--									--
--			NFA to DFA					--
--									--
--	Regular expressions are defined in RegExp, and the type of	--
--	NFAs in NfaTypes. The implementation of sets used is in		--
--	Sets.								--
--	NfaLib contains functions used here and in the implementation:	--
--	closure, startstate etc.					--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--									--
-------------------------------------------------------------------------- 

module NfaToDfa where

import RegExp
import Sets
import NfaTypes
import NfaLib

-------------------------------------------------------------------------- 
--									--
--	Conversion of an nfa produced by build (above) to a dfa.	--
--	Note that a ``dead'' state is produced by the process as	--
--	implemented here.						--
--									--
--	First make a (nfa (set num)) using make_deter, and then 	--
--	convert to a numeric dfa by means of the numbering function	--
--	number.								--
--									--
-------------------------------------------------------------------------- 

make_deterministic :: Nfa Int -> Nfa Int

make_deterministic = number . make_deter

-------------------------------------------------------------------------- 
--									--
--	number will make an (Nfa Int) from an Nfa (Set Int).		--
--									--
--	Extract a list of the states of the machine (statelist),	--
--	then replace each state by its position in the statelist, 	--
--	given by the function change. These replacements are performed	--
--	by means of the mapset operation.				--
--									--
-------------------------------------------------------------------------- 

number :: Nfa (Set Int) -> Nfa Int

number (NFA states moves start finish)
  = NFA states' moves' start' finish'
    where
    statelist = flatten states
    lookup l a = look 0 l a
    look n [] a = error "lookup"
    look n (b:y) a 
      | (b==a)      = n			
      | otherwise   = look (n+1) y a 	
    change = lookup statelist
    states' = mapSet change states
    moves'  = mapSet newmove moves
              where
              newmove (Move s c t) = Move (change s) c (change t) 
              newmove (Emove s t)  = Emove (change s) (change t)
    start' = change start
    finish' = mapSet change finish

-------------------------------------------------------------------------- 
--									--
--	make_deter calls the crucial function 				--
--		deterministic						--
--	on a machine and its alphabet.					--
--									--
-------------------------------------------------------------------------- 

make_deter :: Nfa Int -> Nfa (Set Int)

make_deter mach = deterministic mach (alphabet mach)

-------------------------------------------------------------------------- 
--									--
--	deterministic mach alpha					--
--									--
--	is the result of forming the dfa based on sets of states of	--
--	mach, using the alphabet alpha.					--
--									--
--	Calculated by taking the limit of the function addstep		--
--	which adds all the ststes accessible by one transition on	--
--	one of the characters of the alphabet.				--
--	The starting machine has one (start) state, the closure of the	--
--	start state of mach. Note that this may be terminal - test	--
--	for this by taking an intersection of this state set with	--
--	the set term of terminal states of mach.			--
--									--
-------------------------------------------------------------------------- 

deterministic :: Nfa Int -> [Char] -> Nfa (Set Int)

deterministic mach alpha 
    = nfa_limit (addstep mach alpha) startmach
      where
      startmach = NFA 
                  (sing starter)
                  empty
                  starter
                  finish
      starter = closure mach (sing start)
      finish  
        | (term `inter` starter) == empty     = empty		
        | otherwise                           = sing starter	
      (NFA sts mvs start term) = mach

-------------------------------------------------------------------------- 
--									--
--	Addstep adds all the new states which can be made by a single	--
--	transition on one of the characters of the alphabet.		--
--									--
-------------------------------------------------------------------------- 

addstep :: Nfa Int -> [Char] -> Nfa (Set Int) -> Nfa (Set Int)

addstep mach alpha dfa
  = add_aux mach alpha dfa (flatten states)
    where
    (NFA states m s f) = dfa
    add_aux mach alpha dfa [] = dfa
    add_aux mach alpha dfa (st:rest) 
        = add_aux mach alpha (addmoves mach st alpha dfa) rest

-------------------------------------------------------------------------- 
--									--
--	addmoves mach x alpha dfa					--
--									--
--	will add to dfa all the moves from state set x over alphabet 	--
--	alpha.								--
--	Defined by iterating addmove.					--
--									--
-------------------------------------------------------------------------- 

addmoves :: Nfa Int -> Set Int -> [Char] -> Nfa (Set Int) -> Nfa (Set Int)

addmoves mach x [] dfa    = dfa

addmoves mach x (c:r) dfa = addmoves mach x r (addmove mach x c dfa)

-------------------------------------------------------------------------- 
--									--
--	addmove mach x c dfa						--
--									--
--	will add to dfa the moves from state set x on character c.	--
--									--
-------------------------------------------------------------------------- 

addmove :: Nfa Int -> Set Int -> Char -> Nfa (Set Int) -> Nfa (Set Int)

addmove mach x c (NFA states moves start finish)
  = NFA states' moves' start finish'
    where 
    states' = states `union` (sing new)
    moves'  = moves  `union` (sing (Move x c new))
    finish' 
     | empty /= (term `inter` new)    = finish `union` (sing new)	
     | otherwise                      = finish       		
    new = onetrans mach c x
    (NFA s m q term) = mach

-------------------------------------------------------------------------- 
--									--
--	Finding the limit of an nfa transforming function. Just like	--
--	limit except for the change of equality test.			--
--									--
-------------------------------------------------------------------------- 

nfa_limit :: Eq a => (Nfa a -> Nfa a) -> Nfa a -> Nfa a

nfa_limit f n 
  | (nfa_eq n next) = n		
  | otherwise       = nfa_limit f next
                where
	        next = f n
		nfa_eq (NFA s1 n1 st1 f1) (NFA s2 n2 st2 f2)
		  = s1 == s2 && n1 == n2 && st1 == st2 && f1 == f2

