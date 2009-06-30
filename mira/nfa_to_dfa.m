||----------------------------------------------------------------------|| 
||									||
||	nfa_to_dfa.m							||
||									||
||			NFA to DFA					||
||									||
||	Regular expressions are defined in regexp, and the type of	||
||	NFAs in nfa_types. The implementation of sets used is in	||
||	sets.								||
||	nfa_lib contains functions used here and in the implementation:	||
||	closure, startstate etc.					||
||									||
||	(c) Simon Thompson, 1995					||
||									||
||----------------------------------------------------------------------|| 

%include "regexp"
%include "sets"
%include "nfa_types"
%include "nfa_lib"

||----------------------------------------------------------------------|| 
||									||
||	Conversion of an nfa produced by build (above) to a dfa.	||
||	Note that a ``dead'' state is produced by the process as	||
||	implemented here.						||
||									||
||	First make a (nfa (set num)) using make_deter, and then 	||
||	convert to a numeric dfa by means of the numbering function	||
||	number.								||
||									||
||----------------------------------------------------------------------|| 

make_deterministic :: nfa num -> nfa num

make_deterministic = number . make_deter

||----------------------------------------------------------------------|| 
||									||
||	number will make an (nfa num) from an nfa (set num).		||
||									||
||	Extract a list of the states of the machine (statelist),	||
||	then replace each state by its position in the statelist, 	||
||	given by the function change. These replacements are performed	||
||	by means of the mapset operation.				||
||									||
||----------------------------------------------------------------------|| 

number :: nfa (set num) -> nfa num

number (NFA states moves start finish)
  = NFA states' moves' start' finish'
    where
    statelist = flatten states
    lookup l a = look 0 l a
    look n [] a = error "lookup"
    look n (b:y) a = n			, if b=a
                   = look (n+1) y a 	, otherwise
    change = lookup statelist
    states' = mapset change states
    moves'  = mapset newmove moves
              where
              newmove (Move s c t) = Move (change s) c (change t) 
              newmove (Emove s t)  = Emove (change s) (change t)
    start' = change start
    finish' = mapset change finish

||----------------------------------------------------------------------|| 
||									||
||	make_deter calls the crucial function 				||
||		deterministic						||
||	on a machine and its alphabet.					||
||									||
||----------------------------------------------------------------------|| 

make_deter :: nfa num -> nfa (set num)

make_deter mach = deterministic mach (alphabet mach)

||----------------------------------------------------------------------|| 
||									||
||	deterministic mach alpha					||
||									||
||	is the result of forming the dfa based on sets of states of	||
||	mach, using the alphabet alpha.					||
||									||
||	Calculated by taking the limit of the function addstep		||
||	which adds all the ststes accessible by one transition on	||
||	one of the characters of the alphabet.				||
||	The starting machine has one (start) state, the closure of the	||
||	start state of mach. Note that this may be terminal - test	||
||	for this by taking an intersection of this state set with	||
||	the set term of terminal states of mach.			||
||									||
||----------------------------------------------------------------------|| 

deterministic :: nfa num -> [char] -> nfa (set num)

deterministic mach alpha 
    = nfa_limit (addstep mach alpha) startmach
      where
      startmach = NFA 
                  (sing starter)
                  empty
                  starter
                  finish
      starter = closure mach (sing start)
      finish  = empty		, if eqset (term $inter starter) empty
              = sing starter	, otherwise
      (NFA sts mvs start term) = mach

||----------------------------------------------------------------------|| 
||									||
||	Addstep adds all the new states which can be made by a single	||
||	transition on one of the characters of the alphabet.		||
||									||
||----------------------------------------------------------------------|| 

addstep :: nfa num -> [char] -> nfa (set num) -> nfa (set num)

addstep mach alpha dfa
  = add_aux mach alpha dfa (flatten states)
    where
    (NFA states m s f) = dfa
    add_aux mach alpha dfa [] = dfa
    add_aux mach alpha dfa (st:rest) 
        = add_aux mach alpha (addmoves mach st alpha dfa) rest

||----------------------------------------------------------------------|| 
||									||
||	addmoves mach x alpha dfa					||
||									||
||	will add to dfa all the moves from state set x over alphabet 	||
||	alpha.								||
||	Defined by iterating addmove.					||
||									||
||----------------------------------------------------------------------|| 

addmoves :: nfa num -> set num -> [char] -> nfa (set num) -> nfa (set num)

addmoves mach x [] dfa    = dfa

addmoves mach x (c:r) dfa = addmoves mach x r (addmove mach x c dfa)

||----------------------------------------------------------------------|| 
||									||
||	addmove mach x c dfa						||
||									||
||	will add to dfa the moves from state set x on character c.	||
||									||
||----------------------------------------------------------------------|| 

addmove :: nfa num -> set num -> char -> nfa (set num) -> nfa (set num)

addmove mach x c (NFA states moves start finish)
  = NFA states' moves' start finish'
    where 
    states' = states $union (sing new)
    moves'  = moves  $union (sing (Move x c new))
    finish' = finish $union (sing new)	  , if ~ (eqset empty (term $inter new))
            = finish       		  , otherwise
    new = onetrans mach c x
    (NFA s m q term) = mach

||----------------------------------------------------------------------|| 
||									||
||	Finding the limit of an nfa transforming function. Just like	||
||	limit except for the change of equality test.			||
||									||
||----------------------------------------------------------------------|| 

nfa_limit :: (nfa * -> nfa *) -> nfa * -> nfa *

nfa_limit f n = n		 , if nfa_eq n next
	      = nfa_limit f next , otherwise
                where
	        next = f n
		nfa_eq (NFA s1 n1 st1 f1) (NFA s2 n2 st2 f2)
		  = eqset s1 s2 & eqset n1 n2 & st1=st2 & eqset f1 f2

