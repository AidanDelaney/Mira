||----------------------------------------------------------------------|| 
||									||
||	nfa_lib.m							||
||									||
||	Useful functions used in the implementation of an NFA and	||
||	the conversion of an NFA to a DFA.				||
||	Therefore used in implement_nfa and in nfa_to_dfa.		||
||									||
||	(c) Simon Thompson, 1995					||
||									||
||----------------------------------------------------------------------|| 

%include "regexp"
%include "sets"
%include "nfa_types"

||----------------------------------------------------------------------|| 
||									||
||	The epsilon closure of a set of states in an NFA. This is 	||
||	found by finding the limit of the function which adds to a	||
||	set all those states accessible by a single epsilon move.	||
||	The limit is found using setlimit.				||
||									||
||----------------------------------------------------------------------|| 

closure :: nfa * -> set * -> set *

closure (NFA states moves start term)
      = setlimit add
	where
	add stateset = union stateset (makeset accessible)
		       where
		       accessible
			 = [ s | x <- flatten stateset ; 
				 Emove y s <- flatten moves ;
				 y=x ]

||----------------------------------------------------------------------|| 
||									||
||	Onemove finds the set of states accessible from a given set	||
||	by a single move on the given character.			||
||									||
||----------------------------------------------------------------------|| 

onemove :: nfa * -> char -> set * -> set *

onemove (NFA states moves start term) c x
      = makeset [ s | t <- flatten x ; 
		      Move z d s <- flatten moves ;
		      z=t ; c=d ]

||----------------------------------------------------------------------|| 
||									||
||	Onetrans performs one move (by onemove) and then takes the	||
||	epsilon closure of the result.					||
||									||
||----------------------------------------------------------------------|| 

onetrans :: nfa * -> char -> set * -> set *

onetrans mach c x = closure mach (onemove mach c x)

||----------------------------------------------------------------------|| 
||									||
||	Auxilliary functions.						||
||									||
||	startstate	extracts the start state of a machine.		||
||									||
||	alphabet 	returns the alphabet of the machine, by 	||
||			finding a list of the characters mentioned	||
||			in the Moves.					||
||									||
||----------------------------------------------------------------------|| 

startstate :: nfa * -> *

startstate (NFA states moves start finish) = start

alphabet :: nfa * -> [char]

alphabet (NFA s moves st f)
  = mkset [ c | Move s c t <- flatten moves ]

