||----------------------------------------------------------------------|| 
||									||
||	implement_nfa.m							||
||									||
||	Implementing an NFA.						||
||									||
||	Regular expressions are defined in regexp, and the type of	||
||	NFAs in nfa_types. The implementation of sets used is in	||
||	sets. The file nfa_lib contains functions used by both		||
||	this file and the file nfa_to_dfa which converts to a 		||
||	deterministic machine.						||
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
||	Trans runs down a string, applying onetrans repeatedly for the	||
||	characters in the string, starting from the start state.	||
||	The result is therefore the set of all states accessible from	||
||	the start state by accepting the items in the string; the	||
||	result can be the empty set, of course.				||
||									||
||----------------------------------------------------------------------|| 

trans :: nfa * -> string -> set *

trans mach str = foldl step startset str
		 where
		 step set ch = onetrans mach ch set
		 startset = closure mach (sing (startstate mach))

||----------------------------------------------------------------------|| 
||	Thanks are due to Sven Martin for pointing out the omission 	||
||	of the closure in the definition of startset.			||
||----------------------------------------------------------------------|| 

||----------------------------------------------------------------------|| 
||	Turn the result of trans into printable form.			||
||----------------------------------------------------------------------|| 

print_trans :: nfa num -> string -> [char]

print_trans mach str = show (flatten (trans mach str))

