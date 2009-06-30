||----------------------------------------------------------------------|| 
||									||
||	nfa_types.m							||
||									||
||	The type of NFAs, defined using the set library of sets.	||
||									||
||	(c) Simon Thompson, 1995					||
||									||
||----------------------------------------------------------------------|| 

||----------------------------------------------------------------------|| 
||									||
||	Type definitions.						||
||									||
||	States are of arbitrary type, though we will usually represent	||
||	them as numbers, or as sets of numbers, when we form a dfa	||
||	from an nfa, for example.					||
||									||
||	Strings are lists of characters.				||
||									||
||	Moves are either on a character (Move) or are epsilon moves	||
||	(Emove).							||
||									||
||	An NFA has four components					||
||		the set of its states					||
||		the set of its moves					||
||		the start state						||
||		the set of terminal states				|\
||									||
||----------------------------------------------------------------------|| 


%include "sets"

||	string == [char] 	moved to regexp.m

move * ::= Move * char * | Emove * *

nfa * ::= NFA (set *) (set (move *)) * (set *)

