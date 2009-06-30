||----------------------------------------------------------------------|| 
||									||
||	minimise_dfa.m							||
||									||
|| 	Minimising a DFA.						||
||									||
||	Regular expressions are defined in regexp, and the type of	||
||	NFAs in nfa_types. The implementation of sets used is in	||
||	sets.								||
||									||
||	(c) Simon Thompson, 1995					||
||									||
||----------------------------------------------------------------------|| 

%include "regexp"
%include "sets"
%include "nfa_types"

||----------------------------------------------------------------------||
||									||
||	Minimising the nfa - uses the equivalence classes generated	||
||	by the function eqclasses. Replaces each state by the minimum	||
||	state equivalent to it. The set functions clear up repeats etc.	||
||									||
||----------------------------------------------------------------------||

minimise :: nfa * -> nfa *

minimise mach = replace mini mach
	        where
		replace f (NFA states moves start finish)
		  = NFA states' moves' start' finish'
		    where
		    states' = mapset f states
		    moves' = makeset [ Move (f a) c (f b) |
					Move a c b <- flatten moves ]
		    start' = f start
		    finish' = mapset f finish
		mini a = min (flatten (eqclass a))
		eqclass a = hd [ b | b <-flatten classes ; memset b a ]
		(classes,fun) = eqclasses mach

||----------------------------------------------------------------------|| 
||									||
||	Partition takes a binary predicate, represented by a function	||
|| 	of type 							||
||		* -> * -> bool						||
||	assumed to represent an equivalence relation, and a set *,	||
||	and returns the set of the equivalence classes under the	||
||	relation.							||
||									||
||	Implemented using the function part which does the same 	||
||	operation, except that it works over sets.			||
||									||
||----------------------------------------------------------------------|| 

partition :: (* -> * -> bool) -> set * -> set (set *)

partition f s = makeset (map makeset (part f (flatten s)))

||----------------------------------------------------------------------|| 
||									||
||	Partitions a list into a list of equivalence classes (lists)	||
||	by folding in the addtoclass function.				||
||									||
||----------------------------------------------------------------------|| 

part :: (* -> * -> bool) -> [*] -> [[*]]

part f = foldr (addtoclass f) []

||----------------------------------------------------------------------|| 
||									||
||	addtoclass will add an element to the (first) equivalence	||
||	class to which the element belongs, creating a new class if	||
||	necessary.							||
||									||
||----------------------------------------------------------------------|| 

addtoclass :: (* -> * -> bool) -> * -> [[*]] -> [[*]]

addtoclass f a []    = [[a]]

addtoclass f a (c:r) = (a:c):r 			, if f a (hd c)
		     = c : addtoclass f a r	, otherwise

||----------------------------------------------------------------------|| 
||									||
||	Given an nfa will return the set of sets of indistinguishable	||
||	states, from which the minimal DFA can be constructed.		||
||									||
||	This function simply strips off one half of the pair		||
||	returned by eqclasses.						||
||									||
||----------------------------------------------------------------------|| 

eqivclasses :: nfa * -> set (set *) 

eqivclasses = fst . eqclasses

||----------------------------------------------------------------------|| 
||									||
||	eqclasses returns a pair, which consists of two 		||
||	representations of the partition of the states into indistin-	||
||	guishable classes:						||
||		the set of classes, as sets				||
||		the boolean valued function representing the 		||
||			relation.					||
||									||
||	These are found by iterating the function step which takes one	||
||	such pair to the next, where at the next stage we distinguish	||
||	previously indistinguishable states if and only if a transition	||
||	from them on a particular character gives states distinguished	||
||	by the previous partition.					||
||	Can see from this that we generate the stages simply from the 	||
||	function representation; we carry the set representation so 	||
||	that we can compare two partitions for equality: can't compare	||
||	functions for equality, so compare the set representations.	||
||									||
||	set representations of the partitions are compared by the	||
||	function eqpart, which compares sets of sets for (true) set	||
||	equality.							||
||									||
||	The starting value for the iteration is given by the function	||
||	firstpartfun, which distinguishes the states in finish, i.e.	||
||	the terminal states, from the rest.				||
||									||
||----------------------------------------------------------------------|| 

eqclasses :: nfa * -> ( set (set *) , * -> * -> bool )

eqclasses mach 
	= to_limit step start
	  where

	  start = ( firstpart , firstpartfun )

	  firstpart = partition firstpartfun states
	
	  firstpartfun a b = ( (memset finish a) = (memset finish b) )

	  (NFA states moves startst finish) = mach

	  step ( part , partfun )
		  = ( newpart , newpartfun )
		    where
		    newpart = partition newpartfun states
		    newpartfun a b
		      = and [ partfun c d | (Move a' y c) <- movelist ; a=a' ;
					    (Move b' z d) <- movelist ; b=b' ;
					    y=z ]
			& partfun a b
		    movelist = flatten moves

	  to_limit f (a,b)
	    = (a,b) 		, if eqpart a a'
	    = to_limit f next  	, otherwise
	      where
	      next = f (a,b)
	      (a',b') = next
	  
	  eqpart a a' = and ( flatten (mapset (setmemset a') a) ) & 
			and ( flatten (mapset (setmemset a) a') )

	  setmemset x a = or ( flatten (mapset (eqset a) x) )

