||----------------------------------------------------------------------|| 
||									||
||	nfa_misc.m							||
||									||
||	Misecllaneous definitions for the NFA system. Includes 		||
||	examples of machines, and functions to print an NFA and the	||
||	equivalence classes produces by minimisation.			||
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
||	Examples							||
||									||
||----------------------------------------------------------------------|| 


machM     = NFA
	    (makeset [0..3])
	    (makeset [ Move 0 'a' 0 ,
		       Move 0 'a' 1 ,
		       Move 0 'b' 0 ,
		       Move 1 'b' 2 ,
		       Move 2 'b' 3 ] )
	    0
	    (sing 3)

machN    = NFA
	    (makeset [0..5])
	    (makeset [ Move 0 'a' 1 ,
		       Move 1 'b' 2 ,
		       Move 0 'a' 3 ,
		       Move 3 'b' 4 ,
		       Emove 3 4 ,
		       Move 4 'b' 5 ] )
	    0
	    (makeset [2,5])

||----------------------------------------------------------------------|| 
||									||
||	Printing an NFA.						||
||									||
||----------------------------------------------------------------------|| 

print_nfa :: nfa num -> [char]

print_nfa (NFA states moves start finish)
      = "States:\t" ++ show_states (flatten states) ++ "\n\n" ++
	"Moves:\n" ++ (concat (map print_move (flatten moves))) ++ "\n\n" ++
	"Start:\t" ++ show start ++ "\n\n" ++
	"Finish:\t" ++ show_states (flatten finish) ++ "\n"

show_states :: [num] -> [char]

show_states = concat . (map ((++" ") . show))

print_move :: move num -> [char]

print_move (Move s1 c s2) = "\t" ++ show s1 ++ "\t" ++ [c] ++ "\t"
			    ++ show s2 ++ "\n"

print_move (Emove s1 s2) = "\t" ++ show s1 ++ "\t@\t" ++ show s2 ++ "\n"

||----------------------------------------------------------------------|| 
||									||
||	Printing a set of equivalence classes.				||
||									||
||----------------------------------------------------------------------|| 

print_classes :: set (set num) -> [char]

print_classes ss = pcs (map flatten (flatten ss))
		   where
		   pcs = concat . map pc 
		   pc  = (++"\n") .  concat . (map ((++"\t").show_num))

show_num :: num -> [char]

show_num = show

