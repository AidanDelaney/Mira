import Language.Mira.RegExpParser as Parser
import Language.Mira.NfaTypes
import Language.Mira.BuildNfa
import Language.Mira.NfaMisc

import Data.Set as Set

main = 
     do
       putStr (print_nfa  (m_and a1 (m_and a2 a3)))
     where
        a1 = (NFA 
       	    	(Set.fromList [0..3])
		(Set.fromList [Move 0 'a' 1, Move 0 'b' 3, Move 0 'c' 3,
       	   	  		  Move 1 'a' 1, Move 1 'b' 2, Move 1 'c' 3,
				  Move 2 'a' 3, Move 2 'b' 3, Move 2 'c' 3,
				  Move 3 'a' 3, Move 3 'b' 3, Move 3 'c' 3])
		0
		(Set.fromList [0,1,3]))
        a2 = (NFA 
       	    	(Set.fromList [0,1])
       	    	(Set.fromList [Move 0 'a' 0, Move 0 'b' 0, Move 0 'c' 1,
			     Move 1 'a' 1, Move 1 'b' 1, Move 1 'c' 1])
		0
		(Set.fromList [1]))
	a3 = (NFA
		(Set.fromList [0..3])
		(Set.fromList [Move 0 'a' 2, Move 0 'b' 3, Move 0 'c' 3,
       	   	  		  Move 1 'a' 3, Move 1 'b' 2, Move 1 'c' 3,
				  Move 2 'a' 3, Move 2 'b' 3, Move 2 'c' 3,
				  Move 3 'a' 3, Move 3 'b' 3, Move 3 'c' 3])
		0
		(Set.fromList [0,1,3]))