
-------------------------------------------------------------------------- 
--									--
--	MinimiseDfa.hs							--
--									--
-- 	Minimising a DFA.						--
--									--
--	Regular expressions are defined in RegExp, and the type of	--
--	NFAs in NfaTypes. The implementation of sets used is in		--
--	Sets.								--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--									--
-------------------------------------------------------------------------- 

module MinimiseDfa where

import RegExp
import Sets
import NfaTypes

--------------------------------------------------------------------------
--									--
--	Minimising the nfa - uses the equivalence classes generated	--
--	by the function eqclasses. Replaces each state by the minimum	--
--	state equivalent to it. The set functions clear up repeats etc.	--
--									--
--------------------------------------------------------------------------

minimise :: Ord a => Nfa a -> Nfa a

minimise mach = replace mini mach
	        where
		replace f (NFA states moves start finish)
		  = NFA states' moves' start' finish'
		    where
		    states' = mapSet f states
		    moves' = makeSet [ Move (f a) c (f b) |
					Move a c b <- flatten moves ]
		    start' = f start
		    finish' = mapSet f finish
		mini a = minimum (flatten (eqclass a))
		eqclass a = head [ b | b <-flatten classes , memSet b a ]
		(classes,fun) = eqclasses mach

-------------------------------------------------------------------------- 
--									--
--	Partition takes a binary predicate, represented by a function	--
-- 	of type 							--
--		a -> a -> Bool						--
--	assumed to represent an equivalence relation, and a (Set a),	--
--	and returns the set of the equivalence classes under the	--
--	relation.							--
--									--
--	Implemented using the function part which does the same 	--
--	operation, except that it works over sets.			--
--									--
-------------------------------------------------------------------------- 

partition :: Ord a => (a -> a -> Bool) -> Set a -> Set (Set a)

partition f s = makeSet (map makeSet (part f (flatten s)))

-------------------------------------------------------------------------- 
--									--
--	Partitions a list into a list of equivalence classes (lists)	--
--	by folding in the addtoclass function.				--
--									--
-------------------------------------------------------------------------- 

part :: (a -> a -> Bool) -> [a] -> [[a]]

part f = foldr (addtoclass f) []

-------------------------------------------------------------------------- 
--									--
--	addtoclass will add an element to the (first) equivalence	--
--	class to which the element belongs, creating a new class if	--
--	necessary.							--
--									--
-------------------------------------------------------------------------- 

addtoclass :: (a -> a -> Bool) -> a -> [[a]] -> [[a]]

addtoclass f a []    = [[a]]

addtoclass f a (c:r) 
  | (f a (head c))    = (a:c):r 			
  | otherwise         = c : addtoclass f a r	

-------------------------------------------------------------------------- 
--									--
--	Given an nfa will return the set of sets of indistinguishable	--
--	states, from which the minimal DFA can be constructed.		--
--									--
--	This function simply strips off one half of the pair		--
--	returned by eqclasses.						--
--									--
-------------------------------------------------------------------------- 

eqivclasses :: Ord a => Nfa a -> Set (Set a) 

eqivclasses = fst . eqclasses

-------------------------------------------------------------------------- 
--									--
--	eqclasses returns a pair, which consists of two 		--
--	representations of the partition of the states into indistin-	--
--	guishable classes:						--
--		the set of classes, as sets				--
--		the boolean valued function representing the 		--
--			relation.					--
--									--
--	These are found by iterating the function step which takes one	--
--	such pair to the next, where at the next stage we distinguish	--
--	previously indistinguishable states if and only if a transition	--
--	from them on a particular character gives states distinguished	--
--	by the previous partition.					--
--	Can see from this that we generate the stages simply from the 	--
--	function representation; we carry the set representation so 	--
--	that we can compare two partitions for equality: can't compare	--
--	functions for equality, so compare the set representations.	--
--									--
--	set representations of the partitions are compared by the	--
--	function eqpart, which compares sets of sets for (true) set	--
--	equality.							--
--									--
--	The starting value for the iteration is given by the function	--
--	firstpartfun, which distinguishes the states in finish, i.e.	--
--	the terminal states, from the rest.				--
--									--
-------------------------------------------------------------------------- 

eqclasses :: Ord a => Nfa a -> ( Set (Set a) , a -> a -> Bool )

eqclasses mach 
	= to_limit step start
	  where

	  start = ( firstpart , firstpartfun )

	  firstpart = partition firstpartfun states
	
	  firstpartfun a b = ( (memSet finish a) == (memSet finish b) )

	  (NFA states moves startst finish) = mach

	  step ( part , partfun )
		  = ( newpart , newpartfun )
		    where
		    newpart = partition newpartfun states
		    newpartfun a b
		      = and [ partfun c d | (Move a' y c) <- movelist , a==a' ,
					    (Move b' z d) <- movelist , b==b' ,
					    y==z ]
			&& partfun a b
		    movelist = flatten moves

	  to_limit f (a,b)
	    | (eqpart a a') = (a,b) 		
	    | otherwise = to_limit f next  	
	      where
	      next = f (a,b)
	      (a',b') = next
	  
	  eqpart a a' = and ( flatten (mapSet (setmemSet a') a) ) && 
			and ( flatten (mapSet (setmemSet a) a') )

	  setmemSet x a = or ( flatten (mapSet (eqSet a) x) )

