
-------------------------------------------------------------------------- 
--									--
--	BuildNfa.hs							--
--									--
--	Building an NFA to recognise a regular expression.		--
--									--
--	Regular expressions are defined in regexp, and the type of	--
--	NFAs in nfa_types. The implementation of sets used is in	--
--	sets. 								--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--									--
-------------------------------------------------------------------------- 

module BuildNfa where

import RegExp
import Sets
import NfaTypes

-------------------------------------------------------------------------- 
--									--
--	Here we build NFAs of type (Nfa Int) to recognise regular	--
--	expressions.							--
--									--
--	We define a series of conbinators for these numeric NFAs, so 	--
--	as to build the results by recursion on the structure of the	--
--	regular expression, an object of type regexp.			--
--									--
-------------------------------------------------------------------------- 


build :: Reg -> Nfa Int

build Epsilon = NFA
	        (makeSet [0 .. 1])
	        (sing (Emove 0 1))
	        0
	        (sing 1)

build (Literal c)
	      = NFA
	        (makeSet [0 .. 1])
	        (sing (Move 0 c 1))
	        0
	        (sing 1)

build (Or r1 r2) = m_or (build r1) (build r2)

build (Then r1 r2) = m_then (build r1) (build r2)

build (Star r) = m_star (build r)

-------------------------------------------------------------------------- 
--									--
--	Combinators for machines, called by build.			--
--									--
-------------------------------------------------------------------------- 

m_or :: Nfa Int -> Nfa Int -> Nfa Int

m_or (NFA states1 moves1 start1 finish1) (NFA states2 moves2 start2 finish2)

  = NFA
    (states1' `union` states2' `union` newstates)
    (moves1' `union` moves2' `union` newmoves)
    0
    (sing (m1+m2+1))
    where
    m1 = card states1
    m2 = card states2
    states1' = mapSet (renumber 1) states1
    states2' = mapSet (renumber (m1+1)) states2
    newstates = makeSet [0,(m1+m2+1)]
    moves1'  = mapSet (renumber_move 1) moves1
    moves2'  = mapSet (renumber_move (m1+1)) moves2
    newmoves = makeSet [ Emove 0 1 , Emove 0 (m1+1) ,
                       Emove m1 (m1+m2+1) , Emove (m1+m2) (m1+m2+1) ]

m_then :: Nfa Int -> Nfa Int -> Nfa Int

m_then (NFA states1 moves1 start1 finish1) (NFA states2 moves2 start2 finish2)

      = NFA
        (union states1 states2')
        (union moves1 moves2')
	start1
	finish2'

	where

	states2' = mapSet (renumber k) states2
	moves2'  = mapSet (renumber_move k) moves2
	finish2' = mapSet (renumber k) finish2
	k = card states1 - 1

m_star :: Nfa Int -> Nfa Int 

m_star (NFA states moves start finish)
  = NFA
    (states' `union` newstates)
    (moves' `union` newmoves)
    0
    (sing (m+1))
    where
    m = card states
    states' = mapSet (renumber 1) states
    newstates = makeSet [ 0 , m+1 ]
    moves'  = mapSet (renumber_move 1) moves
    newmoves = makeSet [ Emove 0 1 , Emove m 1 , Emove 0 (m+1) , Emove m (m+1) ]

-------------------------------------------------------------------------- 
--	Auxilliary functions used in the definition of NFAs from	--
--	regular expressions.						--
-------------------------------------------------------------------------- 

renumber :: Int -> Int -> Int

renumber n st = n + st

renumber_move :: Int -> Move Int -> Move Int

renumber_move k (Move s1 c s2)
      = Move (renumber k s1) c (renumber k s2)

renumber_move k (Emove s1 s2)
      = Emove (renumber k s1) (renumber k s2)

