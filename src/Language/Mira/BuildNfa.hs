
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

module Language.Mira.BuildNfa where

import Language.Mira.RegExp
import Language.Mira.NfaToDfa
import qualified Data.Set as Set
import Language.Mira.NfaTypes
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Set (singleton, union)

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
	        (Set.fromList [0 .. 1])
	        (singleton (Emove 0 1))
	        0
	        (singleton 1)

build (Literal c)
	      = NFA
	        (Set.fromList [0 .. 1])
	        (singleton (Move 0 c 1))
	        0
	        (singleton 1)

build (Or r1 r2) = m_or (build r1) (build r2)

build (And r1 r2) = m_and (build r1) (build r2)

build (Then r1 r2) = m_then (build r1) (build r2)

build (Star r) = m_star (build r)

build (Not r) = m_not (build r)

-------------------------------------------------------------------------- 
--									--
--	Combinators for machines, called by build.			--
--									--
-------------------------------------------------------------------------- 

m_or :: Nfa Int -> Nfa Int -> Nfa Int

m_or (NFA states1 moves1 _start1 _finish1) (NFA states2 moves2 _start2 _finish2)

  = NFA
    (states1' `union` states2' `union` newstates)
    (moves1' `union` moves2' `union` newmoves)
    0
    (singleton (m1+m2+1))
    where
    m1 = Set.size states1
    m2 = Set.size states2
    states1' = Set.map (renumber 1) states1
    states2' = Set.map (renumber (m1+1)) states2
    newstates = Set.fromList [0,(m1+m2+1)]
    moves1'  = Set.map (renumber_move 1) moves1
    moves2'  = Set.map (renumber_move (m1+1)) moves2
    newmoves = Set.fromList [ Emove 0 1 , Emove 0 (m1+1) ,
                       Emove m1 (m1+m2+1) , Emove (m1+m2) (m1+m2+1) ]

m_and :: Nfa Int -> Nfa Int -> Nfa Int

m_and n1 n2
      = NFA
      (Set.fromList [0..((length cross_list)-1)])
      (Set.fromList [Move (indexOf (s1,s2) cross_list) a1 (indexOf (sn, sm) cross_list) | Move s1 a1 sn <- moves1',  Move s2 a2 sm <- moves2', a1 == a2])
      start
      (Set.fromList [indexOf (f1,f2) cross_list | f1 <- finish1', f2 <- finish2'])
      
      where
      n1' = make_deterministic n1
      n2' = make_deterministic n2
      states1' = (Set.toList (states n1'))
      states2' = (Set.toList (states n2'))
      cross_list = [(s1, s2) | s1 <- states1', s2 <- states2' ]
      moves1' = (Set.toList (moves n1'))
      moves2' = (Set.toList (moves n2'))
      start =  indexOf (startstate n1', startstate n2') cross_list
      finish1' = (Set.toList (finalstates n1'))
      finish2' = (Set.toList (finalstates n2'))

m_then :: Nfa Int -> Nfa Int -> Nfa Int

m_then (NFA states1 moves1 start1 _finish1) (NFA states2 moves2 _start2 finish2)

      = NFA
        (union states1 states2')
        (union moves1 moves2')
	start1
	finish2'

	where

	states2' = Set.map (renumber k) states2
	moves2'  = Set.map (renumber_move k) moves2
	finish2' = Set.map (renumber k) finish2
	k = Set.size states1 - 1

m_star :: Nfa Int -> Nfa Int 

m_star (NFA myStates myMoves _start _finish)
  = NFA
    (states' `union` newstates)
    (moves' `union` newmoves)
    0
    (singleton (m+1))
    where
    m = Set.size myStates
    states' = Set.map (renumber 1) myStates
    newstates = Set.fromList [ 0 , m+1 ]
    moves'  = Set.map (renumber_move 1) myMoves
    newmoves = Set.fromList [ Emove 0 1 , Emove m 1 , Emove 0 (m+1) , Emove m (m+1) ]

m_not :: Nfa Int -> Nfa Int

m_not (NFA states moves start finish)
      = NFA states moves start (Set.difference states finish)
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

indexOf :: Eq a => a -> [a] -> Int

indexOf x
      = fromMaybe (error "indexOf") . elemIndex x
