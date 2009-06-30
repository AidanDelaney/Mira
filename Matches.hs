
-------------------------------------------------------------------------- 
--									--
--	Matches.hs							--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--									--
-------------------------------------------------------------------------- 

module Matches where

import RegExp

matches :: Reg -> String -> Bool

matches Epsilon st = (st == "")

matches (Literal ch) st = (st == [ch])

matches (Or r1 r2) st
  = matches r1 st || matches r2 st

matches (Then r1 r2) st
  = or [ matches r1 s1 && matches r2 s2 | (s1,s2) <- splits st ]

-------------------------------------------------------------------------- 
--	The definition for (Star r) requires some thought. Can think 	--
--	of (Star r) as Epsilon or (Then r (Star r)), but in the latter 	--
--	case the match with (Then r (Star r)) needs to be one in which	--
--	r matches *non-tivially* at the start of the string; otherwise	--
--	there is a possibility of a recursive call to 			--
--		matches (Star r) st					--
--	a black hole!!							--
--									--
--	matches (Star (Or Epsilon (Or (Literal 'a') (Literal 'b')))) 	--
--	is an example of exactly this.					--
-------------------------------------------------------------------------- 

matches (Star r) st
  = matches Epsilon st || 
      or [ matches r s1 && matches (Star r) s2 | (s1,s2) <- frontSplits st ]

-------------------------------------------------------------------------- 
--	All the ways of splitting a list into two halves.		--
--									--
--	splits [2,3,4] 							--
--	    = [([],[2,3,4]),([2],[3,4]),([2,3],[4]),([2,3,4],[])]	--
-------------------------------------------------------------------------- 

splits :: [a] -> [ ([a],[a]) ]

splits st = [ splitAt n st | n <- [0 .. length st] ]

-------------------------------------------------------------------------- 
--	Splits as above, with the first half non-trivial.		--
-------------------------------------------------------------------------- 

frontSplits :: [a] -> [ ([a],[a]) ]

frontSplits st = [ splitAt n st | n <- [1.. length st] ]

