
-------------------------------------------------------------------------- 
--									--
--	Generate.hs							--
--									--
--	(c) Simon Thompson, 2002					--
--									--
-------------------------------------------------------------------------- 

module Generate where

import RegExp
import Root(root)	-- this gives the integer square root

generate :: Reg -> [String]

generate Epsilon = [ "" ]

generate (Literal ch) = [ [ch] ]

generate (Or r1 r2) 
  = interleave (generate r1) (generate r2)

generate (Then r1 r2) 
  = [ st1 ++ st2 | (st1,st2) <- cross (generate r1) (generate r2) ]

generate (Star r)
  = (generate Epsilon) ++ generate (Then r (Star r))

-- Auxiliary functions

interleave :: [a] -> [a] -> [a]

interleave [] ys     = ys
interleave (x:xs) ys = x : interleave ys xs

cross :: [a] -> [b] -> [(a,b)]

cross [] ys = []
cross (x:xs) ys 
  = interleave [ (x,y) | y<-ys ] (cross xs ys)

-- crossAlt assumes that the two lists are
-- infinite

crossAlt :: [a] -> [b] -> [(a,b)]

crossAlt xs ys = map (find.indexPair) [0 .. ]
		 where
		 find (n,m) = (xs!!n,ys!!m)
		 indexPair p = norm srt (srt,p-sqr)
			       where 
			       srt = root p
			       sqr = srt^2
		 norm x (a,b) 
		  | diff>0	= (a-diff,x)
		  | otherwise   = (a,b)
		    where
		    diff = b-x
