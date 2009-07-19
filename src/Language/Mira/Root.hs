-------------------------------------------------------------------------- 
--									--
--	Root.hs								--
--									--
--	(c) Simon Thompson, 2002					--
--									--
-------------------------------------------------------------------------- 

module Root where

rootStep :: Int -> Int -> Int

rootStep n app = (app + n `div` app) `div` 2

rootCalc :: Int -> [Int]

rootCalc n
  = rootList 1
    where
    rootList x 
      | next == x	= [x]
      | otherwise	= x : rootList next
	where
	next = rootStep n x

root n
  | n<=1	= n
  | otherwise
    = rootConverge start start
    where
    start = n `div` 2
    rootConverge v1 v2
     | v2==next 		= v2
     | v1<v2 && v1==next	= v1
     | otherwise		= rootConverge v2 next
       where
       next = rootStep n v2

