
||----------------------------------------------------------------------|| 
||									||
||	sets.m								||
||									||
||	Library of set manipulating functions. Sets are implemented as	||
||	ordered lists without repetitions.				||
||									||
||	(c) Simon Thompson, 1995					||
||									||
||----------------------------------------------------------------------|| 

abstype 	set *
with		
		sing 		 :: * -> set *
		union,inter,diff :: set * -> set * -> set *
		empty 		 :: set *
		memset 		 :: set * -> * -> bool
		subset 		 :: set * -> set * -> bool
		eqset 		 :: set * -> set * -> bool
		mapset 		 :: (* -> **) -> set * -> set **
		filterset,separate :: (*->bool) -> set * -> set *
		foldset 	 :: (* -> * -> *) -> * -> set * -> *
		makeset 	 :: [*] -> set *
		showset 	 :: (*->[char]) -> set * -> [char]
		card		 :: set * -> num
		flatten		 :: set * -> [*]
		setlimit 	 :: (set * -> set *) -> set * -> set *

||----------------------------------------------------------------------|| 
||									||
||	These sets are ordered lists without repetitions.		||
||									||
||----------------------------------------------------------------------|| 

set * == [*]

sing a = [a]

union [] y	  = y
union x []	  = x
union (a:x) (b:y) = a : union x (b:y)	, if a<b
		  = a : union x y 	, if a=b
		  = b : union (a:x) y	, otherwise

inter [] y = []
inter x [] = []
inter (a:x) (b:y) = inter x (b:y)	, if a<b
		  = a : inter x y 	, if a=b
		  = inter (a:x) y	, otherwise

diff [] y = []
diff x [] = x
diff (a:x) (b:y)  = a : diff x (b:y)	, if a<b
		  = diff x y	 	, if a=b
		  = diff (a:x) y	, otherwise

empty = []

memset [] b    = False
memset (a:x) b = memset x b	, if a<b
	       = True		, if a=b
	       = False		, otherwise

subset [] y = True
subset x [] = False
subset (a:x) (b:y) = False		, if a<b
		   = subset x y		, if a=b
		   = subset (a:x) y	, if a>b

||----------------------------------------------------------------------|| 
||	Could use (inter x y = x) as a test for subset.			||
||	Can PROVE the two eqvt. with the def above.			||
||----------------------------------------------------------------------|| 

eqset = (=)

mapset f = makeset . (map f)

filterset = filter

separate  = filterset

foldset = foldr

makeset = remdups . sort
	  where
	  remdups [] = []
	  remdups [a] = [a]
	  remdups (a:x) = a : remdups x , if a < b
			= remdups x	, otherwise
			  where
			  b = hd x

showset f = concat . (map ((++"\n") . f))

card = (#)

flatten = id

||----------------------------------------------------------------------||
||                                                                      ||
||      The limit is as the standard limit function, except that it     ||
||      compares sets for equality using eqset.                         ||
||                                                                      ||
||----------------------------------------------------------------------||

setlimit f x = x                , if eqset x next
	     = setlimit f next  , otherwise
	       where
	       next = f x

