                                                                             
        Sets.lhs	

        ADT of sets, implemented as ordered lists without repetitions.							
 
        (c) Simon Thompson, 1995, 1998, 2000.

	Documented in Haskell: The Craft of Functional Programming						
       

>       module Sets ( Set ,
>         empty              , -- Set a
>         sing               , -- a -> Set a
>         memSet             , -- Ord a => Set a -> a -> Bool
>         union,inter,diff   , -- Ord a => Set a -> Set a -> Set a
>         eqSet              , -- Eq a  => Set a -> Set a -> Bool
>         subSet             , -- Ord a => Set a -> Set a -> Bool
>         makeSet            , -- Ord a => [a] -> Set a
>         mapSet             , -- Ord b => (a -> b) -> Set a -> Set b
>         filterSet          , -- (a -> Bool) -> Set a -> Set a
>         foldSet            , -- (a -> a -> a) -> a -> Set a -> a
>         showSet            , -- Show a => Set a -> String
>         card               , -- Set a -> Int
>         flatten            , -- Set a -> [a]
>         setlimit             -- Eq a => (Set a -> Set a) -> Set a -> Set a
>         ) where

>	import List hiding ( union )
 
Instance declarations for Eq and Ord					

>       instance Eq a => Eq (Set a) where
>         (==) = eqSet

>       instance Ord a => Ord (Set a) where
>         s1 <= s2 = flatten s1 <= flatten s2

>	instance Show a => Show (Set a) where
>	  show = showSet

The implementation.						
				
>       newtype Set a = SetI [a]

>       empty               :: Set a
>       empty  = SetI []

>       sing                :: a -> Set a
>       sing x = SetI [x]

>       memSet              :: Ord a => Set a -> a -> Bool
>       memSet (SetI []) y    = False
>       memSet (SetI (x:xs)) y 
>         | x<y		= memSet (SetI xs) y
>         | x==y 	= True
>         | otherwise 	= False

>       union    :: Ord a => Set a -> Set a -> Set a
>	union (SetI xs) (SetI ys) = SetI (uni xs ys)

>       uni    :: Ord a => [a] -> [a] -> [a]
>       uni [] ys        = ys
>       uni xs []        = xs
>       uni (x:xs) (y:ys) 
>         | x<y 	= x : uni xs (y:ys)
>         | x==y 	= x : uni xs ys
>         | otherwise 	= y : uni (x:xs) ys

>       inter    :: Ord a => Set a -> Set a -> Set a
>	inter (SetI xs) (SetI ys) = SetI (int xs ys)

>       int    :: Ord a => [a] -> [a] -> [a]
>       int [] ys = []
>       int xs [] = []
>       int (x:xs) (y:ys) 
>         | x<y 	= int xs (y:ys)
>         | x==y 	= x : int xs ys
>         | otherwise 	= int (x:xs) ys

>       diff    :: Ord a => Set a -> Set a -> Set a
>       diff (SetI xs) (SetI ys) = SetI (dif xs ys)

>       dif    :: Ord a => [a] -> [a] -> [a]
>       dif [] ys = []
>       dif xs [] = xs
>       dif (x:xs) (y:ys)  
>         | x<y 	= x : dif xs (y:ys)
>         | x==y 	= dif xs ys
>         | otherwise 	= dif (x:xs) ys

>       subSet        :: Ord a => Set a -> Set a -> Bool
>       subSet (SetI xs) (SetI ys) = subS xs ys

>       subS        :: Ord a => [a] -> [a] -> Bool
>       subS [] ys = True
>       subS xs [] = False
>       subS (x:xs) (y:ys) 
>         | x<y 	= False
>         | x==y 	= subS xs ys
>         | x>y 	= subS (x:xs) ys

>       eqSet        :: Eq a => Set a -> Set a -> Bool
>       eqSet (SetI xs) (SetI ys) = (xs == ys)
       	
>       makeSet             :: Ord a => [a] -> Set a
>       makeSet = SetI . remDups . sort
>                 where
>                 remDups []     = []
>                 remDups [x]    = [x]
>                 remDups (x:y:xs) 
>       	    | x < y 	= x : remDups (y:xs)
>                   | otherwise = remDups (y:xs)

>       mapSet              :: Ord b => (a -> b) -> Set a -> Set b
>       mapSet f (SetI xs) = makeSet (map f xs)

>       filterSet           :: (a -> Bool) -> Set a -> Set a
>       filterSet p (SetI xs) = SetI (filter p xs)

>       foldSet             :: (a -> a -> a) -> a -> Set a -> a
>       foldSet f x (SetI xs)  = (foldr f x xs)

>       showSet             :: Show a => Set a -> String
>       showSet (SetI xs) = concat (map ((++"\n") . show) xs)

>       card                :: Set a -> Int
>       card (SetI xs)     = length xs

>       flatten :: Set a -> [a]
>	flatten (SetI xs) = xs

>	setlimit :: Eq a => (Set a -> Set a) -> Set a -> Set a
>	setlimit f s
>	  | s==next	= s
>	  | otherwise   = setlimit f next
>	    where
>	    next = f s


