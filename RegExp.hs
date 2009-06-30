
-------------------------------------------------------------------------- 
--									--
--	RegExp.hs							--
--									--
--	A type of regular expressions.					--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--									--
-------------------------------------------------------------------------- 

module RegExp where

data Reg = Epsilon |
	   Literal Char |
	   Or Reg Reg |
	   Then Reg Reg |
	   Star Reg
           deriving Eq

-------------------------------------------------------------------------- 
--	Definitions of ? and +						--
-------------------------------------------------------------------------- 

opt,plus :: Reg -> Reg

opt re = Or re Epsilon

plus re = Then (Star re) re

-------------------------------------------------------------------------- 
--	Expanding a character range into a regular expression.		--
--									--
--	range 'a' 'c'							--
--	  = Or (Literal 'a') (Or (Literal 'b') (Literal 'c'))		--
-------------------------------------------------------------------------- 

rangeChar :: Char -> Char -> Reg

rangeChar c1 c2
      = foldr1 Or (map Literal [c1 .. c2])

-------------------------------------------------------------------------- 
--	Examples							--
-------------------------------------------------------------------------- 

a = Literal 'a'
b = Literal 'b'

rex1 = a `Or` (a `Then` b)
rex2 = (a `Then` b) `Or` (Epsilon `Or` (Star a))

regexp0 = Then b (Then (Star regexp2) a)
regexp1 = Then a (Then (Star regexp2) b)
regexp2 = Or (Then a b) (Then b a)

-------------------------------------------------------------------------- 
--	Which literals occur in a regular expression?			--
-------------------------------------------------------------------------- 
 
literals :: Reg -> [Char]

literals Epsilon      = []
literals (Literal ch) = [ch]
literals (Or r1 r2)   = literals r1 ++ literals r2
literals (Then r1 r2) = literals r1 ++ literals r2
literals (Star r)     = literals r

-------------------------------------------------------------------------- 
--	Pretty printing a regular expression.				--
--									--
--	@ is used instead for the epsilon character.			--
-------------------------------------------------------------------------- 

instance Show Reg where
  show = printRE

printRE :: Reg -> [Char]

printRE Epsilon = "@"
printRE (Literal ch) = [ch]
printRE (Or r1 r2) = "(" ++ printRE r1 ++ "|" ++ printRE r2 ++ ")"
printRE (Then r1 r2) = "(" ++ printRE r1 ++ printRE r2 ++ ")"
printRE (Star r) = "(" ++ printRE r ++")*"
