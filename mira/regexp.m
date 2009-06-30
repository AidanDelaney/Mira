||----------------------------------------------------------------------|| 
||									||
||	regexp.m							||
||									||
||	A type of regular expressions.					||
||									||
||	(c) Simon Thompson, 1995					||
||									||
||----------------------------------------------------------------------|| 

string == [char]

reg ::= Epsilon |
	Literal char |
	Or reg reg |
	Then reg reg |
	Star reg

||----------------------------------------------------------------------|| 
||	Definitions of ? and +						||
||----------------------------------------------------------------------|| 

opt , plus :: reg -> reg

opt re = Or re Epsilon

plus re = Then (Star re) re

||----------------------------------------------------------------------|| 
||	Expanding a character range into a regular expression.		||
||									||
||	range 'a' 'c'							||
||	  = Or (Literal 'a') (Or (Literal 'b') (Literal 'c'))		||
||----------------------------------------------------------------------|| 

range :: char -> char -> reg

range c1 c2
      = foldr1 Or (map Literal (charlist c1 c2))
	where
	charlist c1 c2 = map decode [(code c1)..(code c2)]

||----------------------------------------------------------------------|| 
||	Examples							||
||----------------------------------------------------------------------|| 

a = Literal 'a'
b = Literal 'b'

rex1 = a $Or (a $Then b)
rex2 = (a $Then b) $Or (Epsilon $Or (Star a))

regexp0 = Then b (Then (Star regexp2) a)
regexp1 = Then a (Then (Star regexp2) b)
regexp2 = Or (Then a b) (Then b a)

||----------------------------------------------------------------------|| 
||	Which literals occur in a regular expression?			||
||----------------------------------------------------------------------|| 
 
literals :: reg -> [char]

literals Epsilon = []
literals (Literal ch) = [ch]
literals (Or r1 r2) = literals r1 ++ literals r2
literals (Then r1 r2) = literals r1 ++ literals r2
literals (Star r) = literals r

||----------------------------------------------------------------------|| 
||	Pretty printing a regular expression.				||
||									||
||	@ is used instead for the epsilon character.			||
||----------------------------------------------------------------------|| 

printRE :: reg -> [char]

printRE Epsilon = "@"
printRE (Literal ch) = [ch]
printRE (Or r1 r2) = "(" ++ printRE r1 ++ "|" ++ printRE r2 ++ ")"
printRE (Then r1 r2) = "(" ++ printRE r1 ++ printRE r2 ++ ")"
printRE (Star r) = "(" ++ printRE r ++")*"
