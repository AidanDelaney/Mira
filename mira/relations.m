||----------------------------------------------------------------------|| 
||									||
||	relations.m							||
||									||
||	(c) Simon Thompson, 1995					||
||									||
||----------------------------------------------------------------------|| 

%include "sets"

||----------------------------------------------------------------------|| 
||	Relations are sets of pairs.					||
||----------------------------------------------------------------------|| 

relation * == set (*,*)

||----------------------------------------------------------------------|| 
||	Finite relations can be seen as graphs.				||
||----------------------------------------------------------------------|| 

graph1,graph2 :: relation num

graph1 = makeset [ (1,2) , (1,3) , (3,2) , (3,4) , (4,2) , (2,4) ]

graph2 = sing (4,3) $union graph1

||----------------------------------------------------------------------|| 
||	The elements -- union of domain and range -- of a relation	||
||----------------------------------------------------------------------|| 

elems :: relation * -> set *

elems g = mapset fst g $union mapset snd g

||----------------------------------------------------------------------|| 
||	Relational join							||
||----------------------------------------------------------------------|| 

join :: relation * -> relation * -> relation *

join rel1 rel2 = makeset [ (a,d) | (a,b) <- flatten rel1 ;
				   (c,d) <- flatten rel2 ;
				   b=c ]

||----------------------------------------------------------------------|| 
||	Pair up each element of a set with a given element.		||
||	This operation is mapped to give product.			||
||----------------------------------------------------------------------|| 

adjoin :: set * -> ** -> set (*,**)

adjoin st el = mapset (addel el) st
	       where
	       addel el el' = (el',el)

||----------------------------------------------------------------------|| 
||	The Cartesian product of two sets -- map the operation of	||
||	adjoining an element along then second set, and take the	||
||	union of the resulting set of sets.				||
||----------------------------------------------------------------------|| 

setProduct :: set * -> set ** -> set (*,**)

setProduct st1 st2 = unionSet (mapset (adjoin st1) st2)

||----------------------------------------------------------------------|| 
||	The union of a set of sets -- fold in the binary union 		||
||	operation.							||
||----------------------------------------------------------------------|| 

unionSet :: set (set *) -> set *

unionSet = foldset union empty

||----------------------------------------------------------------------|| 
||	Join defined using only set operations: the operations of	||
||	mapset, filterset and setProduct are enough to give a set-	||
||	level simulation of the list comprehension notation above.	||
||----------------------------------------------------------------------|| 

newjoin :: relation * -> relation * -> relation *

newjoin rel1 rel2
  =  mapset outer (filterset equals (setProduct rel1 rel2))
     where
     equals ((a,b),(c,d)) = (b=c)
     outer  ((a,b),(c,d)) = (a,d)

||----------------------------------------------------------------------|| 
||	The identity relation on a particular set of elements.		||
||----------------------------------------------------------------------|| 

idrel elems = mapset double elems
	      where
	      double a = (a,a)

||----------------------------------------------------------------------|| 
||	The reflexive transitive closure of a relation.			||
||----------------------------------------------------------------------|| 

closure rel = setlimit (addjoin rel) (idrel (elems rel))
	      where
	      addjoin rel rel' = rel' $union join rel rel'

||----------------------------------------------------------------------|| 
||	Inverting a relation.						||
||----------------------------------------------------------------------|| 

invert :: relation * -> relation *

invert = mapset swap
	 where
	 swap (a,b) = (b,a)

||----------------------------------------------------------------------|| 
||	Strongly connected components					||
||----------------------------------------------------------------------|| 	

strong :: relation * -> relation *

strong rel = clos $inter (invert clos)
	     where
	     clos = closure rel

||----------------------------------------------------------------------|| 
||	Image of a value under a relation.				||
||----------------------------------------------------------------------|| 

image :: relation * -> * -> set *

image rel val = mapset snd (filterset (fstIs val) rel)
		where
		fstIs val (a,b) = (a=val)

||----------------------------------------------------------------------|| 
||	Image of a set under a relation					||
||----------------------------------------------------------------------|| 

setImage :: relation * -> set * -> set *

setImage rel = unionSet . mapset (image rel) 

||----------------------------------------------------------------------|| 	
||	Add the image of a set to the set itself.			||
||----------------------------------------------------------------------|| 

addImage :: relation * -> set * -> set *

addImage rel st = st $union setImage rel st

||----------------------------------------------------------------------|| 
||	Add images to a set of sets.					||
||----------------------------------------------------------------------|| 

addImages :: relation * -> set (set *) -> set (set *)

addImages rel sts = mapset (addImage rel) sts

||----------------------------------------------------------------------|| 
||	The set of equivalence classes for an equivalence relation.	||
||----------------------------------------------------------------------|| 

classes :: relation * -> set (set *)

classes rel = setlimit (addImages rel) start
	      where
	      start = mapset sing (elems rel)
