implementation module BinSearchTree

import StdEnv
import BinTree

z0 = Leaf
z1 = insertTree 50 z0
z2 = insertTree 10 z1
z3 = insertTree 75 z2
z4 = insertTree 80 z3
z5 = insertTree 77 z4
z6 = insertTree 10 z5
z7 = insertTree 75 z6
z8 = deleteTree 50 z7

//  Uit het diktaat, blz. 73:
insertTree :: a (Tree a) -> Tree a | Ord a
insertTree e Leaf = Node e Leaf Leaf
insertTree e (Node x le ri)
| e <= x = Node x (insertTree e le) ri
| e >  x = Node x le (insertTree e ri)

deleteTree :: a (Tree a) -> (Tree a) | Eq, Ord a
deleteTree e Leaf = Leaf
deleteTree e (Node x le ri)
| e <  x = Node x (deleteTree e le) ri
| e == x = join le ri
| e >  x = Node x le (deleteTree e ri)
where
	join :: (Tree a) (Tree a) -> (Tree a)
	join Leaf b2 = b2
	join b1 b2 = Node x b1` b2
	where
		(x,b1`) = largest b1
		
		largest :: (Tree a) -> (a,(Tree a))
		largest (Node x b1 Leaf) = (x,b1)
		largest (Node x b1 b2)   = (y,Node x b1 b2`)
		where
			(y,b2`) = largest b2

is_geordend :: (Tree a) -> Bool | Ord a
is_geordend Leaf 	 		= True
is_geordend (Node x l r) 	= lcheck x l && rcheck x r
where
	lcheck :: a (Tree a) -> Bool | Ord a
	lcheck x Leaf 	= True
	lcheck x (Node x2 l r)
	| x2 > x 		= False
	| otherwise 	= is_geordend l && is_geordend r
	rcheck :: a (Tree a) -> Bool | Ord a
	rcheck x Leaf 	= True
	rcheck x (Node x2 l r)
	| x2 < x 		= False
	| otherwise		= is_geordend r && is_geordend l

// T7 is volgens ons True ipv False, omdat (1<=4 && 5>=4) && 2<=5	
Start = map is_geordend [t0,t1,t2,t3,t4,t5,t6,t7]

is_gebalanceerd :: (Tree a) -> Bool
is_gebalanceerd Leaf = True
is_gebalanceerd (Node x l r)
| abs (diepte l - diepte r) > 1	= False
| otherwise = is_gebalanceerd l && is_gebalanceerd r

//Start = map is_gebalanceerd [t0,t1,t2,t3,t4,t5,t6,t7]
