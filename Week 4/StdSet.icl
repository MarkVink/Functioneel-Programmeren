implementation module StdSet

import StdEnv

::	Set a = Set [a] 

toSet			:: [a]             -> Set a | Eq a
toSet [] 				= (Set [])
toSet a 				= (Set [hd a : fromSet(toSet [ta \\ ta <- tl a | ta <> hd a ])])

fromSet			:: (Set a)         -> [a]
fromSet (Set [])		= []
fromSet (Set a)			= a

isEmptySet		:: (Set a)         -> Bool
isEmptySet (Set [])		= True
isEmptySet (Set a)		= False

isDisjoint		:: (Set a) (Set a) -> Bool  | Eq a
isDisjoint (Set []) b	= True
isDisjoint (Set [a:as]) b
| memberOfSet a b		= False
| otherwise 			= isDisjoint (Set as) b

memberOfSet		:: a       (Set a) -> Bool  | Eq a
memberOfSet	a (Set [b:bs])
| a == b				= True
| otherwise				= memberOfSet a (Set bs)
memberOfSet	a (Set [])	= False

instance zero (Set a)
where zero = (Set [])

//instance zero (a,b)   | zero a & zero b          
//where zero = (zero,zero)

//SetEqual		:: (Set a) (Set a)	-> Bool	| Eq a
//SetEqual a b	= True
//SetEqual (Set [a:as]) (Set [b:bs])
//| memberOfSet a (Set [b:bs]) && memberOfSet b (Set [a:as]) = SetEqual (Set as) (Set bs)
//| otherwise					= False


//| memberOfSet a (Set bs)	= memberOfSet a (Set bs)
//| otherwise 				= False

SetEqual :: (Set a) (Set a) -> Bool | Eq a
SetEqual (Set []) (Set []) 			= True
SetEqual (Set []) b 				= False
SetEqual a (Set []) 				= False
SetEqual (Set [a:as]) (Set [b:bs])
| memberOfSet a (Set [b:bs]) && memberOfSet b (Set [a:as]) = SetEqual (Set as) (Set bs)
| otherwise 						= False

Start = SetEqual (Set [2,1]) (Set [1,2])

//instance ==   (Set a) | Eq a     
//where 
//	(==) x y 			= isDisjoint x y
	
/*
func :: a -> a
.
.
where
	f1 :: a -> a
	f1 a = a +1
where
	f2 :: a -> a
	f2 a = f1 a
*/

//instance +    (a,b)   | + a & + b                
//where (+) (x0,y0) (x1,y1) = (x0 + x1,y0 + y1)

//Start = (Set [1,2]) == (Set [1,2])

//Start 		= (Set [1]) == (Set [2,1])

//Start 	= isDisjoint (Set [1,2,3,4]) (Set [4,5,6])

//Start = memberOfSet 5 (toSet [1,2,2,3])
//Start = isEmptySet (Set [1,2])
