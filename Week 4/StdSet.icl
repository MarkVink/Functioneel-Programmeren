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

instance <   (Set a) | Eq a     
where (<) a b 			= SetLength a < SetLength b	

instance == (Set a) | Eq a
where (==) a b = SetLength (a diff b) == 0

SetLength :: (Set a) -> Int
SetLength (Set []) = 0
SetLength (Set a) = length a


(\/) infixl 6 :: (Set a) (Set a) -> Set a | Eq a
(\/) (Set []) (Set [])			= zero
(\/) (Set []) b 				= b
(\/) a (Set []) 				= a
(\/) (Set a) (Set b)	= toSet [c \\ c <- a ++ b]

(/\) infixl 7 :: (Set a) (Set a) -> Set a | Eq a
(/\) (Set []) (Set [])			= zero
(/\) (Set []) b 				= zero
(/\) a (Set []) 				= zero
(/\) (Set a) b	= toSet [c \\ c <- a | memberOfSet c b]

(diff) infix :: (Set a) (Set a) -> Set a | Eq a
(diff) (Set []) (Set [])		= zero
(diff) a b 			= toSet [c \\ c <- fromSet (a \/ b) | memberOfSet c (a /\ b) == False]


product	:: (Set a) (Set b) -> Set (a,b)
product (Set a) (Set b)			= Set [(x,y) \\ x <- a, y <- b]

powerSet		:: (Set a)         -> Set (Set a)
powerSet (Set a)				= Set [Set [x,y] \\ x <- a, y <- a]
