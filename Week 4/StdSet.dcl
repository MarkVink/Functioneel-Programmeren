definition module StdSet

import StdClass

//:: Set a :== [a]
::	Set a = Set [a] 

toSet			:: [a]             -> Set a | Eq a
fromSet			:: (Set a)         -> [a]

isEmptySet		:: (Set a)         -> Bool
isDisjoint		:: (Set a) (Set a) -> Bool  | Eq a
memberOfSet		:: a       (Set a) -> Bool  | Eq a

instance zero (Set a)
//instance <    (Set a) | Eq a
//instance ==   (Set a) | Eq a
//instance length Set

//(\/)   infixl 6	:: (Set a) (Set a) -> Set a | Eq a
//(/\)   infixl 7	:: (Set a) (Set a) -> Set a | Eq a
//(diff) infix	:: (Set a) (Set a) -> Set a | Eq a
//product			:: (Set a) (Set b) -> Set (a,b)

//powerSet		:: (Set a)         -> Set (Set a)

SetEqual :: (Set a) (Set a) -> Bool | Eq a
