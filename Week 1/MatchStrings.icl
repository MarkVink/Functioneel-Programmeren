implementation module MatchStrings

import StdEnv

head				:: String -> Char
head s				= s.[0]


tail				:: String -> String
tail s				= s % (1, size s)


is_gelijk						:: String String -> Bool
is_gelijk a b	
| size a == 0 && size b == 0 	= True
| head a <> head b				= False
| otherwise						= is_gelijk (tail a) (tail b)


is_deelstring				:: String String -> Bool
is_deelstring a b
| size b < size a  		 	= False
| a == b % (0, (size a)-1)	= True
| otherwise					= is_deelstring a (tail b)


is_deel						:: String String -> Bool
is_deel a b			
| size a == 0				= True
| size b == 0				= False
| head a == head b			= is_deel (tail a) (tail b)
| otherwise					= is_deel a (tail b)


remove_until				:: String String -> String
remove_until a b
| size b == 0 				= "*"
| head a == '*'				= remove_until (tail a) b
| head a == '.'				= remove_until (tail a) b
| a % (0, 1) <> b % (0, 1)  = remove_until a (tail b)
| otherwise					= b

is_match					:: String String -> Bool
is_match a b
| size a == 0				= True
| head b == '*'				= True
| head a == head b			= is_match (tail a) (tail b)
| head a == '.'				= is_match (tail a) (tail b)
| head a == '*'				= is_match (tail a) (remove_until b a)
| otherwise					= False


//Start 				= remove_until "*.here*.here*." "Is there anybody in there?"

//Start				= (head pink_floyd, tail pink_floyd)
//Start				= is_gelijk     "abc" "abc"
//Start				= is_deelstring "there"          pink_floyd
//Start				= is_deelstring "there"          marillion
//Start				= is_deel       "there"          marillion
//Start				= is_deel       "she and her"    pink_floyd
//Start				= is_deel       radiohead        pink_floyd
Start				= is_match      "*.here*.here*." pink_floyd
//Start				= is_match      "there.here."    pink_floyd
//Start				= is_match      "t.st"    "testssssshhdh"

pink_floyd			= "Is there anybody in there?"
marillion			= "Just for the record"
radiohead			= "There there"
