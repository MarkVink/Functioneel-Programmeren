module IKS

import StdEnv
import StdMaybe
import StdDynamic, StdDynamicFileIO

/**	Een interpreter voor IKS.
*/
//	1. Creeer de dynamics op disk
/*Start :: *World -> *World
Start world 
# (_,world) = writeDynamic "I" (dynamic i :: A. a: a -> a) world
# (_,world) = writeDynamic "K" (dynamic i :: A. a: a -> a) world
# (_,world) = writeDynamic "S" (dynamic i :: A. a: a -> a) world
= world*/

i :: a -> a
i x = x

k :: a b -> a
k x y = x

s :: (a -> b -> c) (a -> b) a -> c
s x y z = x z (y z)


//	2. Parseren van IKS expressies
::	IKS = I | K | S | N Int | App IKS IKS

pIKS :: [Char] -> Maybe IKS
pIKS []							= Nothing
pIKS x							= Just (pIKS` x)
where 
	pIKS` :: [Char] -> IKS
	pIKS` ['I']					= I
	pIKS` ['I' : xs]			= App I (pIKS` xs)
	pIKS` ['K']					= K
	pIKS` ['K' : xs]			= App K (pIKS` xs)
	pIKS` ['S']					= S
	pIKS` ['S' : xs]			= App S (pIKS` xs)
	pIKS` x
	#i							= read x
	| i > 0 && i == length x	= N (getal x)
	| i > 0						= App (pIKS` (take i x)) (pIKS` (drop i x))
	#(a,b)						= split_bracket x
	| a == []					= pIKS` b
	| otherwise					= App (pIKS` a) (pIKS` b)
	read :: [Char] -> Int
	read []						= 0
	read [x : xs]	
	| is_getal x				= 1 + (read xs)
	| otherwise					= 0
	where
		is_getal :: Char -> Bool
		is_getal '0'			= True
		is_getal '1'			= True	
		is_getal '2'			= True
		is_getal '3'			= True
		is_getal '4'			= True
		is_getal '5'			= True
		is_getal '6'			= True
		is_getal '7'			= True
		is_getal '8'			= True
		is_getal '9'			= True
		is_getal _ 				= False

split_bracket :: [Char] -> ([Char],[Char])
split_bracket []	= ([],[])
split_bracket x		
#valid 				= valid_bracket x
| valid == False	= abort "Geen geldige invoer."
#part				= part_bracket x			
= (clean_bracket (take ((length x) - (length part)) x), clean_bracket (reverse part))
where
	valid_bracket :: [Char] -> Bool
	valid_bracket x = 		((count_bracket x 0) == 0)
	where 
		count_bracket :: [Char] Int -> Int
		count_bracket [] i 	= i
		count_bracket x i 
		| i < 0				= -1 // dit resulteerd in False
		| last x == ')'		= count_bracket (init x) (i+1) 
		| last x == '('		= count_bracket (init x) (i-1) 
		| otherwise			= count_bracket (init x) i
	part_bracket :: [Char] -> [Char]
	part_bracket []		= []
	part_bracket x 
	| last x == ')'		= [last x] ++ (in_bracket (init x) 1)
	| otherwise			= out_bracket x
	where 
		in_bracket :: [Char] Int -> [Char]
		in_bracket [] _		= []
		in_bracket x i
		| i == 0			= []
		| last x == ')'		= [last x] ++ (in_bracket (init x) (i+1))
		| last x == '('		= [last x] ++ (in_bracket (init x) (i-1))
		| otherwise 		= [last x] ++ (in_bracket (init x) i)
		out_bracket	:: [Char] -> [Char]
		out_bracket []		= []
		out_bracket x
		| last x == ')'		= []
		| last x == '('		= []
		| otherwise 		= [last x] ++ (out_bracket (init x))
	clean_bracket :: [Char] -> [Char]
	clean_bracket []			= []
	clean_bracket x
	| (length x) > 1	
		&& (hd x) == '(' 
		&& (last x) == ')'		= clean_bracket (tl (init x))
	| otherwise					= x

getal :: [Char] -> Int
getal chars = toInt (toString chars)


//	3. Interpreteren van IKS expressies
//interp :: (Dynamic,Dynamic,Dynamic) IKS -> Dynamic
//interp ...

dynApply :: Dynamic Dynamic -> Dynamic
dynApply (f :: a -> b) (x :: a) = dynamic f x :: b
dynApply _ _ = dynamic "dynamic?type?error"

Start = pIKS ['I','K','(','4','(','(','2',')',')','I','8','4',')']

/*
//	4. console
Start :: *World -> *World
Start ...
*/
