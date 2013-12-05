implementation module RandomGetallen

import StdEnv, Random

/*
Start			:: *World -> ([Int],*World)
Start world
# (rs,world)	= getNewRandomSeed world
= (shuffle [1..10] rs,world)
*/

random_n		:: Int RandomSeed -> ([Int],RandomSeed)
random_n n rs	= seqList (repeatn n random) rs

random_inf		:: RandomSeed -> [Int]
random_inf rs	= iterateSt random rs

iterateSt		:: (s -> (a,s)) s -> [a]
iterateSt f x	= let (a,x1) = f x in [a : iterateSt f x1]

// point-wise stijl:
shuffle			:: [a] RandomSeed -> [a]
shuffle xs rs	= fst (unzip (sortBy (\(_,r1) (_,r2) -> r1 < r2) (zip2 xs (random_inf rs))))

// point-free stijl:
shuffle2		:: [a] -> RandomSeed -> [a]
shuffle2 xs		= fst o unzip o (sortBy (\(_,r1) (_,r2) -> r1 < r2)) o (zip2 xs) o random_inf
