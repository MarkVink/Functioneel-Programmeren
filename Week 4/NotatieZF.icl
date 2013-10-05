module NotatieZF

import StdEnv

/*
g1 :: // meest algemene type
g1 as bs = [(a,b) \\ a <- as, b <- bs]

g2 :: // meest algemene type
g2 as bs = [(a,b) \\ a <- as & b <- bs]

g3 :: // meest algemene type
g3 as bs = [(a,b) \\ a <- as, b <- bs | a <> b]

g4 :: // meest algemene type
g4 as bs = [a \\ a <- as, b <- bs | a == b]

g5 :: // meest algemene type
g5 xss = [x \\ xs <- xss, x <- xs]

g6 :: // meest algemene type
g6 a xs = [i \\ i <- [0 ..] & x <- xs | a == x]
*/

g1 :: [x] [y] -> [(x,y)]//[1..2] [4..6] = [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6)]
g1 as bs = [(a,b) \\ a <- as, b <- bs]

g2 :: [x] [y] -> [(x,y)]//[1..2] [4..6] = [(1,4),(2,5)]
g2 as bs = [(a,b) \\ a <- as & b <- bs]

g3 :: [a] [a] -> [(a,a)] | Eq a//[1..2] [1..2] = [(1,2),(2,1)]
g3 as bs = [(a,b) \\ a <- as, b <- bs | a <> b]

g4 :: [a] [a] -> [a] | Eq a //[1..2] [1..2] = [1,2]
g4 as bs = [a \\ a <- as, b <- bs | a == b]

g5 :: [[a]] -> [a] //[[1..2],[1..2]] = [1,2,1,2] 
g5 xss = [x \\ xs <- xss, x <- xs]

g6 :: a [a] -> [Int] | Eq a //[1,5,2,5] = [1,3]
g6 a xs = [i \\ i <- [0 ..] & x <- xs | a == x]

Start = g6 2 [1..5]