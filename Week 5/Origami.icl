module Origami

import StdEnv

sum` :: [Int] -> Int
sum` x 				= foldr (+) 0 x

prod` :: [Int] -> Int
prod` x 			= foldr (*) 1 x

flatten` :: [[a]] -> [a]
flatten` x 			= foldr (++) [] x

length` :: [a] -> Int
length` x			= foldr (\_ n = n + 1) 0 x

reverse` :: [a] -> [a]
reverse` a 			= foldl (\xs x = [x] ++ xs) [] a

takeWhile` :: (a -> Bool) [a] -> [a]
takeWhile` a b		= foldr (\x xs = if (a x) [x:xs] []) [] b

maxList` :: [a] -> a | Ord a
maxList` x			= foldr max (hd x) x

Start			= and
				  [ sum`       [1 .. 5]                 == sum       [1 .. 5]
				  , prod`      [1 .. 5]                 == prod      [1 .. 5]
				  , flatten`   [[],[1],[1,2],[1,2,3]]   == flatten   [[],[1],[1,2],[1,2,3]]
				  , length`	   [1 .. 5]					== length	 [1 .. 5]
				  , reverse`   [1 .. 5]                 == reverse   [1 .. 5]
				  , takeWhile` ((<>) 0) [1,2,3,0,4,5,6] == takeWhile ((<>) 0) [1,2,3,0,4,5,6]
				  , maxList`   [1 .. 5]                 == maxList   [1 .. 5]
				  ]