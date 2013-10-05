module LijstTypes

import StdEnv

Start = e5

e1 :: [Int]
e1 = [1]

e2 :: [Bool]
e2 = [True]

e3 :: [[Int]]
e3 = [[1]]

e4 :: [[[Real]]]
e4 = [[[1.1]]]


e5fnc :: Int Int -> Int
e5fnc a b = a + b
e5 :: [Int Int -> Int]
e5 = [e5fnc]
