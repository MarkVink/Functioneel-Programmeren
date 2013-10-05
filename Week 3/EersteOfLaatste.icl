implementation module EersteOfLaatste

import StdEnv

//	1.
eerste2 :: [a] -> [a]
eerste2 [] = abort "Lijst is leeg."
eerste2 [a] = abort "Lijst is te kort."
eerste2 [a, b] = [a, b]
eerste2 [a, b : xs] = [a, b]

//Start = eerste2 [1,2,3,4,5]

laatste2 :: [a] -> [a]
laatste2 [] = abort "Lijst is leeg."
laatste2 [a] = abort "Lijst is te kort."
laatste2 [a, b] = [a, b]
laatste2 [a : xs] = laatste2 xs

//Start = laatste2 [1,2,3,4,5]

//	2.
//  Reduceer de volgende Start-regels met de hand:
//Start = hd (hd (hd [[[1,2,3],[4]],[[5],[6]]])) //`1 
//Start = hd (tl [1,2,3,4,5]) // 2
//Start = eerste2 [[1],[],[2,3],[4,5,6]] // [1],[]
//Start = laatste2 [[1],[],[2,3],[4,5,6]] //[2,3],[4,5,6]

//	3.
eersten :: Int [a] -> [a]
eersten i [] = abort "Lijst is leeg"
eersten i [a : as]
| (length [a : as]) < i = abort "Lijst is te kort"
| (length [a : as]) == i = [a : as]
| otherwise = eersten 
//| (length [a : as]) == i = [a : as]
//| otherwise			= i notlast as


//Start = eersten 1 [1,2,3]

laatsten :: Int [a] -> [a]
laatsten i [] = abort "Lijst is leeg"
laatsten i [a : as]
| (length [a : as]) < i  = abort "Lijst is te kort"
| (length [a : as]) == i = [a : as]
| otherwise				 = laatsten i as 

Start = laatsten 5 [1,2]

//	4.
//  Maak de volgende beweringen af:
/*
Voor alle 0 <= n, xs :: [a] : eersten n (eersten  n xs) = 
Voor alle 0 <= n, xs :: [a] : eersten n (laatsten n xs) = 
Voor alle 0 <= n, xs :: [a] : laatsten n (eersten n xs) = 
Voor alle 0 <= n, xs :: [a] : laatsten n (laatsten n xs) = 
Voor alle 0 <= m <= n, xs :: [a] : eersten m (eersten n xs) = 
Voor alle 0 <= m <= n, xs :: [a] : length (eersten m xs) ? length (eersten n xs)
*/
