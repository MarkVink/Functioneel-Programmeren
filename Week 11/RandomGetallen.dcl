definition module RandomGetallen

import Random

random_n	:: Int RandomSeed -> ([Int],RandomSeed)
random_inf	:: RandomSeed ->  [Int]
iterateSt	:: (s -> (a,s)) s -> [a]
shuffle		:: [a] RandomSeed ->  [a]
shuffle2	:: [a] -> RandomSeed -> [a]

