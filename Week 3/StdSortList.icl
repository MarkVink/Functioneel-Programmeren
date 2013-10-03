implementation module StdSortList

import StdEnv

:: SortList a :== [a]

newSortList   :: SortList a  
newSortList = []

memberSort    		:: a (SortList a) -> Bool       | Eq, Ord a 
memberSort a []		= False
memberSort a [x : xs]
| a == x			= True
| otherwise			= memberSort a xs
 
insertSort    :: a (SortList a) -> SortList a | Ord a  
insertSort a [] 	= [a]
insertSort a [x : xs]
| a < x				= [a] ++ [x : xs]
| otherwise			= [x] ++ insertSort a xs  
 
removeFirst   :: a (SortList a) -> SortList a | Eq, Ord a  
removeFirst a []	= []
removeFirst	a [x : xs]
| a <> x			= [x] ++ removeFirst a xs
| otherwise			= xs

removeAll     :: a (SortList a) -> SortList a | Eq, Ord a  
removeAll a []	= []
removeAll a [x : xs]
| a <> x			= [x] ++ removeAll a xs
| otherwise			= removeAll a xs

elements      ::   (SortList a) -> [a] 
elements x			= x

count         ::   (SortList a) -> Int 
count []			= 0
count [x : xs]		= 1 + (count xs)

minimum       ::   (SortList a) -> a | Ord a
minimum []			= abort "Runtime error"
minimum [x]			= x
minimum [x : xs]
| x < (hd xs)		= minimum ([x] ++ (tl xs))
| otherwise			= minimum xs

maximum       ::   (SortList a) -> a | Ord a
maximum []			= abort "Runtime error"
maximum [x]			= x
maximum [x : xs]
| x > (hd xs)		= maximum ([x] ++ (tl xs))
| otherwise			= maximum xs

mergeSortList :: (SortList a) (SortList a) -> SortList a | Eq, Ord a
mergeSortList [] bs				= bs
mergeSortList as [] 			= as
mergeSortList [a : as] [b] 		= insertSort b ([a] ++ as)
mergeSortList [a : as] [b : bs] 
| count as < count bs			= mergeSortList (insertSort a ([b] ++ bs)) as
| otherwise						= mergeSortList (insertSort b ([a] ++ as)) bs
 
Start = mergeSortList [1,2,3] [5,4,0]

