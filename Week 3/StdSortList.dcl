definition module StdSortList

import StdClass

:: SortList a

newSortList   :: SortList a                                    // lege gesorteerde lijst
memberSort    :: a (SortList a) -> Bool       | Eq, Ord a      // is element van
insertSort    :: a (SortList a) -> SortList a | Ord a          // voeg element toe
removeFirst   :: a (SortList a) -> SortList a | Eq, Ord a      // verwijder eerste voorkomen
removeAll     :: a (SortList a) -> SortList a | Eq, Ord a      // verwijder alle voorkomens
elements      ::   (SortList a) -> [a]                         // geef alle elementen
count         ::   (SortList a) -> Int                         // aantal elementen

minimum       ::   (SortList a) -> a | Ord a                   // huidige minimum waarde
maximum       ::   (SortList a) -> a | Ord a                 // huidige maximum waarde

mergeSortList :: (SortList a) (SortList a) -> SortList a | Eq, Ord a // meng gesorteerde lijsten
